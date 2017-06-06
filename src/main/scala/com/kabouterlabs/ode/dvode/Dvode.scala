package com.kabouterlabs.ode.dvode

import java.lang

import com.kabouterlabs.jodeint.cdvode.CdvodeLibrary
import com.kabouterlabs.jodeint.cdvode.CdvodeLibrary._
import com.kabouterlabs.jodeint.cdvode.CdvodeLibrary.{cdvode_itol_e, dvode_f_callback, dvode_jac_callback}
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import org.bridj.Pointer


/** Dvode interface
  *
  *  A variable coefficient ODE solver. See [[http://www.netlib.no/netlib/ode/vode.f vode]]
  *
  * @note : for more information on the underlying algorithm follow this link and look for dvode [[https://computation.llnl.gov/casc/odepack/]].
  *
  * @constructor  Dvode Ode Solver instance.
  * @param  dim    : Dimension of the ODE
  * @param  funcM  : ODE solver call back
  * @param  jacM   : Jacobian
  * @param  params : Parameters
  * @param  config : Configuration parameters
  *
  */
case class Dvode(dim:Int, funcM:OdeFuncM[Double], jacM:JacobianFuncM[Double], params:FuncParams[Double], config:Config)
{
  LogIt().info("configuration : " + config)
  private val logger = LogIt()

  private def log_tolerance_settings(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {
    LogIt().info("tolerance settings : itol (type of setting) : " + cdvode_itol_e.fromValue(itol.get()) + " atol : ")
    cdvode_itol_e.fromValue(itol.get()) match {
      case cdvode_itol_e.ALL_SCALAR => {
        LogIt().info("- absolute tolerance  : " + atol.getDouble())
        LogIt().info("- relative tolerance  : " + rtol.getDouble())
      }
      case cdvode_itol_e.ATOL_ARRAY => {
        LogIt().info("- absolute tolerances : {" + atol.getDoubles().mkString(",") + " }" )
        LogIt().info("- relative tolerance  :  " + rtol.getDouble())
      }
      case cdvode_itol_e.RTOL_ARRAY => {
        LogIt().info("- absolute tolerance  :  " + atol.getDouble())
        LogIt().info("- relative tolerances : {" + rtol.getDoubles().mkString(",") + " }")
      }
      case cdvode_itol_e.ALL_ARRAY => {
        LogIt().info("- absolute tolerances : {" + atol.getDoubles().mkString(",") + " }")
        LogIt().info("- relative tolerances : {" + rtol.getDoubles().mkString(",") + " }")
      }
    }
  }

  private implicit class ev$config(config:Config) {

    def rtolDim(dim: Int): Int = config.relativeTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_,_)  => 1
    }

    def atolDim(dim: Int) = config.absoluteTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_,_)=> 1
    }

    def set_itol(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {
      (config.relativeTolerance.get, config.absoluteTolerance.get) match {
        case ((Some(rtolv), None), (Some(atolv), None)) => {
          itol.set(cdvode_itol_e.ALL_SCALAR.value.toInt);
          rtol.set(rtolv);
          atol.set(atolv)
        }
        case ((Some(rtolv), None), (None, Some(atola))) => {
          itol.set(cdvode_itol_e.ATOL_ARRAY.value.toInt);
          rtol.set(rtolv);
          atol.setDoubles(atola.slice(0,dim))
        }
        case ((None, Some(rtola)), (Some(atolv), None)) => {
          itol.set(cdvode_itol_e.RTOL_ARRAY.value.toInt);
          rtol.setDoubles(rtola.slice(0,dim));
          atol.set(atolv)
        }
        case ((None, Some(rtola)), (None, Some(atola))) => {
          itol.set(cdvode_itol_e.ALL_ARRAY.value.toInt)
          rtol.setDoubles(rtola.slice(0,dim))
          atol.setDoubles(atola.slice(0,dim))
        }
        case (_,_) => {
          itol.set(-10)
          rtol.set(-100.0)
          atol.set(-99999.999)
        }
      }
    }

    private val meth = config.method match {
      case Methods.ADAMS => 1
      case Methods.BDF   => 2
      case _             => 2
    }

    def set_mf (mf:Pointer[lang.Integer], miter:Int) = {
      mf.set(10*meth + miter)
    }

  }//end of implicits

  private def lrw(dim:Int, jt:JacobianType, mf:Int) = {
    (mf, jt) match {
      case (10, JacobianType.PreferNoJacobian) => 20 + 16 * dim
      case (11, JacobianType.FullJacobian)     => 22 + 16 * dim + 2*dim*dim
      case (12, JacobianType.FullJacobian)     => 22 + 16 * dim + 2*dim*dim
      case (13, JacobianType.DiagonalJacobian) => 22 + 17 * dim
      case (14, JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu))) => 22 + 18*dim + (3*ml+2*mu)*dim
      case (15, JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu))) => 22 + 18*dim + (3*ml+2*mu)*dim

      case (20, JacobianType.PreferNoJacobian) => 20 +  9 * dim
      case (21, JacobianType.FullJacobian)     => 22 +  9 * dim + 2*dim*dim
      case (22, JacobianType.FullJacobian)     => 22 +  9 * dim + 2*dim*dim
      case (23, JacobianType.DiagonalJacobian) => 22 + 10 * dim
      case (24, JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu))) => 22 + 11*dim + (3*ml+2*mu)*dim
      case (25, JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu))) => 22 + 10*dim + (3*ml+2*mu)*dim

      case (11,_) => {
        LogIt().warn("incompatible method flag " + mf + " and jacobian type " + jt + " defaulting to lrw for full jacobian")
        22 + 16 * dim + 2*dim*dim
      }
      case (12,_) => {
        LogIt().warn("incompatible method flag " + mf + " and jacobian type " + jt + " defaulting to lrw for full jacobian")
        22 + 16 * dim + 2*dim*dim
      }
      case (21,_) => {
        LogIt().warn("incompatible method flag " + mf + " and jacobian type " + jt + " defaulting to lrw for full jacobian")
        22 + 9 * dim + 2*dim*dim
      }
      case (22,_) => {
        LogIt().warn("incompatible method flag " + mf + " and jacobian type " + jt + " defaulting to lrw for full jacobian")
        22 + 9 * dim + 2*dim*dim
      }
      case (_,_) => {
        LogIt().error("incomapitble metod flag " + mf + " and jacobian type " + jt +" defaulting lrw to -1 which will cause failure")
        -1
      }
    }

  }
  private def liw(dim:Int, jt:JacobianType) = {
    jt match {
      case JacobianType.PreferNoJacobian => 30
      case JacobianType.DiagonalJacobian => 30
      case  _ => 30 + dim
    }
  }

  private val jac = new dvode_jac_callback  {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ml: Pointer[Integer], mu: Pointer[Integer], pd: Pointer[lang.Double], nrowpd: Pointer[Integer], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit ={


      logger.trace("calling jacobian")
      val pd_r = pd.getDoubles(neq.get()*neq.get())
      jacM(neq.get(),t.get(),y.getDoubles(neq.get()),ml.get(),mu.get(),pd_r,nrowpd.get(), params) match {
        case Some(_) => pd.setDoubles(pd_r)
        case None    => logger.error("error in jacobian")
      }

    }
  }


  private val func = new dvode_f_callback  {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {

        val ydot_r:Array[Double] = ydot.getDoubles(neq.get())
        logger.trace("calling function")
        funcM(neq.get, t.getDouble,y.getDoubles(neq.get()), ydot_r, params) match {
          case Some(_) => ydot.setDoubles(ydot_r)
          case None    => logger.error("error in the function call back")
        }
      }
    }


  private val func_sp: Pointer[dvode_f_callback] = Pointer.getPointer(func)

  private val neq:Pointer[lang.Integer]        = Pointer.pointerToInt(dim)
  private val y:Pointer[lang.Double]           = Pointer.allocateDoubles(dim)
  private val t:Pointer[lang.Double]           = Pointer.allocateDouble()
  //private val ydot:Pointer[lang.Double]        = Pointer.allocateDoubles(dim)
  private val tout:Pointer[lang.Double]        = Pointer.allocateDouble()
  private val itol:Pointer[lang.Integer]       = Pointer.allocateInt()
  private val rtol:Pointer[lang.Double]        = Pointer.allocateDoubles(config.rtolDim(dim))
  private val atol:Pointer[lang.Double]        = Pointer.allocateDoubles(config.atolDim(dim))
  private val itask:Pointer[lang.Integer]      = Pointer.allocateInt()
  private val istate:Pointer[lang.Integer]     = Pointer.allocateInt()
  private val iopt:Pointer[lang.Integer]       = Pointer.allocateInt()
  private val lrw:Pointer[lang.Integer]        = Pointer.allocateInt()

  private val liw:Pointer[lang.Integer]        = Pointer.pointerToInt(liw(dim, config.jacobianType))
  private val iwork:Pointer[lang.Integer]      = Pointer.allocateInts(liw.get().toLong)
  private val jac_sp:Pointer[dvode_jac_callback] = Pointer.getPointer(jac)
  private val jt:Pointer[lang.Integer]         = Pointer.allocateInt()
  private val mf:Pointer[lang.Integer]         = Pointer.allocateInt()
  ////



  config.set_itol(itol,rtol,atol)
  log_tolerance_settings(itol, rtol, atol)
  
  iopt.set(cdvode_iopt_e.NO_OPTIONAL_INPUTS.value.toInt)
  istate.set(cdvode_istate_in_e.FIRST_CALL.value.toInt)
  /////

  itask.set(code_itask_e.NORMAL.value.toInt)
  (config.jacobianType, jacM)  match {
    case (JacobianType.PreferNoJacobian,_) => {
      jt.set(cdvode_iteration_method_e.NO_JACOBIAN.value.toInt)
      LogIt().info("don't use jacobian ")
    }
    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(Some(_))) => {
      jt.set(cdvode_iteration_method_e.EXTERNAL_BAND_JACOBIAN.value.toInt)
      iwork.set(0, ml)
      iwork.set(1, mu)
      LogIt().info("user supplied banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(None))    => {
      jt.set(cdvode_iteration_method_e.INTERNAL_BAND_JACOBIAN.value.toInt)
      iwork.set(0, ml)
      iwork.set(1, mu)
      LogIt().info("internally generated banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.FullJacobian, JacobianFuncM(Some(_))) => {
      jt.set(cdvode_iteration_method_e.EXTERNAL_FULL_JACOBIAN.value.toInt)
      LogIt().info("user supplied jacobian ")
    }
    case (JacobianType.FullJacobian, JacobianFuncM(None))    => {
      jt.set(cdvode_iteration_method_e.INTERNAL_FULL_JACOBIAN.value.toInt)
      LogIt().info("generated jacobian")
    }
    case (JacobianType.DiagonalJacobian, JacobianFuncM(None))    => {
      jt.set(cdvode_iteration_method_e.INTERNAL_DIAG_JACOBIAN.value.toInt)
      LogIt().info("internal diagonal jacobian")
    }
    case (_, JacobianFuncM(Some(_))) => {
      LogIt().warn("user provided jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
      jt.set(cdvode_iteration_method_e.EXTERNAL_FULL_JACOBIAN.value.toInt)
    }
    case (_, JacobianFuncM(None))    => {
      LogIt().warn("user provided jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
      jt.set(cdvode_iteration_method_e.INTERNAL_FULL_JACOBIAN.value.toInt)
    }
  }

  config.set_mf(mf, jt.get())
  lrw.set(lrw(dim,config.jacobianType,mf.get()))
  private val rwork:Pointer[lang.Double]       = Pointer.allocateDoubles(lrw.get().toLong)
  LogIt().info(" lrw : " + lrw.get() + " liw : " + liw.get())

  config.options match {
    case None => {
      iopt.set(cdvode_iopt_e.NO_OPTIONAL_INPUTS.value.toInt)
    }
    case Some(options) => {
      LogIt().info(options.toString)
      iopt.set(cdvode_iopt_e.OPTIONAL_INPUTS.value.toInt)

      for (initStepSize <- options.initialStepSize) yield rwork.set(4, initStepSize)
      for (v <- options.maxStepSize) yield rwork.set(5, v)
      for (v <- options.minStepSize) yield rwork.set(6, v)

      for (v <- options.maxOrder) yield iwork.set(4, v)
      for (v <- options.maxSteps) yield iwork.set(5, v)

      for (rwa <- options.rwork) yield {
        for (i <- Range(0, math.min(lrw.get(), rwa.length))) {
          rwork.set(i, rwa(i))
        }
      }

      for (iwa <- options.iwork) yield {
        for (i <- Range(0, math.min(liw.get(), iwa.length))) {
          iwork.set(i, iwa(i))
        }
      }
    }
  }

  private def diagnostics_on(): Unit = {
    LogIt().diagnostic("step size last used sucessfully                         : " + rwork.getDoubleAtIndex(10))
    LogIt().diagnostic("step size to be attempted                               : " + rwork.getDoubleAtIndex(11))
    LogIt().diagnostic("current value of the independent variable               : " + rwork.getDoubleAtIndex(12))

    LogIt().diagnostic("number of steps taken so far                            : " + iwork.getIntAtIndex(10))
    LogIt().diagnostic("number of function evaluations so far                   : " + iwork.getIntAtIndex(11))
    LogIt().diagnostic("number of jacobian evaluations so far                   : " + iwork.getIntAtIndex(12))
    LogIt().diagnostic("method order last used                                  : " + iwork.getIntAtIndex(13))
    LogIt().diagnostic("method order to be attempted at next step               : " + iwork.getIntAtIndex(14))

    LogIt().diagnostic("length of rwork (internal work aray) actually required  : " + iwork.getIntAtIndex(16))
    LogIt().diagnostic("length of iwork (internal work aray) actually required  : " + iwork.getIntAtIndex(17))

    LogIt().diagnostic("number of matrix LU decompositions so far               : " + iwork.getIntAtIndex(18))
    LogIt().diagnostic("number of non-linear newton iterations so far           : " + iwork.getIntAtIndex(19))
    LogIt().diagnostic("number of convergence failures of the non-linear solver : " + iwork.getIntAtIndex(20))
    LogIt().diagnostic("number of error test failures of the integrator so far  : " + iwork.getIntAtIndex(21))
    LogIt().diagnostic("-----------------------------------------------")
  }

  private def diagnostics_off() = {}

  private def diagnostics = config.options match {
    case Some(options) => options.diagnostics match {
      case Some(yn) => if (yn == true) diagnostics_on else diagnostics_off
      case _ => diagnostics_off
    }
    case _ => diagnostics_off
  }

/** Solves the ODE on a 1D grid (i.e. line) for a set of initial conditions
  *
  * @param range : Range of the independent variable. Cannot be infinite.
  * @param init  : Initial conditions in the same order as the variables returned by the class back function
  * @return  a stack containing the solution on each grid point
  *
  *
  */

  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] = HandleException {
    LogIt().info("starting with range : " + range + " initial conditions : {" + init.mkString(",") + "}")
    val stack = StackDouble(dim,range)
    y.setDoubles(init.slice(0, neq.get()))
    t.set(range.start)
    tout.set(range.start)
    stack.append(range.start)
    for (yval <- y.getDoubles(neq.get())) stack.append(yval)

    range.withRange((next:Double) => {
      tout.set(next)
      //logger.info("===>", jt.get())
      logger.trace(" tout : " + tout.get() + " t " + t.get() + " istate " + istate.get())

      CdvodeLibrary.dvode(func_sp, neq, y, t, tout, itol, rtol, atol, itask, istate, iopt, rwork, lrw, iwork, liw, jac_sp, mf, null, null)
      diagnostics
      cdvode_istate_out_e.fromValue(istate.get()) match {
        case c if c == cdvode_istate_out_e.NOTHING_DONE || c == cdvode_istate_out_e.SUCCESS_DONE => {
          val result = y.getDoubles(neq.get())
          NonValueChecker(result).hasNonValue match {
            case true => {
              LogIt().error("detected non-values in the result : " + result.mkString(",") + " stop processing")
              None
            }
            case false => {
              stack.append(tout.get())
              for (yval <- y.getDoubles(neq.get())) stack.append(yval)
              Some(t.get())
            }
          }
        }
        case c if c == cdvode_istate_out_e.MXSTEPS_EXCEEDED => {
          LogIt().error("excessive amount of work done + istate : " + c.value())
          None
        }
        case c if c == cdvode_istate_out_e.TO_MUCH_ACCURACY => {
          LogIt().error("excess accuracy requested + istate : " + c.value())
          LogIt().diagnostic("tolerance scale factor        : " + rwork.getDoubleAtIndex(13))
          None
        }

        case c if c == cdvode_istate_out_e.ILLEGAL_INPUT => {
          LogIt().error("illegal input + istate : " + c.value())
          LogIt().diagnostic("tolerance scale factor        : " + rwork.getDoubleAtIndex(13))
          None
        }
        case c if c == cdvode_istate_out_e.ERROR_TEST_FAILURES => {
          LogIt().error("repeated error test failures + istate : " + c.value())
          LogIt().diagnostic("index of component with the largest weighted local error vector :" + iwork.getIntAtIndex(15))
          None
        }
        case c if c == cdvode_istate_out_e.CONVERGENCE_FAILURES => {
          LogIt().error("repeated convergence failures + istate : " + c.value())
          LogIt().diagnostic("index of component with the largest weighted local error vector :" + iwork.getIntAtIndex(15))
          None
        }
        case c if c == cdvode_istate_out_e.ZERO_ERR_TOLERANCE => {
          LogIt().error("error weight zero + istate : " + c.value())
          None
        }

      }

    }) match {
      case Some(_) =>   Some(stack)
      case None => {
        LogIt().warn("error occured; no results returned")
        None
      }
    }

  }

}
