package com.kabouterlabs.ode.odepack

import java.lang

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.jodeint.codepack.CodepackLibrary._
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.config.{Config, JacobianType, LowerBandWidth, UpperBandWidth}
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import org.bridj.Pointer

/**
  * This class wraps the codepack function lsodar.
  *
  * lsodar implements root finding.
  *
  * When a root is found a special event callback is  called allowing a new set of initial
  * to the ode solution to be supplied.
  *
  *
  */


case class Lsodar(dim:Int, funcM:OdeFuncM[Double], jacM:JacobianFuncM[Double], nconstraints:Int, conM:ConstraintFuncM[Double], eventM:EventFuncM[Double],
                  params:FuncParams[Double], config:Config)
{

  private def set_lrw(dim:Int, ng:Int, jacType:JacobianType) = {
   val lrn = 20 + 16 * dim + 3 * ng
   jacType match {
     case JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)) => scala.math.max(lrn, 22 + (10 + 2*ml+mu)*dim + 3*ng)
     case _ => scala.math.max(lrn, 22 + (9 + dim) * dim + 3 * ng)
   }
  }

  private def set_liw(dim:Int) = 20 + dim

  private implicit class ev$config(config:Config) {

    def rtolDim(dim: Int): Int = config.relativeTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_,_) => 1
    }

    def atolDim(dim: Int) = config.absoluteTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_,_) => 1
    }

    def set_itol(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {
      (config.relativeTolerance.get, config.absoluteTolerance.get) match {
        case ((Some(rtolv), None), (Some(atolv), None)) => {
          itol.set(codepack_itol_e.ALL_SCALAR.value.toInt);
          rtol.set(rtolv)
          atol.set(atolv)
        }
        case ((Some(rtolv), None), (None, Some(atola))) => {
          itol.set(codepack_itol_e.ATOL_ARRAY.value.toInt);
          rtol.set(rtolv)
          atol.setDoubles(atola.slice(0, dim))
        }
        case ((None, Some(rtola)), (Some(atolv), None)) => {
          itol.set(codepack_itol_e.RTOL_ARRAY.value.toInt);
          rtol.setDoubles(rtola.slice(0, dim));
          atol.set(atolv)
        }
        case ((None, Some(rtola)), (None, Some(atola))) => {
          itol.set(codepack_itol_e.ALL_ARRAY.value.toInt)
          rtol.setDoubles(rtola.slice(0, dim))
          atol.setDoubles(atola.slice(0, dim))
        }
        case (_, _) => {
          LogIt().warn("unable to set tolerances; defaulting to error values")
          itol.set(-10)
          rtol.set(-100.0)
          atol.set(-99999.999)
        }
      }

    }

  }//end of implicit

  def this(dim:Int, funcM:OdeFuncM[Double], jac: JacobianFuncM[Double],params:FuncParams[Double], config:Config) {
    this(dim, funcM, jac, 0, ConstraintFuncM.none, EventFuncM.none, params, config)
  }


  //def lrw(dim:Int, ng:Int) = scala.math.max(20 + 16 * dim, 22 + (9 + dim)*dim) + 3 * ng

  private val jac = new dlsodar_jac_callback {
    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ml: Pointer[Integer], mu: Pointer[Integer],
                       pd: Pointer[lang.Double], nrowpd: Pointer[Integer]): Unit = {
      LogIt().trace("calling jacobian")
      val pd_r = pd.getDoubles(neq.get()*neq.get())
      jacM(neq.get(),t.get(),y.getDoubles(neq.get()),ml.get(),mu.get(),pd_r,nrowpd.get(), params) match {
        case Some(_) => pd.setDoubles(pd_r)
        case None    => LogIt().error("error in jacobian callback")
      }

    }
  }


  private val func = new dlsodar_f_callback {
    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double]): Unit = {
        val ydot_r:Array[Double] = ydot.getDoubles(neq.get())

        funcM(neq.get, t.getDouble, y.getDoubles(neq.get()), ydot_r, params) match {
          case Some(_) => ydot.setDoubles(ydot_r)
          case None    => LogIt().error("error in function call back")
        }
    }
  }

  private val gcon = new dlsodar_g_callback {
    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ng: Pointer[Integer], gout: Pointer[lang.Double]): Unit = {
      val gout_r = gout.getDoubles(neq.get())

      conM(neq.get(), t.get(), y.getDoubles(neq.get()), ng.get(), gout_r, params)  match {
        case Some(_) => gout.setDoubles(gout_r)
        case None    => LogIt().error("error in event call back")
      }

    }
  }

  private def event_call_back(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ng: Pointer[Integer], jroot:Pointer[lang.Integer]): Unit = {
    val y_r = y.getDoubles(neq.get())
    eventM(neq.get, t.get(), y_r, ng.get(),jroot.getInts(ng.get()), params) match {
      case Some(_) => y.setDoubles(y_r)
      case None    => Unit
    }
  }

  private def log_tolerance_settings(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {
    LogIt().info("tolerance settings : itol (type of setting) : " + codepack_itol_e.fromValue(itol.get()) + " atol : ")
    codepack_itol_e.fromValue(itol.get()) match {
      case codepack_itol_e.ALL_SCALAR => {
        LogIt().info("- absolute tolerance  : " + atol.getDouble())
        LogIt().info("- relative tolerance  : " + rtol.getDouble())
      }
      case codepack_itol_e.ATOL_ARRAY => {
        LogIt().info("- absolute tolerances : {" + atol.getDoubles().mkString(",") + " }" )
        LogIt().info("- relative tolerance  :  " + rtol.getDouble())
      }
      case codepack_itol_e.RTOL_ARRAY => {
        LogIt().info("- absolute tolerance  :  " + atol.getDouble())
        LogIt().info("- relative tolerances : {" + rtol.getDoubles().mkString(",") + " }")
      }
      case codepack_itol_e.ALL_ARRAY => {
        LogIt().info("- absolute tolerances : {" + atol.getDoubles().mkString(",") + " }")
        LogIt().info("- relative tolerances : {" + rtol.getDoubles().mkString(",") + " }")
      }
    }
  }

  private val func_sp: Pointer[dlsodar_f_callback] = Pointer.getPointer(func)

  private val neq:Pointer[lang.Integer]        = Pointer.pointerToInt(dim)
  private val y:Pointer[lang.Double]           = Pointer.allocateDoubles(dim)
  private val t:Pointer[lang.Double]           = Pointer.allocateDouble()

  private val tout:Pointer[lang.Double]        = Pointer.allocateDouble()
  private val itol:Pointer[lang.Integer]       = Pointer.allocateInt()
  private val rtol:Pointer[lang.Double]        = Pointer.allocateDoubles(config.rtolDim(dim))
  private val atol:Pointer[lang.Double]        = Pointer.allocateDoubles(config.atolDim(dim))
  private val itask:Pointer[lang.Integer]      = Pointer.allocateInt()
  private val istate:Pointer[lang.Integer]     = Pointer.allocateInt()
  private val iopt:Pointer[lang.Integer]       = Pointer.allocateInt()
  private val lrw:Pointer[lang.Integer]        = Pointer.pointerToInt(set_lrw(dim, nconstraints, config.jacobianType))
  private val rwork:Pointer[lang.Double]       = Pointer.allocateDoubles(lrw.get().toLong)
  private val liw:Pointer[lang.Integer]        = Pointer.pointerToInt(set_liw(dim))
  private val iwork:Pointer[lang.Integer]      = Pointer.allocateInts(liw.get().toLong)
  private val jac_sp:Pointer[dlsodar_jac_callback] = Pointer.getPointer(jac)
  private val jt:Pointer[lang.Integer]         = Pointer.allocateInt()
  private val gcon_sp:Pointer[dlsodar_g_callback] = Pointer.getPointer(gcon)

  private val ng:Pointer[lang.Integer]         = Pointer.pointerToInt(nconstraints)
  private val jroot:Pointer[lang.Integer]      = Pointer.allocateInts(ng.get().toLong)


  config.set_itol(itol, rtol, atol)
  log_tolerance_settings(itol, rtol, atol)
  iopt.set(codepack_iopt_e.NO_OPTIONAL_INPUTS.value.toInt)
  istate.set(CodepackLibrary.codepack_istate_in_e.FIRST_CALL.value.toInt)
  itask.set(CodepackLibrary.codepack_itask_e.NORMAL.value.toInt)

  (config.jacobianType, jacM)  match {

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(Some(_))) => {
      jt.set(CodepackLibrary.codepack_jac_type_e.USER_PROVIDED_BANDED.value.toInt)
      iwork.set(0, ml)
      iwork.set(1, mu)
      LogIt().info("user supplied banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(None))    => {
      jt.set(CodepackLibrary.codepack_jac_type_e.INTERNAL_BANDED.value.toInt)
      iwork.set(0, ml)
      iwork.set(1, mu)
      LogIt().info("internally generated banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }
    case (JacobianType.FullJacobian, JacobianFuncM(Some(_))) => {
      jt.set(CodepackLibrary.codepack_jac_type_e.USER_PROVIDED.value.toInt)
      LogIt().info("user supplied jacobian ")
    }

    case (JacobianType.FullJacobian, JacobianFuncM(None))    => {
      jt.set(CodepackLibrary.codepack_jac_type_e.INTERNAL.value.toInt)
      LogIt().info("generated jacobian")
    }

    case (_, JacobianFuncM(Some(_))) => {
      LogIt().warn("user provided jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian with user provided jacobian")
      jt.set(CodepackLibrary.codepack_jac_type_e.USER_PROVIDED.value.toInt)
    }
    case (_, JacobianFuncM(None))    => {
      LogIt().warn("user provided jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
      jt.set(CodepackLibrary.codepack_jac_type_e.INTERNAL.value.toInt)
    }
  }
  config.options match {
    case None => {
      iopt.set(codepack_iopt_e.NO_OPTIONAL_INPUTS.value.toInt)
    }
    case Some(options) => {
      LogIt().info(options.toString)
      iopt.set(codepack_iopt_e.OPTIONAL_INPUTS.value.toInt)
      for (initStepSize <- options.initialStepSize) yield rwork.set(4, initStepSize)

      for (v <- options.maxStepSize) yield rwork.set(5, v)
      for (v <- options.minStepSize) yield rwork.set(6, v)
      for (v <- options.maxSteps) yield iwork.set(5, v)
      for (v <- options.maxOrderNonStiff) yield iwork.set(7, v)
      for (v <- options.maxOrderStiff) yield iwork.set(8, v)

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

    LogIt().diagnostic("value of the independent variable at last method switch : " + rwork.getDoubleAtIndex(14))
    LogIt().diagnostic("number of steps taken so far                            : " + iwork.getIntAtIndex(10))
    LogIt().diagnostic("number of function evaluations so far                   : " + iwork.getIntAtIndex(11))
    LogIt().diagnostic("number of jacobian evaluations so far                   : " + iwork.getIntAtIndex(12))
    LogIt().diagnostic("method order last used                                  : " + iwork.getIntAtIndex(13))
    LogIt().diagnostic("method order to be attempted at next step               : " + iwork.getIntAtIndex(14))
    LogIt().diagnostic("length of rwork (internal work aray) actually required  : " + iwork.getIntAtIndex(16))
    LogIt().diagnostic("length of iwork (internal work aray) actually required  : " + iwork.getIntAtIndex(17))
    
    iwork.getIntAtIndex(18) match {
      case 1 => LogIt().diagnostic("method used in last successful step                     : Adams; problem not stiff")
      case 2 => LogIt().diagnostic("method used in last successful step                     : BDF; problem stiff")
      case c => LogIt().diagnostic("method used in last successful step (id not recognized) : " + c)
    }
    iwork.getIntAtIndex(19) match {
      case 1 => LogIt().diagnostic("method attempted in the next step                       : Adams; problem not stiff")
      case 2 => LogIt().diagnostic("method attempted in the next step                       : BDF; problem stiff")
      case c => LogIt().diagnostic("method attempted in the next step (id not recognized)   : " + c)
    }

    LogIt().diagnostic("-----------------------------------------------")
  }

  private def diagnostics_off() = {}

  private  def diagnostics() = config.options match {
    case Some(options) => options.diagnostics match {
      case Some(yn) => if (yn == true) diagnostics_on() else diagnostics_off()
      case _ => diagnostics_off()
    }
    case _ => diagnostics_off()
  }

  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] = HandleException {

    LogIt().info("starting with range : " + range + " initial conditions : {" + init.mkString(",") + "}")

    /*
      initialize the output stack with the initial conditions
     */

    val stack = StackDouble(dim + nconstraints,range)
    y.setDoubles(init.slice(0, neq.get()))
    t.set(range.start)
    tout.set(range.start)
    stack.append(range.start)
    for (yval <- y.getDoubles(neq.get())) stack.append(yval)
    if (ng.get() > 0) for (rindex <- jroot.getInts(ng.get())) stack.append(rindex)

    range.withRange((next:Double) => {
      tout.set(next)
      //println("===>", jt.get() + " t : " + t.get() + " tout " + tout.get() + " istate " + istate.get())
      LogIt().trace(" tout : " + tout.get() + " t " + t.get() + " istate " + istate.get())
      dlsodar(func_sp, neq, y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac_sp,jt,gcon_sp,ng,jroot)
      diagnostics()
      LogIt().trace("state get " + istate.get() + "," + codepack_istate_out_e.fromValue(istate.get()) + " " + codepack_istate_out_e.ROOT_FOUND.value )
      codepack_istate_out_e.fromValue(istate.get()) match {
        case c if c ==codepack_istate_out_e.NOTHING_DONE || c == codepack_istate_out_e.SUCCESS_DONE   => {
          val result = y.getDoubles(neq.get())
          NonValueChecker(result).hasNonValue match {
            case true => {
              LogIt().error("detected non-values in the result : " + result.mkString(",") + " stop processing")
              None
            }
            case false => {
              stack.append(tout.get())
              for (yval <- y.getDoubles(neq.get())) stack.append(yval)
              if (ng.get() > 0) for (rindex <- jroot.getInts(ng.get())) stack.append(rindex)
              Some(t.get())
            }
          }
          
        }
        case c if c == codepack_istate_out_e.ROOT_FOUND => {
          //        val gout = new Array[Double](ng.get())
          //        conM(neq.get(), t.get(), y.getDoubles(neq.get()), ng.get(), gout, params)
          //
          //        for ((_, index) <- jroot.getInts(ng.get()).zipWithIndex.filter(_._1 == 1))  {
          //          println("root found at index ", index, gout(index) )
          //        }
          LogIt().trace("root found : " + jroot.getInts(ng.get()).mkString(","))
          event_call_back(neq,t,y,ng,jroot)
          istate.set(CodepackLibrary.codepack_istate_in_e.FIRST_CALL.value.toInt)
          stack.append(t.get())
          for (yval <- y.getDoubles(neq.get())) stack.append(yval)
          if (ng.get() > 0) for (rindex <- jroot.getInts(ng.get())) stack.append(rindex)
          Some(t.get())
        }
        case c if c == codepack_istate_out_e.MAX_STEPS_EXCEEDED =>  {
          LogIt().error("excessive amount of work done + istate : " + c.value())
          None
        }
        case c if c == codepack_istate_out_e.TO_MUCH_ACCURACY =>  {
          LogIt().error("excess accuracy requested + istate : " + c.value())
          LogIt().diagnostic("tolerance scale factor                                  : " + rwork.getDoubleAtIndex(13))
          None
        }

        case c if c == codepack_istate_out_e.ILLEGAL_INPUT =>  {
          LogIt().error("illegal input + istate : " + c.value())
          LogIt().diagnostic("tolerance scale factor                                  : " + rwork.getDoubleAtIndex(13))
          None
        }
        case c if c == codepack_istate_out_e.ERROR_TEST_FAILURES =>  {
          LogIt().error("repeated error test failures + istate : " + c.value())
          LogIt().diagnostic("index of component with the largest weighted local error vector :" + iwork.getIntAtIndex(15))
          None
        }
        case c if c == codepack_istate_out_e.CONVERGENCE_FAILURES =>  {
          LogIt().error("repeated convergence failures + istate : " + c.value())
          LogIt().diagnostic("index of component with the largest weighted local error vector :" + iwork.getIntAtIndex(15))
          None
        }
        case c if c == codepack_istate_out_e.ZERO_ERR_TOLERANCE =>  {
          LogIt().error("error weight zero + istate : " + c.value())
          None
        }
        case c if c == codepack_istate_out_e.TOO_SMALL_WORK_ARRAY =>  {
          LogIt().error("work space insufficient + istate : " + c.value())
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
