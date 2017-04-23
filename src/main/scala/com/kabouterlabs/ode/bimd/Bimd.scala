package com.kabouterlabs.ode.bimd

import com.kabouterlabs.jodeint.cbimd.CbimdLibrary
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode._
import java.lang

import com.kabouterlabs.jodeint.cbimd.CbimdLibrary._
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import org.bridj.Pointer

/**
  * Created by fons on 3/10/17.
  */
case class Bimd(dim:Int, funcM:OdeFuncM[Double], jacM:JacobianFuncM[Double], massM:MassMatrixFuncM[Double],
                daoVar: DaeIndexVariables, params:FuncParams[Double], config:Config)
{

  LogIt().info("configuration : " + config)

  private val logger = LogIt()

  private def log_tolerance_settings(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {
    LogIt().info("tolerance settings : itol (type of setting) : " +   bimd_itol_e.fromValue(itol.get()) + " atol : ")
    bimd_itol_e.fromValue(itol.get()) match {
      case   bimd_itol_e.ALL_SCALAR => {
        LogIt().info("- absolute tolerance  : " + atol.getDouble())
        LogIt().info("- relative tolerance  : " + rtol.getDouble())
      }

      case   bimd_itol_e.ALL_ARRAY => {
        LogIt().info("- absolute tolerances : {" + atol.getDoubles().mkString(",") + " }")
        LogIt().info("- relative tolerances : {" + rtol.getDoubles().mkString(",") + " }")
      }
      case _ => {
        LogIt().warn("unable to identify the tolerance types")
      }
    }
  }

  implicit class ev$config(config: Config) {

    def rtolDim(dim: Int): Int = config.relativeTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_, _) => 1
    }

    def atolDim(dim: Int) = config.absoluteTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_, _) => 1
    }

    def set_itol(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {

      val rtolv = config.relativeTolerance.get match {
        case (Some(v), None) => v
        case (None, Some(arr)) => {
          LogIt().warn("bimd does not support arrays of relative tolerances; using the average value ")
          (0.0 /: arr){_ + _}/arr.length
        }
        case (_,_) => {
          LogIt().error("cannot determine relative tolerance; returning out-of-range value ")
          -99999999.0
        }
      }

      config.absoluteTolerance.get match {
        case  (Some(atolv), None) => {
          itol.set(bimd_itol_e.ALL_SCALAR.value.toInt)
          rtol.set(rtolv)
          atol.set(atolv)
        }

        case (None, Some(atola)) => {
          itol.set(bimd_itol_e.ALL_ARRAY.value.toInt)
          rtol.set(rtolv)
          atol.setDoubles(atola.slice(0, dim))
        }
          
        case (_,_) => {
          LogIt().error("cannot determine tolerances; returning out-o-range value ")
          itol.set(-99999)
          rtol.set(-9999999.0)
          atol.set(-9999999.0)
        }
      }
    }

  }

  //end of implicit

  private def lrw(dim: Int, jt: JacobianType, mt:MassMatrixType) = {
    val (ldjac, ldlu) = jt match {
      case JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)) => (ml + mu + 1, 2 * ml + mu + 1)
      case _ => (dim, dim)
    }
    val lmas = mt match {
      case MassMatrixType.IdentityMatrix => 1
      case MassMatrixType.FullMassMatrix => dim
      case MassMatrixType.BandedMassMatrix(LowerBandWidth(mlmas),UpperBandWidth(mumas)) => mlmas + mumas + 1
    }
    val ordmax = 12
    val kmax   = math.max(3, ordmax - 2)

    14 + kmax + 9*dim + 5 * kmax * dim + dim * (ldjac+ldlu+ lmas)

  }

  private def liw(dim: Int) = dim + 40 + 5

  private val func = new bimd_fcn_callback {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double],
                       ierr: Pointer[lang.Integer], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {

      val ydot_r: Array[Double] = ydot.getDoubles(neq.get())
      logger.trace("calling function")
      funcM(neq.get, t.getDouble, y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => {
          ierr.set(0)
          ydot.setDoubles(ydot_r)
        }
        case None => {
          ierr.set(-1)
          logger.error("error in the function call back")
        }
      }
    }
  }



  private val func_sp: Pointer[bimd_fcn_callback] = Pointer.getPointer(func)

  private val neq: Pointer[lang.Integer] = Pointer.pointerToInt(dim)
  private val y: Pointer[lang.Double] = Pointer.allocateDoubles(dim)
  private val x: Pointer[lang.Double] = Pointer.allocateDouble()
  private val xend: Pointer[lang.Double] = Pointer.allocateDouble()
  private val h: Pointer[lang.Double] = Pointer.allocateDouble()
  private val rtol: Pointer[lang.Double] = Pointer.allocateDoubles(config.rtolDim(dim))
  private val atol: Pointer[lang.Double] = Pointer.allocateDoubles(config.atolDim(dim))
  private val itol: Pointer[lang.Integer] = Pointer.allocateInt()

  private val ijac: Pointer[lang.Integer] = Pointer.allocateInt()
  private val mljac: Pointer[lang.Integer] = Pointer.allocateInt()
  private val mujac: Pointer[lang.Integer] = Pointer.allocateInt()

  private val imas: Pointer[lang.Integer] = Pointer.allocateInt()
  private val mlmas: Pointer[lang.Integer] = Pointer.allocateInt()
  private val mumas: Pointer[lang.Integer] = Pointer.allocateInt()
  //solout
  private val iout: Pointer[lang.Integer] = Pointer.pointerToInt(bimd_iout_e.NEVER_CALLED.value.toInt)

  private val lwork: Pointer[lang.Integer] = Pointer.pointerToInt(lrw(dim, config.jacobianType, config.mass))
  private val work: Pointer[lang.Double] = Pointer.allocateDoubles(lwork.get().toLong)

  private val liwork: Pointer[lang.Integer] = Pointer.pointerToInt(liw(dim))
  private val iwork: Pointer[lang.Integer] = Pointer.allocateInts(liwork.get().toLong)

  private val idid: Pointer[lang.Integer] = Pointer.allocateInt()


  (config.jacobianType, jacM) match {

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(Some(_))) => {
      ijac.set(bimd_jac_type_e.USER_PROVIDED.value.toInt)
      mljac.set(ml)
      mujac.set(mu)
      LogIt().info("user supplied banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(None)) => {
      ijac.set(bimd_jac_type_e.NUMERICAL_JACOBIAN.value.toInt)
      LogIt().info("internally generated banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.FullJacobian, JacobianFuncM(Some(_))) => {
      ijac.set(bimd_jac_type_e.USER_PROVIDED.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().info("user supplied full jacobian ")
    }
    case (JacobianType.FullJacobian, JacobianFuncM(None)) => {
      ijac.set(bimd_jac_type_e.NUMERICAL_JACOBIAN.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().info("generated full jacobian")
    }

    case (_, JacobianFuncM(Some(_))) => {
      ijac.set(bimd_jac_type_e.USER_PROVIDED.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().warn("user provided jacobian with jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
    }
    case (_, JacobianFuncM(None)) => {
      ijac.set(bimd_jac_type_e.NUMERICAL_JACOBIAN.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().warn("internally generated jacobian; user provided jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
    }
  }

  private val jac = new bimd_jac_callback {

/*
    override def apply(neq: Pointer[Integer], x: Pointer[lang.Double], y: Pointer[lang.Double], pd: Pointer[lang.Double], nrowpd: Pointer[Integer],
                       ierr:Pointer[lang.Integer], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {
      logger.trace("calling jacobian")
      val pd_r = pd.getDoubles(neq.get() * neq.get())
      jacM(neq.get(), x.get(), y.getDoubles(neq.get()), mljac.get(), mujac.get(), pd_r, nrowpd.get(), params) match {
        case Some(_) => {
          ierr.set(0)
          pd.setDoubles(pd_r)
        }
        case None => {
          ierr.set(-1)
          logger.error("error in jacobian")
        }
      }

    }
*/
  }

  private val jac_sp: Pointer[bimd_jac_callback] = Pointer.getPointer(jac)
  
//
  (massM, config.mass) match {
    case (_, MassMatrixType.IdentityMatrix) => imas.set(bimd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)
    case (MassMatrixFuncM(None), _) =>  imas.set(bimd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)

    case (MassMatrixFuncM(Some(_)), MassMatrixType.FullMassMatrix) => {
      imas.set(bimd_mass_matrix_e.MASS_USER_PROVIDED.value.toInt)
      mlmas.set(dim)
      mumas.set(dim)
    }

    case (MassMatrixFuncM(Some(_)), MassMatrixType.BandedMassMatrix(LowerBandWidth(ml), UpperBandWidth(mu))) => {
      imas.set(bimd_mass_matrix_e.MASS_USER_PROVIDED.value.toInt)
      mlmas.set(ml)
      mumas.set(mu)
    }

    case (_,_) => {
      LogIt().warn("provided mas matrix type " + config.mass + " and mass function " + massM + " but resolving to identiy matrix/regular ode")
      imas.set(bimd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)
    }
  }
  //
  private val mass = new bimd_mas_callback {

    override def apply(neq: Pointer[Integer], am: Pointer[lang.Double], lmas: Pointer[Integer],
                       ierr:Pointer[lang.Integer], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {
      val am_r :Array[Double] = am.getDoubles(neq.get()*lmas.get())
      massM(neq.get(), am_r, lmas.get(), mlmas.get(), mumas.get(), params) match {
        case Some(_) => {
          ierr.set(0)
          am.setDoubles(am_r)
        }
        case None => {
          ierr.set(-1)
          logger.error("error in the mass matrix call back")
        }
      }
    }

  }
  private val mass_sp: Pointer[bimd_mas_callback] = Pointer.getPointer(mass)
  daoVar match {
    case DaeIndexVariables(None,None,None) => {
      LogIt().warn("no index variables set so assuming not a dao; defaulting to ODE ")
      imas.set(bimd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)
      mlmas.set(dim)
      mumas.set(dim)
    }
    case DaeIndexVariables(Some(i1),None,None) => iwork.set(8,i1)
    case DaeIndexVariables(Some(i1),Some(i2),None) => {
      iwork.set(8, i1)
      iwork.set(9, i2)
    }
    case DaeIndexVariables(Some(i1),Some(i2),Some(i3)) => {
      iwork.set(8, i1)
      iwork.set(9, i2)
      iwork.set(10, i3)
    }
  }

  //
  LogIt().info(config.options.toString())


  config.options match {
    case None => {
       LogIt().info("no configuration options provided")
    }
    case Some(options) => {
      LogIt().info(options.toString)


      for (initStepSize <- options.initialStepSize) yield h.set(initStepSize)
      for (v <- options.maxStepSize) yield work.set(1, v)

      for (v <- options.maxSteps)    yield iwork.set(0, v)
      for (v <- options.minOrder)    yield iwork.set(1, v)
      for (v <- options.maxOrder)    yield iwork.set(2, v)


      for (rwa <- options.rwork) yield {
        for (i <- Range(0, math.min(lwork.get(), rwa.length))) {
          work.set(i, rwa(i))
        }
      }

      for (iwa <- options.iwork) yield {
        for (i <- Range(0, math.min(liwork.get(), iwa.length))) {
          iwork.set(i, iwa(i))
        }
      }
    }
  }

  def diagnostics_on(): Unit = {

    LogIt().diagnostic("number of function evaluations so far                   : " + iwork.getIntAtIndex(11))
    LogIt().diagnostic("number of jacobian evaluations so far                   : " + iwork.getIntAtIndex(12))
    LogIt().diagnostic("number of matrix LU decompositions so far               : " + iwork.getIntAtIndex(13))
    LogIt().diagnostic("number of linear systems solved                         : " + iwork.getIntAtIndex(14))

    for (index <- Range(15,20)){
      LogIt().diagnostic("number of blended iterations per method                  (index :  " + index + "): " + iwork.getIntAtIndex(index))
    }

    for (index <- Range(20,25)){
      LogIt().diagnostic("number of steps per method                               (index :  " + index + "): " + iwork.getIntAtIndex(index))
    }

    for (index <- Range(25,30)){
      LogIt().diagnostic("number of accepted steps per method                      (index :  " + index + "): " + iwork.getIntAtIndex(index))
    }

    for (index <- Range(30,35)){
      LogIt().diagnostic("number of refused steps per method; error test           (index :  " + index + "): " + iwork.getIntAtIndex(index))
    }

    for (index <- Range(35,40)){
      LogIt().diagnostic("number of refused steps per method; newton's convergence (index :  " + index + "): " + iwork.getIntAtIndex(index))
    }
    
    LogIt().diagnostic("-----------------------------------------------")
  }

  def diagnostics_off() = {}

  def diagnostics = config.options match {
    case Some(options) => options.diagnostics match {
      case Some(yn) => if (yn == true) diagnostics_on else diagnostics_off
      case _ => diagnostics_off
    }
    case _ => diagnostics_off
  }


  config.set_itol(itol,rtol,atol)

  log_tolerance_settings(itol,rtol,atol)

  LogIt().info(" lrw : " + lwork.get() + " liw : " + liwork.get())

  def run(range: LineRangeT[Double], init: Array[Double]): Option[StackT] = HandleException {
    LogIt().info("starting with range : " + range + " initial conditions : {" + init.mkString(",") + "}")
    val stack = StackDouble(dim, range)
    y.setDoubles(init.slice(0, neq.get()))
    x.set(range.start)
    xend.set(range.start)
    stack.append(range.start)
    for (yval <- y.getDoubles(neq.get())) stack.append(yval)

    range.withRange((next: Double) => {
      xend.set(next)
      logger.trace(" xend : " + xend.get() + " x " + x.get() + " idid " + idid.get())
      CbimdLibrary.bimd(neq, func_sp, x,xend, y, h, rtol, atol, itol, jac_sp, ijac, mljac, mujac, mass_sp, imas,
        mlmas, mumas, null, iout, work, lwork, iwork, liwork, null, null, idid)
      diagnostics
      //LogIt().info("=====> idid  " + idid.get())
      bimd_idid_e.fromValue(idid.get()) match {
        case c if c == bimd_idid_e.SUCCESS => {
          val result = y.getDoubles(neq.get())
          NonValueChecker(result).hasNonValue match {
            case true => {
              LogIt().error("detected non-values in the result : " + result.mkString(",") + " stop processing")
              None
            }
            case false => {
              stack.append(xend.get())
              for (yval <- y.getDoubles(neq.get())) stack.append(yval)
              Some(x.get())
            }
          }
        }
        case c if c == bimd_idid_e.INPUT_INCONSISTENT => {
          LogIt().error("inconsitent input ; idid : " + c.value())
          None
        }
        case c if c == bimd_idid_e.NMAX_TOO_SMALL => {
          LogIt().error("larger number of iterations (namx) needed ; idid : " + c.value())
          None
        }
        case c if c == bimd_idid_e.STEP_TOO_SMALL => {
          LogIt().error("step size too small ; idid : " + c.value())
          None
        }
        case c if c == bimd_idid_e.SINGULAR_MATRIX => {
          LogIt().error("matrix is repeatedly singular ; idid : " + c.value())
          None
        }
        case c if c == bimd_idid_e.FCN_JAC_ERROR_RETURNED => {
          LogIt().error(" call back returned error ; idid : " + c.value())
          None
        }
      }

    }) match {
      case Some(_) => Some(stack)
      case None => {
        LogIt().warn("an error occured")
        Some(stack)
      }
    }
  }

}
