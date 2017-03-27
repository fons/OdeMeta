package com.kabouterlabs.ode.gamd


import com.kabouterlabs.jodeint.cgamd.CgamdLibrary
import com.kabouterlabs.jodeint.cgamd.CgamdLibrary._

import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode._


import java.lang


import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt}
import org.bridj.Pointer

class Gamd(dim:Int, funcM:OdeFuncM[Double], jacM:JacobianFuncM[Double], massM:MassMatrixFuncM[Double],
           daoVar:DaeIndexVariables, params:FuncParams[Double], config:Config)
{

  private val logger = LogIt()

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
      (config.relativeTolerance.get, config.absoluteTolerance.get) match {
        case ((Some(rtolv), None), (Some(atolv), None)) => {
          itol.set(gamd_itol_e.ALL_SCALAR.value.toInt);
          rtol.set(rtolv);
          atol.set(atolv)
        }

        case ((None, Some(rtola)), (None, Some(atola))) => {
          itol.set(gamd_itol_e.ALL_ARRAY.value.toInt)
          rtol.setDoubles(rtola.slice(0, dim))
          atol.setDoubles(atola.slice(0, dim))
        }
        case (_, _) => {
          LogIt().warn("unable to assign the right error option; neither all scalar or all aarray")
          itol.set(-10)
          rtol.set(-100.0)
          atol.set(-99999.999)
        }
      }
    }


  }

  //end of implicit

  private def lrw() = 21

  private def liw(dim: Int) = 27

  private val func = new gamd_fcn_callback {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double],
                       ierr: Pointer[lang.Integer], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {

      val ydot_r: Array[Double] = ydot.getDoubles(neq.get())
      logger.trace("calling function")
      funcM(neq.get, t.getDouble, y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => logger.error("error in the function call back")
      }
    }
  }



  private val func_sp: Pointer[gamd_fcn_callback] = Pointer.getPointer(func)

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
  private val iout: Pointer[lang.Integer] = Pointer.pointerToInt(gamd_iout_e.NEVER_CALLED.value.toInt)

  private val lwork: Pointer[lang.Integer] = Pointer.pointerToInt(lrw())
  private val work: Pointer[lang.Double] = Pointer.allocateDoubles(lwork.get().toLong)

  private val liwork: Pointer[lang.Integer] = Pointer.pointerToInt(liw(dim))
  private val iwork: Pointer[lang.Integer] = Pointer.allocateInts(liwork.get().toLong)

  private val idid: Pointer[lang.Integer] = Pointer.allocateInt()


  (config.jacobianType, jacM) match {

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(Some(_))) => {
      ijac.set(gamd_jacobian_e.JAC_USER_PROVIDED.value.toInt)
      mljac.set(ml)
      mujac.set(mu)
      LogIt().info("user supplied banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.BandedJacobian(LowerBandWidth(ml), UpperBandWidth(mu)), JacobianFuncM(None)) => {
      ijac.set(gamd_jacobian_e.INTERNAL.value.toInt)
      LogIt().info("internally generated banded jacobian with lower bandwith " + ml + " and upper bandwith " + mu)
    }

    case (JacobianType.FullJacobian, JacobianFuncM(Some(_))) => {
      ijac.set(gamd_jacobian_e.JAC_USER_PROVIDED.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().info("user supplied full jacobian ")
    }
    case (JacobianType.FullJacobian, JacobianFuncM(None)) => {
      ijac.set(gamd_jacobian_e.INTERNAL.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().info("generated full jacobian")
    }

    case (_, JacobianFuncM(Some(_))) => {
      ijac.set(gamd_jacobian_e.JAC_USER_PROVIDED.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().warn("user provided jacobian with jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
    }
    case (_, JacobianFuncM(None)) => {
      ijac.set(gamd_jacobian_e.INTERNAL.value.toInt)
      mljac.set(dim)
      mujac.set(dim)
      LogIt().warn("internally generated jacobian; user provided jacobian type : " + config.jacobianType + " but defaulting to JacobianType.FullJacobian")
    }
  }

  private val jac = new gamd_jac_callback {

    override def apply(neq: Pointer[Integer], x: Pointer[lang.Double], y: Pointer[lang.Double], pd: Pointer[lang.Double], nrowpd: Pointer[Integer],
                       rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {
      logger.trace("calling jacobian")
      val pd_r = pd.getDoubles(neq.get() * neq.get())
      jacM(neq.get(), x.get(), y.getDoubles(neq.get()), mljac.get(), mujac.get(), pd_r, nrowpd.get(), params) match {
        case Some(_) => pd.setDoubles(pd_r)
        case None => logger.error("error in jacobian")
      }

    }
  }
  private val jac_sp: Pointer[gamd_jac_callback] = Pointer.getPointer(jac)

  (massM, config.mass) match {
    case (_, MassMatrixType.IdentityMatrix) => imas.set(gamd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)
    case (MassMatrixFuncM(None), _) =>  imas.set(gamd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)

    case (MassMatrixFuncM(Some(_)), MassMatrixType.FullMassMatrix) => {
      imas.set(gamd_mass_matrix_e.MASS_USER_PROVIDED.value.toInt)
      mlmas.set(dim)
      mumas.set(dim)
    }

    case (MassMatrixFuncM(Some(_)), MassMatrixType.BandedMassMatrix(LowerBandWidth(ml), UpperBandWidth(mu))) => {
      imas.set(gamd_mass_matrix_e.MASS_USER_PROVIDED.value.toInt)
      mlmas.set(ml)
      mumas.set(mu)
    }

    case (_,_) => {
      LogIt().warn("provided mas matrix type " + config.mass + " and mass function " + massM + " but resolving to identiy matrix/regular ode")
      imas.set(gamd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)
    }
  }
  //
  private val mass = new gamd_mas_callback {

    override def apply(neq: Pointer[Integer], am: Pointer[lang.Double], lmas: Pointer[Integer], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {
      val am_r :Array[Double] = am.getDoubles(neq.get()*lmas.get())
      massM(neq.get(), am_r, lmas.get(), mlmas.get(), mumas.get(), params) match {
        case Some(_) => am.setDoubles(am_r)
        case None => logger.error("error in the mass matrix call back")
      }
    }

  }
  private val mass_sp: Pointer[gamd_mas_callback] = Pointer.getPointer(mass)
  daoVar match {
    case DaeIndexVariables(None,None,None) => {
      LogIt().warn("no index variables set so assuming not a dao; defaulting to ODE ")
      imas.set(gamd_mass_matrix_e.IDENTITY_MATRIX.value.toInt)
      mlmas.set(dim)
      mumas.set(dim)
    }
    case DaeIndexVariables(Some(i1),None,None) => iwork.set(24,i1)
    case DaeIndexVariables(Some(i1),Some(i2),None) => {
      iwork.set(24, i1)
      iwork.set(25, i2)
    }
    case DaeIndexVariables(Some(i1),Some(i2),Some(i3)) => {
      iwork.set(24, i1)
      iwork.set(25, i2)
      iwork.set(26, i3)
    }
  }



  config.set_itol(itol,rtol,atol)
  LogIt().info(" lrw : " + lwork.get() + " liw : " + liwork.get())

  config.options match {
    case None => {
      LogIt().info("no configuration options provided")
    }
    case Some(options) => {
      LogIt().info(options.toString)
      for (initStepSize <- options.initialStepSize) yield h.set(initStepSize)
      for (v <- options.maxStepSize) yield work.set(1, v)

      for (v <- options.maxSteps)    yield iwork.set(1, v)
      for (v <- options.minOrder)    yield iwork.set(2, v)
      for (v <- options.maxOrder)    yield iwork.set(3, v)


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

    LogIt().diagnostic("number of function evaluations so far                   : " + iwork.getIntAtIndex(9))
    LogIt().diagnostic("number of jacobian evaluations so far                   : " + iwork.getIntAtIndex(10))
    LogIt().diagnostic("number of matrix LU decompositions so far               : " + iwork.getIntAtIndex(23))
    val order = Array(3,5,7,9)
    for (index <- Range(11,15)){
      LogIt().diagnostic("number of computed steps per method                             (order:  " + order(index-11) + "): " + iwork.getIntAtIndex(index))
    }

    for (index <- Range(15,19)){
      LogIt().diagnostic("number of rejected steps per method due to newton's convergence (order :  " + order(index-15) + "): " + iwork.getIntAtIndex(index))
    }
    for (index <- Range(19,23)){
      LogIt().diagnostic("number of rejected steps per method due to error test           (order :  " + order(index-19) + "): " + iwork.getIntAtIndex(index))
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
      CgamdLibrary.gamd(neq, func_sp, x, y, xend, h, rtol, atol, itol, jac_sp, ijac, mljac, mujac, mass_sp, imas,
        mlmas, mumas, null, iout, work, lwork, iwork, liwork, null, null, idid)
      diagnostics
      gamd_idid_e.fromValue(idid.get()) match {
        case c if c == gamd_idid_e.SUCCESS => {
          stack.append(xend.get())
          for (yval <- y.getDoubles(neq.get())) stack.append(yval)
          Some(x.get())
        }
        case c if c == gamd_idid_e.INPUT_INCONSISTENT => {
          LogIt().error("inconsitent input ; idid : " + c.value())
          None
        }
        case c if c == gamd_idid_e.NMAX_TOO_SMALL => {
          LogIt().error("larger number of iterations (namx) needed ; idid : " + c.value())
          None
        }
        case c if c == gamd_idid_e.STEP_TOO_SMALL => {
          LogIt().error("step size too small ; idid : " + c.value())
          None
        }
        case c if c == gamd_idid_e.STIFF_PROBLEM => {
          LogIt().error("matrix is repeatedly singular ; idid : " + c.value())
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
