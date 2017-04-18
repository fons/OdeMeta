package com.kabouterlabs.ode.odepack

import java.lang

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.jodeint.codepack.CodepackLibrary._
import com.kabouterlabs.ode.config.{LowerBandWidth, UpperBandWidth}
import com.kabouterlabs.ode.config.{Config, JacobianType, Methods}
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import org.bridj.Pointer

/**
  * Created by fons on 1/29/17.
  */
case class Lsodes(dim:Int, funcM:OdeFuncM[Double], jacM:JacobianFuncM[Double], params:FuncParams[Double], config:Config)
{
  LogIt().info("configuration : " + config)
  
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
  case class Settings(mf:Int, lrw:Int, liw:Int)

  implicit class ev$config(config: Config) {

    case class Selection(miter:Int, mos:Int, lwm:Int, liw:Int)

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
          itol.set(codepack_itol_e.ALL_SCALAR.value.toInt);
          rtol.set(rtolv);
          atol.set(atolv)
        }
        case ((Some(rtolv), None), (None, Some(atola))) => {
          itol.set(codepack_itol_e.ATOL_ARRAY.value.toInt);
          rtol.set(rtolv);
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
          itol.set(-10)
          rtol.set(-100.0)
          atol.set(-99999.999)
        }
      }
    }

    def mf() = {
      val meth = config.method match {
        case Methods.ADAMS => 1
        case Methods.BDF   => 2
        case _             => 2
      }

      val Selection(miter, mos, lwm, liw) = (config.jacobianType, jacM) match {

          // miter = 0; moss = 0  b/c irrelevant
        case (JacobianType.PreferNoJacobian, _)                                    => Selection(0, 0, 0, 30)

         //miter = 1; moss = 2 => cannot exist
        //case (JacobianType.SparseJacobian(nnz), JacobianFuncM(Some(_)))            => (1, 2, 2*nnz+2*dim + (nnz+9*dim) )

          // miter = 1; user supplied jacobian; moss = 0 IA/JA supplied ;includes banded jacobians and full jacobians
        case (JacobianType.SparseJacobianStructure(ia,_), JacobianFuncM(Some(_)))  => Selection(1, 0, 2*ia.length+2*dim + (ia.length + 9*dim) , 31 + dim + ia.length)
        case (JacobianType.BandedJacobian(ml,mu), JacobianFuncM(Some(_)))   => {val nnz = JacobianType.BandedJacobian.nonzero(dim,ml,mu)
          Selection(1, 0, 2 * nnz +2 * dim + (nnz+9 * dim), 31 + dim + nnz)
        }

        //================================================================================================================
        //miter = 1; user supplied jac; moss = 1; structure will be figured out  ; assume nnz = dim*dim i.e full matrix
        case (JacobianType.SparseJacobian, JacobianFuncM(Some(_)))        => Selection(1,1, 2 * dim*dim + 2 * dim + (dim*dim + 9 * dim ), 30)
        case (JacobianType.BandedJacobian, JacobianFuncM(Some(_)))        => Selection(1,1, 2 * dim*dim + 2 * dim + (dim*dim + 9 * dim ), 30)

        case (JacobianType.FullJacobian, JacobianFuncM(Some(_)))          => {val nnz = dim * dim
          Selection(1, 1,2 * nnz +2 * dim + (nnz+9 * dim), 30)}

        //miter = 2 (internal jacobian); mos = 0: IA/JA supplied; include  banded jacobian
        case (JacobianType.SparseJacobianStructure(ia,_), JacobianFuncM(None))     => Selection(2, 0, 2*ia.length + 2*dim + (ia.length + 10 * dim), 31 + dim + ia.length)
        case (JacobianType.BandedJacobian(ml,mu), JacobianFuncM(None))=> {val nnz = JacobianType.BandedJacobian.nonzero(dim,ml,mu)
          Selection(2, 0, 2 * nnz +2 * dim + (nnz+10 * dim),31 + dim + nnz)
        }

        //miter =2 (internal jacobian) mos =2 : structure from calls to F
        case (JacobianType.SparseJacobian, JacobianFuncM(None))               => Selection(2, 2, 2*dim*dim + 2*dim + (dim*dim + 10 * dim),30)
        case (JacobianType.BandedJacobian, JacobianFuncM(None))               => Selection(2, 2, 2*dim*dim + 2*dim + (dim*dim + 10 * dim), 30)
        case (JacobianType.FullJacobian,   JacobianFuncM(None))               => Selection(2, 2, 2 * dim * dim + 2 * dim + (dim * dim + 10 * dim), 30)

        //miter = 3; moss =0 b/c irrelevant
        case (JacobianType.DiagonalJacobian, JacobianFuncM(None))             => Selection(3, 0, dim + 2, 30)

        // default
        case (_,_) => {
          LogIt().warn("user provided jacobian type : " + config.jacobianType + " but defaulting to no PreferNoJacobian")
          Selection(0, 0, 0, 30)
        }
      }


      val mf = 100 * mos + 10 * meth + miter

      val lrw = mf match {
        case 10 => 20 + 16 *dim
        case 11 | 111 |  12 | 212 => 20 + 16 * dim + lwm
        case 13 => 22 + 17 * dim
        case 20 => 20 + 9 * dim
        case 21 | 121 | 22 | 222 => 20 + 9 * dim + lwm
        case 23 => 22 + 10 * dim
      }

      Settings(mf,lrw,liw)

    }
  }

//end of implicit

  private val jac = new dlsodes_jac_callback {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], j: Pointer[Integer], ian: Pointer[lang.Double], jan: Pointer[lang.Double], pdj: Pointer[lang.Double]): Unit = {

      //LogIt().trace("calling jacobian")
      val pd_r = pdj.getDoubles(neq.get() * neq.get())
      jacM(neq.get(), t.get(), y.getDoubles(neq.get()), -1, -1, pd_r, j.get(), params) match {
        case Some(_) => pdj.setDoubles(pd_r)
        case None => LogIt().error("error in jacobian")
      }

    }
  }


  private val func = new dlsodes_f_callback {
    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double]): Unit = {
      {
        val ydot_r: Array[Double] = ydot.getDoubles(neq.get())
        //LogIt().trace("calling function")
        funcM(neq.get, t.getDouble, y.getDoubles(neq.get()), ydot_r, params) match {
          case Some(_) => ydot.setDoubles(ydot_r)
          case None => LogIt().error("error in function")
        }
      }
    }
  }

  val settings = config.mf()

  LogIt().trace("settings : " + settings.toString() + " dim : " + dim)

  private val func_sp: Pointer[dlsodes_f_callback] = Pointer.getPointer(func)

  private val neq: Pointer[Integer] = Pointer.pointerToInt(dim)
  private val y: Pointer[lang.Double] = Pointer.allocateDoubles(dim)
  private val t: Pointer[lang.Double] = Pointer.allocateDouble()

  private val tout: Pointer[lang.Double] = Pointer.allocateDouble()
  private val itol: Pointer[lang.Integer] = Pointer.allocateInt()
  private val rtol: Pointer[lang.Double] = Pointer.allocateDoubles(config.rtolDim(dim))
  private val atol: Pointer[lang.Double] = Pointer.allocateDoubles(config.atolDim(dim))
  private val itask: Pointer[lang.Integer] = Pointer.allocateInt()
  private val istate: Pointer[lang.Integer] = Pointer.allocateInt()
  private val iopt: Pointer[lang.Integer] = Pointer.allocateInt()
  private val lrw: Pointer[lang.Integer] = Pointer.pointerToInt(settings.lrw)
  private val rwork: Pointer[lang.Double] = Pointer.allocateDoubles(lrw.get().toLong)
  private val liw: Pointer[lang.Integer] = Pointer.pointerToInt(settings.liw)
  private val iwork: Pointer[lang.Integer] = Pointer.allocateInts(liw.get().toLong)
  private val jac_sp: Pointer[dlsodes_jac_callback] = Pointer.getPointer(jac)
  private val mfl: Pointer[lang.Integer] = Pointer.pointerToInt(settings.mf)

  config.set_itol(itol,rtol,atol)
  log_tolerance_settings(itol,rtol,atol)
  istate.set(CodepackLibrary.codepack_istate_in_e.FIRST_CALL.value.toInt)
  itask.set(CodepackLibrary.codepack_itask_e.NORMAL.value.toInt)

  config.jacobianType match {
    case JacobianType.SparseJacobianStructure(ia,ja) => {
      for (j <- Range(1, dim+2)) {
        iwork.set(29+j, ia(j))
      }
      for (k <- Range(1, dim+1)){
        iwork(30 + dim + k) = ja(k)
      }
    }

    case JacobianType.BandedJacobian(ml, mu)   => {
       val jacj = JacobianType.BandedJacobian.toSparseJacobianStructure(dim, ml,mu)
      for (j <- Range(1, dim+2)) {
        iwork.set(29+j, jacj.ia(j))
      }
      for (k <- Range(1, dim+1)){
        iwork(30 + dim + k) = jacj.ja(k)
      }
    }

    case _ => iwork.set(0,0)
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

  def diagnostics_on(): Unit = {
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
    LogIt().diagnostic("number of nonzero elements in the jacobian, incl. diag. : " + iwork.getIntAtIndex(18))
    LogIt().diagnostic("number of groups of column indices, used if miter=2     : " + iwork.getIntAtIndex(19))
    LogIt().diagnostic("number of sparse LU decompositions so far               : " + iwork.getIntAtIndex(20))

    LogIt().diagnostic("non-zero elements in the strict lower triangle of LU    : " + iwork.getIntAtIndex(24))
    LogIt().diagnostic("non-zero elements in the strict upper triangle of LU    : " + iwork.getIntAtIndex(25))
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
    val stack = StackDouble(dim, range)
    y.setDoubles(init.slice(0, neq.get()))
    t.set(range.start)
    tout.set(range.start)
    stack.append(range.start)
    for (yval <- y.getDoubles(neq.get())) stack.append(yval)

    range.withRange((next: Double) => {
      tout.set(next)
      //println("===>", jt.get())
      LogIt().trace(" tout : " + tout.get() + " t " + t.get() + " istate " + istate.get())
      dlsodes(func_sp, neq, y, t, tout, itol, rtol, atol, itask, istate, iopt, rwork, lrw, iwork, liw, jac_sp, mfl)
      diagnostics
      codepack_istate_out_e.fromValue(istate.get()) match {
        case c if c == codepack_istate_out_e.NOTHING_DONE || c == codepack_istate_out_e.SUCCESS_DONE => {
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
        case c if c == codepack_istate_out_e.MAX_STEPS_EXCEEDED => {
          LogIt().error("excessive amount of work done + istate : " + c.value())
          None
        }
        case c if c == codepack_istate_out_e.TO_MUCH_ACCURACY => {
          LogIt().error("excess accuracy requested + istate : " + c.value())
          LogIt().diagnostic("tolerance scale factor        : " + rwork.getDoubleAtIndex(13))
          None
        }

        case c if c == codepack_istate_out_e.ILLEGAL_INPUT => {
          LogIt().error("illegal input + istate : " + c.value())
          LogIt().diagnostic("tolerance scale factor        : " + rwork.getDoubleAtIndex(13))
          None
        }
        case c if c == codepack_istate_out_e.ERROR_TEST_FAILURES => {
          LogIt().error("repeated error test failures + istate : " + c.value())
          LogIt().diagnostic("index of component with the largest weighted local error vector :" + iwork.getIntAtIndex(15))
          None
        }
        case c if c == codepack_istate_out_e.CONVERGENCE_FAILURES => {
          LogIt().error("repeated convergence failures + istate : " + c.value())
          LogIt().diagnostic("index of component with the largest weighted local error vector :" + iwork.getIntAtIndex(15))
          None
        }
        case c if c == codepack_istate_out_e.ZERO_ERR_TOLERANCE => {
          LogIt().error("error weight zero + istate : " + c.value)
          None
        }
        case c if c == codepack_istate_out_e.TOO_SMALL_WORK_ARRAY => {
          LogIt().error("work space insufficient + istate : " + c.value())
          None
        }
      }

    }) match {
      case Some(_) => Some(stack)
      case None => {
        LogIt().warn("error occured; no results returned")
        None
      }

    }
  }

}
