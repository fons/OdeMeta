package com.kabouterlabs.ode.symplectic.gniirk2


import java.lang

import com.kabouterlabs.jodeint.cgnicodes.CgnicodesLibrary
import com.kabouterlabs.jodeint.cgnicodes.CgnicodesLibrary._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import com.kabouterlabs.ode._
import org.bridj.Pointer

/**
  * Created by fons on 3/7/17.
  */
case class GniIrk2(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{
  private val logger = LogIt()

  private val method = config.method match {
    case Methods.SYMPLECTIC_2_STAGES => gni_irk2_method_e.IRK2_METH_2.value.toInt
    case Methods.SYMPLECTIC_4_STAGES => gni_irk2_method_e.IRK2_METH_4.value.toInt
    case Methods.SYMPLECTIC_6_STAGES => gni_irk2_method_e.IRK2_METH_6.value.toInt
    case _ => gni_irk2_method_e.IRK2_METH_2.value.toInt

  }
  private val func = new gni_irk2_f_callback  {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {

      val ydot_r: Array[Double] = ydot.getDoubles(neq.get())
      logger.trace("calling function")
      funcM(neq.get, t.getDouble, y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => logger.error("error in the function call back")
      }
    }
  }

  private val func_sp: Pointer[gni_irk2_f_callback] = Pointer.getPointer(func)

  private val neq: Pointer[lang.Integer] = Pointer.pointerToInt(dim)
  private val Q: Pointer[lang.Double] = Pointer.allocateDoubles(dim)
  private val P: Pointer[lang.Double] = Pointer.allocateDoubles(dim)
  private val x: Pointer[lang.Double] = Pointer.allocateDouble()
  private val xend: Pointer[lang.Double] = Pointer.allocateDouble()
  private val nstep: Pointer[lang.Integer] = Pointer.allocateInt()
  private val meth: Pointer[lang.Integer]  = Pointer.pointerToInt(method)
  private val rpar: Pointer[lang.Double] = Pointer.allocateDoubles(10)
  private val ipar: Pointer[lang.Integer] = Pointer.allocateInts(10)
  //solout
  private val iout: Pointer[lang.Integer] = Pointer.pointerToInt(gnicodes_iout_e.NEVER_CALLED.value.toInt)
  nstep.set(1)


  config.absoluteTolerance.get match {
    case (Some(tol), _) if tol != 0.0 => rpar.set(0, tol)
    case (_ , Some(tol)) if tol.length > 0 && tol(0) != 0.0 => rpar.set(0, tol(0))
    case (_,_ ) => rpar
  }

  config.options match {
    case None => {
      LogIt().info("no configuration options provided")
    }
    case Some(options) => {
      LogIt().info(options.toString)
      for (v <- options.maxSteps)  yield ipar.set(0, v)
      for (v <- options.minSteps)  yield nstep.set(v)
    }
    case _ => ipar  
  }

  /*
    The init array contains both the initial position (Q) and velocity (P)
     */
  def run(range: LineRangeT[Double], init: Array[Double]): Option[StackT] = HandleException {
    LogIt().info("dimension :" + dim + " ; starting with range : " + range + " initial conditions : {" + init.mkString(",") + "}")
    val stack = StackDouble(2 * dim, range)
    Q.setDoubles(init.slice(0, neq.get()))
    P.setDoubles(init.slice(neq.get(), neq.get()*2))
    x.set(range.start)

    xend.set(range.start)
    stack.append(range.start)
    for (qval <- Q.getDoubles(neq.get())) stack.append(qval)
    for (pval <- P.getDoubles(neq.get())) stack.append(pval)
    range.withRange((next:Double) => {
      xend.set(next)
      logger.trace(" xend : " + xend.get() + " x " + x.get())
      CgnicodesLibrary.gni_irk2(neq, func_sp, nstep, x, P, Q,xend, meth, null, iout, rpar, ipar)
      val Qresult = Q.getDoubles(neq.get())
      val Presult = P.getDoubles(neq.get())
      NonValueChecker(Qresult).hasNonValue || NonValueChecker(Presult).hasNonValue match {
            case true => {
              LogIt().error("detected non-values in the result : " + Qresult.mkString(",") + ";" + Presult.mkString(",") + " stop processing")
              None
            }
            case false => {
              stack.append(xend.get())
              for (qval <- Q.getDoubles(neq.get())) stack.append(qval)
              for (pval <- P.getDoubles(neq.get())) stack.append(pval)
              Some(x.get())
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




