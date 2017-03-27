package com.kabouterlabs.ode.radau5

import java.lang

import com.kabouterlabs.jodeint.cradau5.Cradau5Library._
import com.kabouterlabs.ode.config.Config
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.{FuncParams, OdeFuncM, StackT, LineRangeT}
import com.kabouterlabs.ode.util.{HandleException, LogIt}
import org.bridj.Pointer

/**
  * Created by fons on 3/6/17.
  */
class Radau5Basic(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{
  private val func1 = new radau5_ode_func   {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double],
                       rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {


      val ydot_r:Array[Double] = ydot.getDoubles(neq.get())

      funcM(neq.get, t.get(),y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => LogIt().error("error in function callback")
      }
    }
  }

  private val func_sp: Pointer[radau5_ode_func] = Pointer.getPointer(func1)


  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] =
    HandleException {
      val stack = StackDouble(dim, range)
      val init_ptr = Pointer.allocateDoubles(dim)
      init_ptr.setDoubles(init.slice(0,dim))


      val ptr: Pointer[lang.Double] = Pointer.allocateDoubles(stack.size)
      radau5_basic(ptr, init_ptr, func_sp, dim, range.start, range.end, range.stepSize) match {
        case radau5_idid_e.SUCCESS => Some(stack.fromArray(ptr.getDoubles(stack.size)))
        case err => {
          LogIt().error("error encountered in dvode basic :" + err)
          None
        }
      }
    }
}
