package com.kabouterlabs.ode.rkf

import com.kabouterlabs.jodeint.cdop853.Cdop853Library._
import com.kabouterlabs.ode.stack.{StackDouble, StackT}
import com.kabouterlabs.ode.util.HandleException
import com.kabouterlabs.ode.util.LogIt
import java.lang

import org.bridj.Pointer
import com.kabouterlabs.ode.config.Config
import com.kabouterlabs.ode.FuncParams
import com.kabouterlabs.ode.kernel.OdeFuncM
import com.kabouterlabs.ode.linerange.LineRangeT

/**
  * Created by fons on 3/2/17.
  */
class Dop853Basic(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{
  private val func1 = new dop853_ode_func    {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double],
                       rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {


      val ydot_r:Array[Double] = ydot.getDoubles(neq.get())

      funcM(neq.get, t.get(),y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => LogIt().error("error in function callback")
      }
    }
  }

  private val func_sp: Pointer[dop853_ode_func] = Pointer.getPointer(func1)


  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] =
    HandleException {
      val stack = StackDouble(dim, range)
      val init_ptr = Pointer.allocateDoubles(dim)
      init_ptr.setDoubles(init.slice(0,dim))


      val ptr: Pointer[lang.Double] = Pointer.allocateDoubles(stack.size)
      dop853_basic(ptr, init_ptr, func_sp, dim, range.start, range.end, range.stepSize) match {
        case dop853_idid_e.SUCCESS => Some(stack.fromArray(ptr.getDoubles(stack.size)))
        case err => {
          LogIt().error("error encountered in dopri5 basic :" + err)
          None
        }
      }
    }
}
