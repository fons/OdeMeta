package com.kabouterlabs.ode.rkf

import java.lang

import com.kabouterlabs.jodeint.crkf45.Crkf45Library._
import com.kabouterlabs.ode.config.Config
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt}
import com.kabouterlabs.ode.{StackT, LineRangeT, FuncParams, OdeFuncM}
import org.bridj.Pointer

/**
  * Created by fons on 3/2/17.
  */
class Rkf45Basic(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{
  private val func1 = new rkf45_ode_func{


    override def apply(t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double]): Unit = {


      val ydot_r:Array[Double] = ydot.getDoubles(dim)

      funcM(dim, t.get(),y.getDoubles(dim), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => LogIt().error("error in function callback")
      }
    }
  }

  private val func_sp: Pointer[rkf45_ode_func] = Pointer.getPointer(func1)


  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] =
    HandleException {
      val stack = StackDouble(dim, range)
      val init_ptr = Pointer.allocateDoubles(dim)
      init_ptr.setDoubles(init.slice(0,dim))


      val ptr: Pointer[lang.Double] = Pointer.allocateDoubles(stack.size)
      rkf45_basic(ptr, init_ptr, func_sp, dim, range.start, range.end, range.stepSize) match {
        case rkf45_retval_e.SUCCESS => Some(stack.fromArray(ptr.getDoubles(stack.size)))
        case err => {
          LogIt().error("error encountered in rkf45 basic :" + err)
          None
        }
      }
    }
}



