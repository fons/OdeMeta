package com.kabouterlabs.ode.odepack

import java.lang

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.jodeint.codepack.CodepackLibrary.{codepack_method_e, codepack_ode_err_e, codepack_ode_func}
import com.kabouterlabs.ode.config.{Methods, Config}
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{LogIt, HandleException}
import com.kabouterlabs.ode.{FuncParams, StackT, LineRangeT, OdeFuncM}
import org.bridj.{IntValuedEnum, Pointer}
import com.kabouterlabs.ode.odepack.OdePackTypes._
/**
  * Created by fons on 1/16/17.
  */


case class OdePackBasic(dim:Int, funcM:OdeFuncM[Double], func:OdePackTypes.OdeBasicFunMfT,
                        params:FuncParams[Double], config:Config)(implicit ev$MfConfigT:MethodFlagConfigT)
{

  def this (dim:Int, funcM:OdeFuncM[Double], func:OdePackTypes.OdeBasicFunT, params:FuncParams[Double])(implicit ev$MfConfigT:MethodFlagConfigT)
  {
    this(dim, funcM, func,params,Config())
  }
  private val func1 = new codepack_ode_func {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double]): Unit = {
      val ydot_r:Array[Double] = ydot.getDoubles(neq.get())

      funcM(neq.get, t.get(),y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => LogIt().error("error in function callback")
      }
    }
  }

  private val func_sp: Pointer[codepack_ode_func] = Pointer.getPointer(func1)


  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] =
    HandleException {
      val stack = StackDouble(dim, range)
      val init_ptr = Pointer.allocateDoubles(dim)
      init_ptr.setDoubles(init.slice(0,dim))
      val mf:codepack_method_e = ev$MfConfigT.set_mf(config)

      val ptr: Pointer[lang.Double] = Pointer.allocateDoubles(stack.size)
      func(ptr, init_ptr, func_sp, dim, range.start, range.end, range.stepSize, mf) match {
        case codepack_ode_err_e.SUCCESS => Some(stack.fromArray(ptr.getDoubles(stack.size)))
        case _ => None
      }
    }

}
