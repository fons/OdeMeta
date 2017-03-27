package com.kabouterlabs.ode.odepack

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import org.bridj.{Pointer, IntValuedEnum}
import scala.language.implicitConversions
/**
  * Created by fons on 1/16/17.
  */
object OdePackTypes {

  type OdeBasicFunT = (Pointer[java.lang.Double], Pointer[java.lang.Double], Pointer[CodepackLibrary.codepack_ode_func],
      Int, Double, Double, Double) => IntValuedEnum[CodepackLibrary.codepack_ode_err_e]

  type OdeBasicFunMfT = (Pointer[java.lang.Double], Pointer[java.lang.Double], Pointer[CodepackLibrary.codepack_ode_func],
    Int, Double, Double, Double, IntValuedEnum[CodepackLibrary.codepack_method_e]) => IntValuedEnum[CodepackLibrary.codepack_ode_err_e]

  implicit def converFuncMfToFunc(fun:OdeBasicFunT):OdeBasicFunMfT =  (stack:Pointer[java.lang.Double],
    init:Pointer[java.lang.Double], f:Pointer[CodepackLibrary.codepack_ode_func],
    neq:Int, t0:Double, tf:Double, dt:Double, mf:IntValuedEnum[CodepackLibrary.codepack_method_e]) => fun(stack, init, f, neq,t0,tf,dt)
}
