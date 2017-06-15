package com.kabouterlabs.ode.kernel

import com.kabouterlabs.ode.FuncParams
import com.kabouterlabs.ode.util.HandleException

/**
  * Created by fons on 1/14/17.
  */



case class JacobianFuncM[U](jacOption:Option[(Int, U, Array[U], Int, Int, Array[U], Int, FuncParams[U])=>Unit])
{
  type ElemT = U
  type FuncT = (Int, U, Array[U], Int, Int, Array[U], Int, FuncParams[U])=>Unit



  def map(h:FuncT=>FuncT):JacobianFuncM[U] = jacOption match {
    case Some(jac) => JacobianFuncM[U](Some(h(jac)))
    case _         => JacobianFuncM[U](None)
  }

  def apply(neq:Int, x:U, y:Array[U], ml:Int, mu:Int, pd:Array[U], nrowpd:Int, params:FuncParams[U]): Option[Unit] =
    HandleException {jacOption.map(_(neq,x,y,ml,mu,pd,nrowpd,params))}

}

object JacobianFuncM
{
  //def apply[U](jacOption:Option[(Int, U, Array[U], Int, Int, Array[U], Int)=>Unit]) = new JacobianFuncM[U](jacOption)
  def none[U] = new JacobianFuncM[U](None)
}
