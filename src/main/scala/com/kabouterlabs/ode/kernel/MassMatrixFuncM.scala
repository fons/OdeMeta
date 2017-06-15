package com.kabouterlabs.ode.kernel

import com.kabouterlabs.ode.FuncParams
import com.kabouterlabs.ode.util.HandleException

/**
  * Created by fons on 3/8/17.
  */
case class MassMatrixFuncM[U](massOption:Option[(Int, Array[U], Int, Int, Int, FuncParams[U])=>Unit])
{
  type ElemT = U
  type FuncT = (Int, Array[U], Int, Int, Int, FuncParams[U])=>Unit

  def map(h:FuncT=>FuncT):MassMatrixFuncM[U] = massOption match {
    case Some(mass) => MassMatrixFuncM[U](Some(h(mass)))
    case _         => MassMatrixFuncM[U](None)
  }
  def apply(neq:Int, am:Array[U], lmass:Int, ml:Int, mu: Int, params:FuncParams[U]): Option[Unit] =
    HandleException {massOption.map(_(neq,am, lmass, ml, mu, params))}

}

object MassMatrixFuncM
{
  //def apply[U](jacOption:Option[(Int, U, Array[U], Int, Int, Array[U], Int)=>Unit]) = new JacobianFuncM[U](jacOption)
  def none[U] = new MassMatrixFuncM[U](None)
}
