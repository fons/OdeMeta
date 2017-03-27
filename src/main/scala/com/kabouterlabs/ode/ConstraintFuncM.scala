package com.kabouterlabs.ode

import com.kabouterlabs.ode.util.HandleException

/**
  * Created by fons on 1/16/17.
  */
case class ConstraintFuncM[U](constraintOption:Option[(Int, U, Array[U], Int, Array[U], FuncParams[U])=>Unit])
{
  type ElemT = U
  type FuncT = (Int, U, Array[U], Int, Array[U], FuncParams[U])=>Unit

  def map(h:FuncT=>FuncT):ConstraintFuncM[U] = constraintOption match {
    case Some(jac) => ConstraintFuncM[U](Some(h(jac)))
    case _         => ConstraintFuncM[U](None)
  }

  def apply(neq:Int, x:U, y:Array[U], ng:Int, gout:Array[U], params: FuncParams[U]):Option[Unit] = HandleException{ constraintOption.map(_(neq,x,y,ng,gout,params)) }
}

object ConstraintFuncM
{
  def none[U] = new ConstraintFuncM[U](None)
}