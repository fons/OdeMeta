package com.kabouterlabs.ode

import com.kabouterlabs.ode.util.HandleException

/**
  * Created by fons on 1/19/17.
  */

case class EventFuncM[U](eventOpt:Option[(Int, Double, Array[U], Int, Array[Int], FuncParams[U]) =>Unit] )
{
  type ElemT = U
  type FuncT =  (Int, Double, Array[U], Int, Array[Int], FuncParams[U]) =>Unit

  def map(h :FuncT=>FuncT):EventFuncM[U] = EventFuncM[U](Some(h(eventOpt.get)))
  def apply(n:Int, x:Double, y:Array[U], ng: Int, jroot:Array[Int], params:FuncParams[U]):Option[Unit] =
    HandleException {eventOpt.map(_(n, x, y, ng, jroot, params))}

}

object EventFuncM
{
  def none[U] = new EventFuncM[U](None)
}