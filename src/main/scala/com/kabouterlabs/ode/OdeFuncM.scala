package com.kabouterlabs.ode

import java.io.{PrintWriter, StringWriter}

import com.kabouterlabs.ode.util.HandleException



/**
  * Created by fons on 1/6/17.
  */



case class OdeFuncM[U](funcOption:Option[(Int, Double, Array[U], Array[U], FuncParams[U])=>Unit]) {
  type ElemT = U
  type FuncT =  (Int, Double, Array[U], Array[U], FuncParams[U])=>Unit

  def map(h :FuncT=>FuncT):OdeFuncM[U] = OdeFuncM[U](Some(h(funcOption.get)))
  def apply(n:Int, x:Double, y:Array[U], ydot:Array[U], params:FuncParams[U]):Option[Unit] =
      HandleException {funcOption.map(_(n, x, y, ydot,params))}

}

object OdeFuncM
{
  def none[U] = new OdeFuncM[U](None)
}