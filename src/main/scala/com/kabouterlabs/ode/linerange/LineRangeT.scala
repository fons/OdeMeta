package com.kabouterlabs.ode.linerange

/**
  * Created by fons on 1/7/17.
  */



//abstract class LineRangeT[U](val start:U, val end :U, val stepSize:U)
//{
//  type ElemT=U
//  def steps:Int
//  def withRange(f: (U)=>Option[U]):Option[U]
//}

trait LineRangeT[U]
{
  type ElemT=U
  val start:U
  val end:U
  val stepSize:U
  val steps:Int
  def withRange(f: (U)=>Option[U]):Option[U]
}
