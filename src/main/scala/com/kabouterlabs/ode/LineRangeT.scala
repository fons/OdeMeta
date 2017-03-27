package com.kabouterlabs.ode

/**
  * Created by fons on 1/7/17.
  */



abstract class LineRangeT[U](val start:U, val end :U, val stepSize:U)
{
  type ElemT=U
  def steps:Int
  def withRange(f: (U)=>Option[U]):Option[U]
}
