package com.kabouterlabs.ode.util

/**
  * Created by fons on 4/13/17.
  */
case class ConvertArrayToMatrix(arr:Array[Double])
{

  val dim = math.sqrt(arr.length).toInt

  def apply(row:Int, column:Int, value:Double) = arr(row + column * dim)=value
  def apply(row:Int, column:Int) = arr(row + column * dim)

}
