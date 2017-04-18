package com.kabouterlabs.ode.util

/**
  * Created by fons on 4/14/17.
  */
case class ConvertArrayToFortranMatrix(arr:Array[Double])
{
  
  val dim = math.sqrt(arr.length).toInt
  def apply(row:Int, column:Int, value:Double) = arr((row - 1) + (column - 1) * dim)=value
  def apply(row:Int, column:Int) = arr((row-1) + (column-1) * dim)

}
