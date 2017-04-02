package com.kabouterlabs.ode

/**
  *
  * This is a stack of results returned by the solvers.
  * The results for each integration step are stored in an array of data type U.
  * The first element in that array is the independent variable, followed by the dependent variables in the order
  * they appear in the solution array.
  * 
  */
trait StackT
{
  type ElemT
  def show:Unit
  def toFile(fn:String)
  val size:Int
  def toArray:Array[Array[Double]]
  def append(e:ElemT):StackT
  def fromArray(arr:Array[ElemT]):StackT
  def apply(index:Int):Array[ElemT]
  def first:Array[ElemT]
  def last:Array[ElemT]
}
