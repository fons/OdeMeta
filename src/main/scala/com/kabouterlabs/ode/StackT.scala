package com.kabouterlabs.ode

/**
  * Created by fons on 1/7/17.
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
  def apply(index:Int):Option[Array[ElemT]]
  def first:Option[Array[ElemT]]
  def last:Option[Array[ElemT]]
}
