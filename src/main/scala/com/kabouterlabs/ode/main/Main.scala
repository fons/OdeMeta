package com.kabouterlabs.ode.main



import scala.language.existentials
import scala.language.reflectiveCalls
import scala.language.higherKinds
import scala.language.postfixOps


/**
  * Created by fons on 1/5/17.
  */

case class FM(arr:Array[Double])
{
  private var a_ = 9.0
  private var i_ = 0
  private var j_ = 0

  def a = a_
  def a_=(v:Double):Unit = {a_ = v}

  def i = i_
  def i_=(v:Int)= {i_ = v}

  def j = j_
  def j_=(v:Int)= {j_ = v}

  def step(v:Int) = {
    i = v
    j_=_
  }

  def step2(v1:Int) = (v2:Int)=>{
    i = v1
    j = v2
    //a_=_
    a_
  }

  val dim = math.sqrt(arr.length).toInt
  def apply(row:Int, column:Int, value:Double) = {
    val index = column * dim + row
    println("index : " + index)
    arr(index) = value
  }
  def apply(row:Int,column:Int) = arr(row + dim*column)
}

object Main extends App {


  val arr1 = Array.ofDim[Double](3*3)
  val cv = FM(arr1)
  cv(0,0,10.0)
  cv(1,2,89.0)
  //cv(1,2) = 900000.0
  cv.a = 56.0
  val b= cv.step2(10)(9)
  //b = 78.0
  println(cv.i, cv.j, b)

  println(arr1.mkString(","))
}
