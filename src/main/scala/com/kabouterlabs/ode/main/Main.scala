package com.kabouterlabs.ode.main



import com.kabouterlabs.ode.config.{Config, Tolerance}
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.experimental.ode.{OdeSolver, RKEmbedded56}
import com.kabouterlabs.ode.range.LineRange

import scala.language.existentials
import scala.language.reflectiveCalls
import scala.language.higherKinds
import scala.language.postfixOps
import scala.math.sin



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

case class Range1D(start:Double, end:Double, stepSize:Double)

import com.kabouterlabs.ode.implicits.OdeImplicits._

import com.kabouterlabs.ode.experimental.ode.implicits.RKEmbeddedFehlberg78Implicit._


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
  // ode to solve
  // simple pendulum
  def f(x: Double, y: List[Double]): Double = y(1)
  def g(x: Double, y: List[Double]): Double = -sin(y(0))
  def func(x:Double, y:List[Double]) = {
    List(f(x,y), g(x,y))
  }

  def func2(dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params:FuncParams[Double]): Unit =
  {
    ydot(0)= f(x,y.toList)
    ydot(1)= g(x,y.toList)
  }

  val init = List(3.138451060936203, 0.0)
  val tstart   = 0.00
  val tend     = 1.0
  val stepSize = 0.1
  val solver =  new OdeSolver(RKEmbedded56, 2, func2(_,_,_,_,_), FuncParams[Double]("alpha" -> 1.0), 0.0000001)
  val result = solver.run(tstart, tend, stepSize, init).getOrElse(List())
  for ((x, yargs) <- result)  yield {
    print(f"$x%.12f , ")
    for ( y <- yargs) {
      print(f"$y%.12f , ")
    }
    println("")
  }
  val result2 = solver.run(LineRange(tstart, tend, stepSize), init.toArray)
  result2.map(_.show)

  val ivpsolver = Ivp(2) +  (Config() -> Tolerance(0.000000000001)) + ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
    ydot(0)= f(x,y.toList)
    ydot(1)= g(x,y.toList)
  }) +>

  println(ivpsolver)
  val results = ivpsolver(LineRange(tstart, tend, stepSize), init.toArray)()
  results.map(_.show)

//  println(tstart, init)
//  var tend = tstart
//  var results = init
//  for (index <- Range(1,10)) {
//    tstart = tend
//    tend = index * 0.1
//    init = results
//    val (tend1, results1) = solver(tstart, tend, init)
//    //print(f"$res%.12e ,")
//    print(f"$tend1%.12f , ")
//    for (res <- results1)  print(f"$res%.12f ,")
//    println()
//      //f"//results1.mkString(","))
//    tend = tend1
//    results = results1
//  }
  /*
  0.700000000000000,3.137649425358330,-0.002383155985903,
  0.800000000000000,3.137390979846055,-0.002790060929765,
  0.900000000000000,3.137090482695206,-0.003224889492799,
   */
}
