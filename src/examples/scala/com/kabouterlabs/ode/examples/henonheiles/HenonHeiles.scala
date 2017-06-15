package com.kabouterlabs.ode.examples.henonheiles

import breeze.plot.plot
import com.kabouterlabs.ode.util.{ConvertArrayToMatrix, NonValueChecker}

/**
  * Created by fons on 4/14/17.
  */


import breeze.plot._
import com.kabouterlabs.ode.kernel.OdeSolver.{OdeSolverTC, _}
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.LogIt
import com.kabouterlabs.ode.{FuncParams, Ivp}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}


/*
 * Solving the HenonHeiles equation.
 *
 * Autonomous Hamiltonian
 *
 * E = p^2_x + p^2_y + 1/2 (x^2 + y^2) + (x^2*y - y^3/3)
 * 
 *
 */
object HenonHeilesExample {

  private def collect(l:(List[Double], List[Double], List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2), l._4 :+ a(3), l._5 :+ a(4))
  }

  def apply[A](init:Array[Double])(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}):(List[Double], List[Double], List[Double], List[Double], List[Double])  = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val mu1 =  0.012277471

    val ivpsolver = Ivp(dim = 4) +
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.0000000000001)) +
      FuncParams[Double]("mu1" -> mu1) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {

        ydot(0) = y(2)
        ydot(1) = y(3)
        ydot(2) = -y(0) - 2.0 * y(0)*y(1)
        ydot(3) = - y(1) - y(0)*y(0) + y(1)*y(1)
      }) +
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +
      ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {


        val conv = ConvertArrayToMatrix(pd)
        conv(0,0, 0.0)
        conv(0,1, 0.0)
        conv(0,2, 1.0)
        conv(0,3, 0.0)

        conv(1,0, 0.0)
        conv(1,1, 0.0)
        conv(1,2, 0.0)
        conv(1,3, 1.0)

        conv(2,0, -1.0 - 2.0 * y(1))
        conv(2,1, - 2.0 * y(0))
        conv(2,2,0.0)
        conv(2,3,2.0)

        conv(3,0, 2.0*y(0))
        conv(3,1, -1 + 2*y(1))
        conv(3,2, 0.0)
        conv(3,3,0.0)

      }) +>


    val eval = ivpsolver.solve(LineRange(0.0, 35.0, 0.025), init)
    //eval().map(_.show)
    /*
     * eval returns a lazy object which needs to be executed to get the values
     */
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => {
        LogIt().warn("no results returned for Henon-Heiles system")
        (List[Double](), List[Double](), List[Double](), List[Double](), List[Double]())
      }
    }


  }
}

private case class HHPlotter(E:Double)
{
  val fig = Figure("henon-heiles")
  fig.width = (fig.width * 2).toInt
  fig.height = (fig.height * 2).toInt


  val plt  = fig.subplot(2, 3,  0)
  plt.xlabel = "time"
  plt.ylabel = "x"

  val plt1 = fig.subplot(2, 3,  1)
  plt1.xlabel = "time"
  plt1.ylabel = "y"

  val plt2 = fig.subplot(2, 3,  2)
  plt2.xlabel = "x"
  plt2.ylabel = "y"

  val plt3 = fig.subplot(2, 3,  3)
  plt3.xlabel = "y"
  plt3.ylabel = "dy/dt"

  val plt4 = fig.subplot(2, 3,  4)
  plt4.xlabel = "x"
  plt4.ylabel = "dx/dt"

  val plt5 = fig.subplot(2, 3,  5)
  plt5.xlabel = "time"
  plt5.ylabel = "energy residual * 1000"

  def apply(lists:(List[Double], List[Double], List[Double], List[Double], List[Double])): Unit = {
    println("plotting")
    val (time, xval, yval, dxval, dyval) = lists
    plt += plot(time, xval)
    plt1 += plot(time, yval)
    plt2 += plot(xval, yval)
    plt3 += plot(yval, dyval)
    plt4 += plot(xval, dxval)
    val energy = for (index <- time.indices) yield {
        val r:Double = 0.5 * (dxval(index) * dxval(index) + dyval(index)*dyval(index) + xval(index)*xval(index)+yval(index)*yval(index)) +
          yval(index)*(xval(index)*xval(index) - yval(index)*yval(index)/3.0)
        (r - E) * 1000
    }
    plt5 += plot(time, energy)

    fig.refresh()
    println("done")
  }
}

import com.kabouterlabs.ode.implicits.radau5.Radau5Implicit._


object HenonHeiles {
  def main(args: Array[String]) = {

    def zero(E:Double, y:Double, ydot:Double, start:Double):Double = {

      def f(xdot:Double)  =  E - 0.5 * xdot*xdot - 0.5*ydot*ydot - 0.5 * y * y +  y*y*y/3.0
      def fn(xdot:Double) = -xdot
      var trial = start
      while(math.abs(f(trial)) > 0.0000000001) {
        trial = trial - (f(trial) / fn(trial))
        //println(trial)
      }
      trial
    }



    val E = 1.0/6.0
    val plty = HHPlotter(E)

    val from  = -math.sqrt(2.0 * E)
    val to    = math.sqrt(2.0 * E)
    val steps = 10
    val stepsize = (to - from)/steps
    val res1 = HenonHeilesExample(Array(0.0,0.0,0.0,from))
    plty(res1)
    val res2 = HenonHeilesExample(Array(0.0,0.0,0.0,to))
    plty(res2)
    for (step <- Range(1,4)) yield {
      val x    = 0.0
      val y    = -0.3
      val ydot = from + step*stepsize * 1.1
      val xdot = zero(E, y, ydot, math.pow(2 * E - ydot *ydot, 0.5))

      val c    = 0.5 * xdot *xdot + 0.5*ydot*ydot + 0.5 * x * x  +  0.5 *y *y - y*y*y/3.0 - E
      println(x, xdot, y, ydot, c)
      val res3 = HenonHeilesExample(Array(0.0,y,0.0, ydot))
      plty(res3)
    }
    for (step <- Range(4,6)) yield {
      val x    = 0.0
      val y    = 0.3 //zero(E,ydot, 0.001)
      val ydot = from + step*stepsize*1.2
      val xdot = zero(E, y, ydot, math.pow(2 * E - ydot *ydot, 0.5))

      val c    = 0.5 * xdot *xdot + 0.5*ydot*ydot + 0.5 * x * x  +  0.5 *y *y - y*y*y/3.0 - E
      println(x, xdot, y, ydot, c)
      val res4 = HenonHeilesExample(Array(0.0,y,0.0, ydot))
      plty(res4)
    }

     for (step <- Range(1,10)) yield {
      val x    = 0.0
      val y    = 0.0
      val ydot = from + step*stepsize*1.01
      val xdot = zero(E, y, ydot, math.pow(2 * E - ydot *ydot, 0.5))

      val c    = 0.5 * xdot *xdot + 0.5*ydot*ydot + 0.5 * x * x  +  0.5 *y *y - y*y*y/3.0 - E
      println(x, xdot, y, ydot, c)
      val res5 = HenonHeilesExample(Array(0.0,y,0.0, ydot))
      plty(res5)
    }


    println("done")

  }
}

