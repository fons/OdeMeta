package com.kabouterlabs.ode.examples.lorenz

import com.kabouterlabs.ode.util.ConvertArrayToMatrix

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
 * Solving the Lorentz equation.
 *
 * dX/dt = a * X + Y * Z
 * dY/dt = b * (Y - Z)
 * dZ/dt = - X * Y + c * Y - Z
 *
 */
object LorenzExample {

  private def collect(l:(List[Double], List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2), l._4 :+ a(3))
  }

  def apply[A]()(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}):(List[Double], List[Double], List[Double], List[Double])  = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ivpsolver = Ivp(dim = 3) +
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.000001)) +
      FuncParams[Double]("a" -> -8.0/3.0, "b" -> -10.0, "c" -> 28.0) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        /*
         * X <-> y(0)
         * Y <-> y(1)
         * Z <-> y(2)
         */
        val a = params->"a"
        val b = params->"b"
        val c = params->"c"
        ydot(0) = a * y(0) + y(1)*y(2)
        ydot(1) = b * (y(1) - y(2))
        ydot(2) = - y(0) * y(1) + c * y(1) - y(2)
      }) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +
    ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {
      val a = params->"a"
      val b = params->"b"
      val c = params->"c"
      val conv = ConvertArrayToMatrix(pd)
      conv(0,0,a)
      conv(0,1, y(2))
      conv(0,2, y(1))
      conv(1,0,0)
      conv(1,1,b)
      conv(1,2,-b)
      conv(2,0,-y(1))
      conv(2,1,-y(0)+c)
      conv(2,2,-1.0)
      }) +>


    val eval = ivpsolver.solve(LineRange(0.0, 100.0, 0.01), Array(0.01, 0.01,0.01))
    /*
     * eval returns a lazy object which needs to be executed to get the values
     */
    //val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}
    val result = for (result <- eval()) yield ( result.toArray.foldLeft((List[Double](), List[Double](), List[Double](), List[Double]()) )){collect(_,_)}
    result match {
      case Some(tuple) => tuple
      case None => (List[Double](), List[Double](), List[Double](), List[Double]())
    }


  }
}

import com.kabouterlabs.ode.implicits.dvode.DvodeImplicit._


object Lorenz {
  def main(args: Array[String]) = {
    val res1 = LorenzExample()


    val fig = Figure("lorenz")
    fig.width = (fig.width * 2).toInt
    fig.height = (fig.height * 2).toInt


    val plt  = fig.subplot(2, 3,  0)
    plt.xlabel = "time"
    plt.ylabel = "x"

    val plt1 = fig.subplot(2, 3,  1)
    plt1.xlabel = "time"
    plt1.ylabel = "y"

    val plt2 = fig.subplot(2, 3,  2)
    plt2.xlabel = "time"
    plt2.ylabel = "z"

    val plt3 = fig.subplot(2, 3,  3)
    plt3.xlabel = "x"
    plt3.ylabel = "y"

    val plt4 = fig.subplot(2, 3,  4)
    plt4.xlabel = "x"
    plt4.ylabel = "z"

    val plt5 = fig.subplot(2, 3,  5)
    plt5.xlabel = "y"
    plt5.ylabel = "z"

    val results = Array(res1)

    println(results.mkString(","))

    val a = for (lists <- results) {
      println("plotting")
      val (time, xval, yval, zval) = lists
      plt += plot(time, xval)
      plt1 += plot(time, yval)
      plt2 += plot(time, zval)
      plt3 += plot(xval, yval)
      plt4 += plot(xval, zval)
      plt5 += plot(yval, zval)

    }
    fig.refresh()
    println("done")

  }
}
