package com.kabouterlabs.ode.examples.arenstorf



import com.kabouterlabs.ode.util.ConvertArrayToMatrix

/**
  * Created by fons on 4/14/17.
  */


import breeze.plot._

import com.kabouterlabs.ode.kernel.OdeSolver._

import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.LogIt
import com.kabouterlabs.ode.{FuncParams, Ivp}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}


/**
 * Solving the Arenstorf equation.
 *
  *
 * y_1'' = y1 + 2y_1' - mu_2 (y1 + mu_1)/D1 - mu_1 (y_1-mu_2)/D2
 * y_2'' = y2 + 2y_1' - mu_2 y2/D1 - mu_1 y_2/D2
 *
 * D1 = ((y_1 + mu_1)^2 + y_2^2)^(3/2)
 * D2 = ((y_1 - mu_2)^2 + y_2^2)^(3/2)
 * 
 */
object ArenstorfExample {

  private def collect(l:(List[Double], List[Double], List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2), l._4 :+ a(3), l._5 :+ a(4))
  }

  def apply[A](init:Array[Double])(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}):(List[Double], List[Double], List[Double], List[Double], List[Double])  = {

    LogIt().level.info()
    /**
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val mu1 =  0.012277471

    val ivpsolver = Ivp(dim = 4) +
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.0000000000001)) +
      FuncParams[Double]("mu1" -> mu1, "mu2" -> (1.0 - mu1)) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        val mu1 = params->"mu1"
        val mu2 = params->"mu2"
        val d1  =  (y(0) + mu1)
        val D1  = math.pow(d1*d1 + y(1)*y(1), 3.0/2.0)
        val d2  = y(0) - mu2
        val D2  = math.pow(d2*d2 + y(1)*y(1), 3.0/2.0)
        ydot(0) = y(2)
        ydot(1) = y(3)
        ydot(2) = y(0) + 2*y(3) - mu2*(y(0)+mu1)/D1 - mu1 * (y(0) - mu2)/D2
        ydot(3) = y(1) - 2*y(2) - mu2*y(1)/D1 - mu1 * y(1)/D2
      }) +
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +
      ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {
        val mu1 = params->"mu1"
        val mu2 = params->"mu2"

        val d1  =  (y(0) + mu1)
        val D1  = math.pow(d1*d1 + y(1)*y(1), 3.0/2.0)
        val d2  = y(0) - mu2
        val D2  = math.pow(d2*d2 + y(1)*y(1), 3.0/2.0)

        val conv = ConvertArrayToMatrix(pd)
        conv(0,0, 0.0)
        conv(0,1, 0.0)
        conv(0,2, 1.0)
        conv(0,3, 0.0)

        conv(1,0,0.0)
        conv(1,1, 0.0)
        conv(1,2, 0.0)
        conv(1,3, 0.0)

        conv(2,0, 1.0 - mu2/D1 - mu1/D2)
        conv(2,1,0.0)
        conv(2,2,0.0)
        conv(2,3,2.0)

        conv(3,0,0.0)
        conv(3,1,1.0 - mu2/D1 - mu1/D2)
        conv(3,2,-2.0)
        conv(3,3,0.0)

      }) +>


    val eval = ivpsolver.solve(LineRange(0.0, 20.0, 0.005), init)
    /**
     * eval returns a lazy object which needs to be executed to get the values
     */
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => (List[Double](), List[Double](), List[Double](), List[Double](), List[Double]())
    }


  }
}

/////import com.kabouterlabs.ode.implicits.radau5.Radau5Implicit._
import com.kabouterlabs.ode.experimental.implicits.RKEmbeddedFehlberg56Implicit._

object Arenstorf {
  def main(args: Array[String]) = {
    val res1 = ArenstorfExample(Array(0.994, 0.0,0.0, -2.0317326295573368357302057924))
    val res2 = ArenstorfExample(Array(0.994, 0.0,0.0, -2.00158510637908252240537862224))
    val res3 = ArenstorfExample(Array(1.2, 0.0,0.0, -1.049357510))

    val fig = Figure("arenstof")
    fig.width = (fig.width * 2).toInt
    fig.height = (fig.height * 2).toInt


    val plt  = fig.subplot(2, 2,  0)
    plt.xlabel = "time"
    plt.ylabel = "x"

    val plt1 = fig.subplot(2, 2,  1)
    plt1.xlabel = "time"
    plt1.ylabel = "y"

    val plt2 = fig.subplot(2, 2,  2)
    plt2.xlabel = "xval"
    plt2.ylabel = "yval"

    val plt3 = fig.subplot(2, 2,  3)
    plt3.xlabel = "xval"
    plt3.ylabel = "dxval"
    
    val results = Array(res1, res2, res3)
    println(results.mkString(","))

    val a = for (lists <- results) {
      println("plotting")
      val (time, xval, yval, dxval, dyval) = lists
      plt += plot(time, xval)
      plt1 += plot(time, yval)
      plt2 += plot(xval, yval)
      plt3 += plot(xval, dxval)
    }
    fig.refresh()
    println("done")

  }
}

