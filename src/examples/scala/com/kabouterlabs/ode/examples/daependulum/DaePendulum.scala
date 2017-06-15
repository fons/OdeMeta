package com.kabouterlabs.ode.examples.daependulum




import breeze.plot.{Figure, plot}
import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.kernel.OdeSolver._
import com.kabouterlabs.ode.config._

import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.{ConvertArrayToMatrix, LogIt}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._

/**
  *
  *  pendulum dae
  *
  * dx/dt = u
  * dy/dt = v
  * du/dt = - lambda x
  * dv/dt = - lambda y - 9.81
  * 0     = x^2 + y^2 - 1
  *
  *
  */


object DaePendulum {
  private def collect(l:(List[Double], List[Double], List[Double], List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2), l._4 :+ a(3), l._5 :+ a(4), l._6 :+ a(5))
  }
  def apply[A]()(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}) = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ivpsolver = Ivp(dim = 5) +
      (Config() -> JacobianType.FullJacobian -> MassMatrixType.FullMassMatrix -> Tolerance(0.000001)) +
      DaeIndexVariables(2,2,1) +
      FuncParams("g" -> 9.81, "lambda" -> 1.0) +
    ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        ydot(0) = y(2)
        ydot(1) = y(3)
        ydot(2) = - y(4) * y(0)
        ydot(3) = - y(4) * y(1) - (params->"g")
        ydot(4) = y(0)*y(0) + y(1)*y(1) - (params->"lambda")
      })  +
      ((dim:Int, am:Array[Double], lmass:Int, ml:Int, mu:Int, params:FuncParams[Double]) => {

        val conv = ConvertArrayToMatrix(am)
        for (index <- Range(0,4)) yield conv(index, index, 1.0)
        conv(4,4,0)
        //for (index <- Range(0,5*5)) println("index : " + index + " " + am(index))

    }) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.01)) +>

    val eval = ivpsolver.solve(LineRange(0.00, 10.0, 0.01), Array(1.0, 0.0, 0.0, 1.0, 1.0))
    //eval().map(_.show)
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => {
        LogIt().warn("no results returned for the dae pendulum  system")
        (List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double]())
      }
    }

  }
}


import com.kabouterlabs.ode.implicits.radau5.Radau5Implicit._

object DaePendulumExample
{
  def main(args:Array[String])
  {

    val lists = DaePendulum()
    val (time, xval, yval, dxval, dyval, lval) = lists

    val fig = Figure("DaePendulum")
    fig.width = (fig.width * 3).toInt
    fig.height = (fig.height * 3).toInt

    val plt  = fig.subplot(3, 3,  0)
    plt += plot(time, xval)
    plt.xlabel = "time"
    plt.ylabel = "x"

    val plt1 = fig.subplot(3, 3,  1)
    plt1 += plot(time, yval)
    plt1.xlabel = "time"
    plt1.ylabel = "y"

    val plt2 = fig.subplot(3, 3,  2)
    plt2 += plot(time, dxval)
    plt2.xlabel = "time"
    plt2.ylabel = "dx/dt"

    val plt3 = fig.subplot(3, 3,  3)
    plt3 += plot(time, dyval)
    plt3.xlabel = "time"
    plt3.ylabel = "dy/dt"

    val plt4 = fig.subplot(3, 3,  4)
    plt4 += plot(time, lval)
    plt4.xlabel = "time"
    plt4.ylabel = "lambda (tension)"

    val plt5 = fig.subplot(3, 3,  5)
    plt5 += plot(xval, yval)
    plt5.xlabel = "x"
    plt5.ylabel = "y"

    val v:List[Double] = xval.map((x)=>x*x).zip(yval.map((y)=>y*y)).map((x)=>x._1 + x._2 - 1.0)

    val plt6 = fig.subplot(3, 3,  6)
    plt6 += plot(time, v)
    plt6.xlabel = "time"
    plt6.ylabel = "x^2 + y^2 - 1"

    val plt7 = fig.subplot(3, 3,  7)
    plt7 += plot(xval, dxval)
    plt7.xlabel = "x"
    plt7.ylabel = "dx/dt"

    val plt8 = fig.subplot(3, 3,  8)
    plt8 += plot(yval, dyval)
    plt8.xlabel = "y"
    plt8.ylabel = "dy/dt"
    
  }
}
