package com.kabouterlabs.ode.examples.daeexample






import breeze.plot.{Figure, plot}
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.OdeSolver._
import com.kabouterlabs.ode.config._

import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.{ConvertArrayToFortranMatrix, ConvertArrayToMatrix, LogIt}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._

/**
  *
  *  pendulum dae
  *
  * dy1/dt = y2
  * 0     = y1 - cos(t)
  *
  *
  */


object DaeExample {
  private def collect(l:(List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2))
  }
  def apply[A]()(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}) = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ivpsolver = Ivp(dim = 2) +
      (Config() -> JacobianType.FullJacobian -> MassMatrixType.FullMassMatrix -> Tolerance(0.000001)) +
      DaeIndexVariables(1,1) +
      FuncParams("g" -> -9.81, "lambda" -> 1.0) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        ydot(0) = y(1)
        ydot(1) = y(0) - math.cos(x)

      })  +
      ((dim:Int, am:Array[Double], lmass:Int, ml:Int, mu:Int, params:FuncParams[Double]) => {

        am(0) = 1.0
        am(1) = 0.0
        am(2) = 0.0
        am(3) = 0.0
      }) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.01)) +>

    val eval = ivpsolver.solve(LineRange(0.0, 10.0, 0.01), Array(math.cos(0.0), - math.sin(0.0)))
    //eval().map(_.show)
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => {
        LogIt().warn("no results returned for the bouncing ball system")
        (List[Double](), List[Double](), List[Double]())
      }
    }

  }
}


import com.kabouterlabs.ode.implicits.radau5.Radau5Implicit._

object DaeExampleExample
{
  def main(args:Array[String])
  {

    val lists = DaeExample()
    val (time, xval, yval) = lists

    val fig = Figure("DaeExample")
    fig.width = (fig.width * 1).toInt
    fig.height = (fig.height * 2).toInt

    val plt  = fig.subplot(2, 1,  0)
    plt += plot(time, xval)
    plt.xlabel = "time"
    plt.ylabel = "x"

    val plt1 = fig.subplot(2, 1,  1)
    plt1 += plot(time, yval)
    plt1.xlabel = "time"
    plt1.ylabel = "y"
    
  }
}
