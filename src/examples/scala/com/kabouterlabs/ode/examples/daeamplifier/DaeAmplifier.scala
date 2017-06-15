package com.kabouterlabs.ode.examples.daeamplifier



import breeze.plot.{Figure, plot}
import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.kernel.OdeSolver._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.{ConvertArrayToMatrix, LogIt}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._
import org.bouncycastle.jce.provider.JCEKeyGenerator.RC4
import org.jfree.chart.axis.NumberTickUnit

/**
  *
  *  transistor amplifier dae
  *
  *
  */


object DaeAmplifier {
  private def collect(l:(List[Double], List[Double], List[Double], List[Double], List[Double], List[Double], List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2), l._4 :+ a(3), l._5 :+ a(4), l._6 :+ a(5), l._7 :+ a(6), l._8 :+ a(7), l._9 :+ a(8))
  }
  def apply[A]()(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}) = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ub = 6.0
    val R1 = 9000.0
    val R2 = 9000.0
    val R5 = 9000.0
    val R6 = 9000.0

    val ivpsolver = Ivp(dim = 8) +
      (Config() -> JacobianType.FullJacobian -> MassMatrixType.FullMassMatrix -> Tolerance(0.000001)) +
      DaeIndexVariables(8) +
      FuncParams("ub" -> ub,
        "uf" -> 0.026,
        "alpha"->0.99,
        "beta" -> "1.0e-6".toDouble,
        "R0" -> 1000.0,
        "R1" -> R1,
        "R2" -> R2,
        "R3" -> 9000.0,
        "R4" -> 9000.0,
        "R5" -> R5,
        "R6" -> R6,
        "R7" -> 9000.0,
        "R8" -> 9000.0,
        "R9" -> 9000.0,
        "C1" -> "1.0e-6".toDouble,
        "C2" -> "2.0e-6".toDouble,
        "C3" -> "3.0e-6".toDouble,
        "C4" -> "4.0e-6".toDouble,
        "C5" -> "5.0e-6".toDouble) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {

        val alpha = params->"alpha"
        val beta  = params->"beta"
        val uf    = params->"uf"
        val ub    = params->"ub"

        val R0    = params->"R0"
        val R1    = params->"R1"
        val R2    = params->"R2"
        val R3    = params->"R3"
        val R4    = params->"R4"
        val R5    = params->"R5"
        val R6    = params->"R6"
        val R7    = params->"R7"
        val R8    = params->"R8"
        val R9    = params->"R9"

        val uin = 0.1 * math.sin(200.0 * math.Pi * x)
        val g23 = beta * (math.exp((y(1)-y(2))/uf)  - 1.0)
        val g56 = beta * (math.exp((y(4)-y(5))/uf)  - 1.0)

        ydot(0) = (y(0) - uin)/R0
        ydot(1) = y(1)/R1 + (y(1) - ub)/R2 + (1.0 - alpha) * g23
        ydot(2) = y(2)/R3 - g23
        ydot(3) = (y(3) - ub)/R4 + alpha * g23
        ydot(4) = y(4)/R5 + (y(4)-ub)/R6 + (1.0 - alpha)*g56
        ydot(5) = y(5)/R7 - g56
        ydot(6) = (y(6)-ub)/R8 + alpha * g56
        ydot(7) = y(7)/R9

      })  +
      ((dim:Int, am:Array[Double], lmass:Int, ml:Int, mu:Int, params:FuncParams[Double]) => {


        val C1 = params->"C1"
        val C2 = params->"C2"
        val C3 = params->"C3"
        val C4 = params->"C4"
        val C5 = params->"C5"
        val conv = ConvertArrayToMatrix(am)

        conv(0,0, -C1)
        conv(1,1, -C1)
        conv(1,0,  C1)
        conv(0,1,  C1)
        conv(2,2, -C2)

        conv(3,3, -C3)
        conv(4,4, -C3)
        conv(4,3,  C3)
        conv(3,4,  C3)

        conv(5,5,-C4)

        conv(6,6, -C5)
        conv(7,7, -C5)
        conv(7,6,  C5)
        conv(6,7,  C5)


      }) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.01)) +>

    val i1 = ub / (R2/R1 + 1)
    val i2 = ub / (R6/R5 + 1)
    val eval = ivpsolver.solve(LineRange(0.0, 0.2, 0.001), Array(0, i1, i1, ub, i2, i2, ub, 0.0))
    //eval().map(_.show)
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double](),List[Double](), List[Double](), List[Double](),
      List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => {
        LogIt().warn("no results returned for the dae pendulum  system")
        (List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double](),List[Double](), List[Double](), List[Double]())
      }
    }

  }
}


import com.kabouterlabs.ode.implicits.radau5.Radau5Implicit._

object DaeAmplifierExample
{
  def main(args:Array[String])
  {

    val lists = DaeAmplifier()
    val (time, u1, u2, u3, u4, u5, u6, u7, u8) = lists

    val fig = Figure("DaeAmplifier")
    fig.width = (fig.width * 3).toInt
    fig.height = (fig.height * 3).toInt

    val plt  = fig.subplot(3, 3,  0)
    plt += plot(time, u1)
    plt.xlabel = "time"
    plt.ylabel = "u1"

    val plt1  = fig.subplot(3, 3,  1)
    plt1 += plot(time, u2)
    plt1.xlabel = "time"
    plt1.ylabel = "u2"

    val plt2  = fig.subplot(3, 3,  2)
    plt2 += plot(time, u3)
    plt2.xlabel = "time"
    plt2.ylabel = "u3"

    val plt3  = fig.subplot(3, 3,  3)
    plt3 += plot(time, u4)
    plt3.xlabel = "time"
    plt3.ylabel = "u4"

    val plt4  = fig.subplot(3, 3,  4)
    plt4 += plot(time, u5)
    plt4.xlabel = "time"
    plt4.ylabel = "u5"

    val plt5  = fig.subplot(3, 3,  5)
    plt5 += plot(time, u6)
    plt5.xlabel = "time"
    plt5.ylabel = "u6"

    val plt6  = fig.subplot(3, 3,  6)
    plt6 += plot(time, u7)
    plt6.xlabel = "time"
    plt6.ylabel = "u7"

    val plt7  = fig.subplot(3, 3,  7)
    plt7 += plot(time, u8)
    plt7.xlabel = "time"
    plt7.ylabel = "u8"


  }
}
