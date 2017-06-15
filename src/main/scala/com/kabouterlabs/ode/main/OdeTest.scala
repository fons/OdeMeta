package com.kabouterlabs.ode.main

import com.kabouterlabs.ode.kernel.OdeSolver._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.LogIt
import scala.language.{postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._

/**
  * Created by fons on 3/15/17.
  */
case class OdeTest[A](implicit ev1:OdeSolverTC[A]{type SolverDataType=Double})
{
  def apply() = {
    println("starting here")
    val logIt = LogIt()
    logIt.level.info()
    //
    val alpha = -0.04
    val beta  = "1.0e4".toDouble
    val gamma =  "3.0e7".toDouble
    val params =  FuncParams +\ ("alpha" -> alpha, "beta" -> beta, "gamma" -> gamma)

    val w = Ivp(dim=3, constraints=0)+ (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.000000001)) + FuncParams("alpha" -> -0.04, "beta" -> "1.0e4".toDouble, "gamma" -> "3.0e7".toDouble ) +
      ( (dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params:FuncParams[Double]) => {
        ydot(0) = (params -> "alpha") * y(0) + (params -> "beta") * y(1) * y(2)
        ydot(2) = (params -> "gamma") * y(1)*y(1)
        ydot(1) = -ydot(0) - ydot(2)
      } ) + ((dim:Int, x:Double, y:Array[Double], mul:Int, mpk:Int, pd:Array[Double], pdr:Int, params:FuncParams[Double])=> {
      val alpha  = params -> "alpha"
      val beta   = params -> "beta"
      val gamma  = params -> "gamma"
      pd(0) = alpha
      pd(1) = -alpha
      pd(2) = 0.0
      pd(3) = beta * y(2)
      pd(4) = -beta*y(2) - 2.0 * gamma * y(1)
      pd(5) = 2.0 * gamma * y(1)
      pd(6) = beta * y(1)
      pd(7) = -1.0 * beta * y(1)
      pd(8) = 0.0
    } ) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.000001) ) +>


    //val result = OdeSolver.solve(w, LineRange(0.0, 4.0, 0.1), Array(1.0, 0.0,0.0,0.0))
    println(ev1, w)
    w.solve(LineRange(0.0, 4.0, 0.1), Array(1.0, 0.0,0.0,0.0))().map(_.show)

  }
}
