package com.kabouterlabs.ode.examples.vanderpol

/**
  * Created by fons on 4/4/17.
  */


import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.OdeSolver._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.LogIt

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._

import breeze.plot._


/*
 * Solving the van der pol equation.
 *
 * dx/dt = y
 * dy/dt = mu (1 - y1*y1)*y2 - y1
 *
 */
object VanderPolExample {
  private def collect(l:(List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2))
  }
  def apply[A](mu:Double)(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}):(List[Double], List[Double], List[Double])  = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ivpsolver = Ivp(dim = 2) +
      /*
        * Configure the solver. Notice that Backward Differention (BDF) is selected.
        * The solver will ignore options it does not require, e.g. the soda slover will automtically detect the right differention scheme and will ignore this option.
        *
        * The following options are used :
        *
        *  - Method       : This is the basic linear mutistep method : BDF (backward differentiation) or Adams. The full method flag is determined in combination
        *                   with othet configuration parameters provided, notably the jocobian type.
        *
        *  - JacobianType : In this case a full jacobian is assumed. If the jacobian function is present the suer supplied jobian option will be selected
        *                   in the codepack wrapper. Other options as banded and sparse.
        *
        *  - Tolerances   : The Tolerance class can be used to simply provide the same relative and absolute tolerances. Below the absolute tolerance
        *                    is different for each dependent variable and the relative tolerance is the same
        *
       */
      (Config(Methods.BDF) -> JacobianType.FullJacobian ->Tolerance(0.000001)  -> AbsoluteTolerance(Array(0.000000001, 0.000000000000001, 0.000001))) +
      /*
        * The parameters used in the functions. This is a standardized way of supplying data to the ode functions and bypasses the way fortran code.
       */
      FuncParams("mu" -> mu) +
      /*
        * This is the ODE callback function.
        *
        * It implements the differential equation.
        * x is the independent variable; y are the depdent variables.
        * ydot are the new values of the differentials
        * Only ydot is marshalled and unmarshalled, so that's the only data set that is allowed to change
       */
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        ydot(0) = y(1)
        ydot(1) = (params -> "mu") * (1.0 - y(0) * y(0))*y(1) - y(0)

      }) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +
    /*
    Jacobian call back; passing the jacobian back to the array in column order
     */
    ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {
        val mu = params -> "mu"

        pd(0) = 0
        pd(1) = -mu * 2 * y(0) * y(1) - 1
        pd(2) = 1
        pd(3) = mu * (1.0 - y(0)*y(0))

      }) +>
     

    val eval = ivpsolver.solve(LineRange(0.0, 50.0, 0.02), Array(1.0, 0.0))
    /*
     * eval returns a lazy object which needs to be executed to get the values
     */
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => (List[Double](), List[Double](), List[Double]())
    }
    

  }
}

import com.kabouterlabs.ode.implicits.lsoda.LsodaImplicit._

//import com.kabouterlabs.ode.experimental.implicits.RKEmbedded56Implicit._


object VanderPol
{
  def main(args:Array[String]) = {
    val res1 = VanderPolExample(0.01)
    val res2 = VanderPolExample(0.1)
    val res3 = (for (index <- Range(1, 10)) yield VanderPolExample(index * 1.0)).toArray
    
    val fig = Figure("vanderpol")
    fig.width  = (fig.width * 2).toInt
    fig.height = (fig.height * 2).toInt
    

    val plt  = fig.subplot(1,1,0)
    plt.xlabel = "x"
    plt.ylabel = "dx/dt"

    val plt1 = fig.subplot(1,2,1)
    plt1.xlabel = "time"
    plt1.ylabel = "x"

    val plt2 = fig.subplot(2,2,2)
    plt2.xlabel = "time"
    plt2.ylabel = "dx/dt"
    
    val results = res1 +: res2 +: res3

    println(results.mkString(","))

    val a = for (lists <- results) {
      println("plotting")
      val (time, xval, dxval) = lists
      plt   += plot(xval, dxval)
      plt1  += plot(time, xval)
      plt2  += plot(time, dxval)
    }
    fig.refresh()
   println("done")
    
  }
}
