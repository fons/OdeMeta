package com.kabouterlabs.ode.examples.bouncingball




import breeze.plot.{Figure, plot}
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.OdeSolver._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.examples.henonheiles.HenonHeilesExample.collect
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.{ConvertArrayToFortranMatrix, ConvertArrayToMatrix, LogIt}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._

import scala.collection.immutable.Range
/**
  *
  *  Bouncing Ball
  *
  *  d^2y/dt^2 = -g
  *  y(0) = 0
  *  y'(00 = 10.0
  *  Looses 10 % of its energy on the return bounce
  *
  *
  */


object BouncingBall {
  private def collect(l:(List[Double], List[Double], List[Double], List[Double]), a:Array[Double]) = {
    (l._1 :+ a(0), l._2 :+ a(1), l._3 :+ a(2), l._4 :+ a(3))
  }
  def apply[A]()(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}) = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ivpsolver = Ivp(dim = 2, constraints=1) +
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
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(Array(0.000000001, 0.000000000000001, 0.000001))) +
      /*
        * The parameters used in the functions. This is a standardized way of supplying data to the ode functions and bypasses the way fortran code.
       */
      FuncParams("g" -> -9.81) +
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
        ydot(1) = params->"g"
      })  +
      /*
      * Constraint callback function to find roots by sodar.
      * gout are the constraint fucntions and  determine the zeros'
      */
      ((dim:Int, x:Double, y:Array[Double], ng:Int, gout:Array[Double], params: FuncParams[Double]) => {
        gout(0) = y(0)
      }) +
      /*
        * Event function called when roots are found
        * The array of dependent variables can be changed here.
       */
      ((dim: Int, x:Double, y:Array[Double], ng:Int, jroot:Array[Int], params: FuncParams[Double])=>{

        println("root found : " + x + " " + y.mkString(" , ") + " add jroot : [ " + jroot.mkString(" , ") + " ]")
        y(0)  = 0.0
        y(1)  = -0.9 * y(1)
        println("new values : " + y.mkString(","))
      }) +
      /*
        * Settting optional parameters. Those differ per solver but the most common ones have been factored out.
        * In the setting below the diagnostic messages have not been suppressed and an initial step size is provided.
        * The optional parameters are highly specific to the fortran ode solver being called.
        * Use the OptionalParameters to pass in iwork or rwark arrays if that's needed.
       */
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +>

    val eval = ivpsolver.solve(LineRange(0.0, 20.0, 0.01), Array(0.0, 10.0))
    //eval().map(_.show)
    val result = for (result <- eval()) yield ( (List[Double](), List[Double](), List[Double](), List[Double]()) /: result.toArray){collect(_,_)}

    result match {
      case Some(tuple) => tuple
      case None => {
        LogIt().warn("no results returned for the bouncing ball system")
        (List[Double](), List[Double](), List[Double](), List[Double]())
      }
    }

  }
}


import com.kabouterlabs.ode.implicits.lsodar.LsodarImplicit._

object BouncingBallExample
{
  def main(args:Array[String])
  {

    val lists = BouncingBall()
    val (time, xval, dxval, zval) = lists
    
    val fig = Figure("Bouncing Ball")
    fig.width = (fig.width * 2).toInt
    fig.height = (fig.height * 3).toInt


    val plt  = fig.subplot(3, 1,  0)
    plt += plot(time, xval)
    
    val plt1 = fig.subplot(3, 1,  1)
    plt1 += plot(time, zval)
    
    val plt2 = fig.subplot(3, 1,  2)
    plt2 += plot(xval, dxval)
    
  }
}
