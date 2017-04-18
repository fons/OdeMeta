package com.kabouterlabs.ode.examples.ode2

/**
  * Created by fons on 4/3/17.
  */
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.OdeSolver._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.LogIt

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._

import scala.collection.immutable.Range
/**
  *
  *  Chemical kinetics problem taken from the dlsodes fortran code.
  *
  *  Three rate equations :
  *
  *  dy1/dt  =  - rk1*y1
  *  dy2/dt  =  rk1*y1 + rk11*rk14*y4 + rk19*rk14*y5 - rk3*y2*y3 - rk15*y2*y12 - rk2*y2
  *  dy3/dt  =  rk2*y2 - rk5*y3 - rk3*y2*y3 - rk7*y10*y3 + rk11*rk14*y4 + rk12*rk14*y6
  *  dy4/dt  =  rk3*y2*y3 - rk11*rk14*y4 - rk4*y4
  *  dy5/dt  =  rk15*y2*y12 - rk19*rk14*y5 - rk16*y5
  *  dy6/dt  =  rk7*y10*y3 - rk12*rk14*y6 - rk8*y6
  *  dy7/dt  =  rk17*y10*y12  - rk20*rk14*y7 - rk18*y7
  *  dy8/dt  =  rk9*y10 - rk13*rk14*y8 - rk10*y8
  *  dy9/dt  =  rk4*y4 + rk16*y5 + rk8*y6 + rk18*y7
  *  dy10/dt =  rk5*y3 + rk12*rk14*y6 + rk20*rk14*y7 + rk13*rk14*y8 - rk7*y10*y3 - rk17*y10*y12 - rk6*y10 - rk9*y10
  *  dy11/dt =  rk10*y8
  *  dy12/dt =  rk6*y10 + rk19*rk14*y5 + rk20*rk14*y7 - rk15*y2*y12 - rk17*y10*y12
  *
  *  rk1  = rk5 = 0.1
  *  rk4  = rk8 = rk16 = rk18 = 2.5
  *  rk10 = 5
  *  rk2 = rk6 = 10
  *  rk14 = 30
  *  rk3 = rk7 = rk9 = rk11 = rk12 = rk13 = rk19 = rk20 = 50
  *  rk15 = rk17 = 100
  *
  *  This problem is sparse and stiff.
  *
  */


object Ode2Example {
  def apply[A](constraints:Int=0)(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}): Unit = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 12 d problem with 0 constraints (roots we need to satisfy) by default.
      *
     */
    val ivpsolver = Ivp(dim = 12) +
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
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(0.000001)) +
      /*
        * The parameters used in the functions. This is a standardized way of supplying data to the ode functions and bypasses the way fortran code.
       */
      FuncParams("rk1" -> 0.1,"rk5" -> 0.1,
        "rk4"->2.5,  "rk8"-> 2.5, "rk16" -> 2.5, "rk18"->2.5,
        "rk10" -> 5.0,
        "rk2" -> 10.0, "rk6" -> 10.0,
        "rk14" -> 30.0,
        "rk3" -> 50.0, "rk7" -> 50.0,"rk9" -> 50.0,"rk11" -> 50.0,"rk12" -> 50.0,"rk13" -> 50.0,"rk19" -> 50.0,"rk20" -> 50.0,
        "rk15" -> 100.0, "rk17" -> 100.0
      ) +
      /*
        * This is the ODE callback function.
        *
        * It implements the differential equation.
        * x is the independent variable; y are the depdent variables.
        * ydot are the new values of the differentials
        * Only ydot is marshalled and unmarshalled, so that's the only data set that is allowed to change
       */
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        val rk1  = params->"rk1"
        val rk2  = params->"rk2"
        val rk3  = params->"rk3"
        val rk4  = params->"rk4"
        val rk5  = params->"rk5"
        val rk6  = params->"rk6"
        val rk7  = params->"rk7"
        val rk8  = params->"rk8"
        val rk9  = params->"rk9"
        val rk10 = params->"rk10"
        val rk11 = params->"rk11"
        val rk12 = params->"rk12"
        val rk13 = params->"rk13"
        val rk14 = params->"rk14"
        val rk15 = params->"rk15"
        val rk16 = params->"rk16"
        val rk17 = params->"rk17"
        val rk18 = params->"rk18"
        val rk19 = params->"rk19"
        val rk20 = params->"rk20"


        ydot(0)  = -rk1*y(0)
        ydot(1)  = rk1*y(0) + rk11*rk14*y(3) + rk19*rk14*y(4) - rk3*y(1)*y(2) - rk15*y(1)*y(11) - rk2*y(1)
        ydot(2)  = rk2*y(1) - rk5*y(2) - rk3*y(1)*y(2) - rk7*y(9)*y(2) + rk11*rk14*y(3) + rk12*rk14*y(5)
        ydot(3)  = rk3*y(1)*y(2) - rk11*rk14*y(3) - rk4*y(3)
        ydot(4)  = rk15*y(1)*y(11) - rk19*rk14*y(4) - rk16*y(4)
        ydot(5)  = rk7*y(9)*y(2) - rk12*rk14*y(5) - rk8*y(5)
        ydot(6)  = rk17*y(9)*y(11) - rk20*rk14*y(6) - rk18*y(6)
        ydot(7)  = rk9*y(9) - rk13*rk14*y(7) - rk10*y(7)
        ydot(8)  = rk4*y(3) + rk16*y(4) + rk8*y(5) + rk18*y(6)
        ydot(9)  = rk5*y(2) + rk12*rk14*y(5) + rk20*rk14*y(6) + rk13*rk14*y(7) - rk7*y(9)*y(2) - rk17*y(9)*y(11) - rk6*y(9) - rk9*y(9)
        ydot(10) = rk10*y(7)
        ydot(11) = rk6*y(9) + rk19*rk14*y(4) + rk20*rk14*y(6) - rk15*y(1)*y(11) - rk17*y(9)*y(11)

      }) +
      /*
        * this is the jacobian callback function.
        *  pdc : column id  for which the jacobian is requested
       */
      ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdc: Int, params: FuncParams[Double]) => {
        val rk1  = params->"rk1"
        val rk2  = params->"rk2"
        val rk3  = params->"rk3"
        val rk4  = params->"rk4"
        val rk5  = params->"rk5"
        val rk6  = params->"rk6"
        val rk7  = params->"rk7"
        val rk8  = params->"rk8"
        val rk9  = params->"rk9"
        val rk10 = params->"rk10"
        val rk11 = params->"rk11"
        val rk12 = params->"rk12"
        val rk13 = params->"rk13"
        val rk14 = params->"rk14"
        val rk15 = params->"rk15"
        val rk16 = params->"rk16"
        val rk17 = params->"rk17"
        val rk18 = params->"rk18"
        val rk19 = params->"rk19"
        val rk20 = params->"rk20"

        pdc match {
          case 1 => {
            pd(0) = -rk1
            pd(1) = rk1
          }

          case 2 => {
            pd(1)  = -rk3 * y(2) - rk15 * y(11) - rk2
            pd(2)  = rk2 - rk3 * y(2)
            pd(3)  = rk3 * y(2)
            pd(4)  = rk15 * y(11)
            pd(11) = -rk15 * y(11)
          }

          case 3 => {
            pd(1) = -rk3 * y(1)
            pd(2) = -rk5 - rk3 * y(1) - rk7 * y(9)
            pd(3) = rk3 * y(1)
            pd(5) = rk7 * y(9)
            pd(9) = rk5 - rk7 * y(9)
          }
          case 4 => {
            pd(1) = rk11 * rk14
            pd(2) = rk11 * rk14
            pd(3) = -rk11 * rk14 - rk4
            pd(8) = rk4
          }
          case 5 => {
            pd(1) = rk19 * rk14
            pd(4) = -rk19 * rk14 - rk16
            pd(8) = rk16
            pd(11) = rk19 * rk14
          }
          case 6 => {
            pd(2) = rk12 * rk14
            pd(5) = -rk12 * rk14 - rk8
            pd(8) = rk8
            pd(9) = rk12 * rk14
          }
          case 7 => {
            pd(6) = -rk20 * rk14 - rk18
            pd(8) = rk18
            pd(9) = rk20 * rk14
            pd(11) = rk20 * rk14
          }
          case 8 => {
            pd(7) = -rk13 * rk14 - rk10
            pd(9) = rk13 * rk14
            pd(10) = rk10
          }
          case 9 => {}
          case 10 => {
            pd(2) = -rk7 * y(2)
            pd(5) = rk7 * y(2)
            pd(6) = rk17 * y(11)
            pd(7) = rk9
            pd(9) = -rk7 * y(2) - rk17 * y(11) - rk6 - rk9
            pd(11) = rk6 - rk17 * y(11)
          }
          case 11 => {}
          case 12 => {
            pd(1)  = -rk15 * y(1)
            pd(4)  =  rk15 * y(1)
            pd(6)  =   rk17 * y(9)
            pd(9)  = - rk17 * y(9)
            pd(11) = -rk15*y(1) - rk17 * y(9)
          }

        }
      }) +
      /*
        * Settting optional parameters. Those differ per solver but the most common ones have been factored out.
        * In the setting below the diagnostic messages have been suppressed and an initial step size is provided.
        * The optional parameters are highly specific to the fortran ode solver being called.
        * Use the OptionalParameters to pass in iwork or rwark arrays if that's needed.
       */
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true)) +>  //+> is the termintor; this returns the solver

    /*
     * This creates an array of independent variables (time in this case).
     */
    val range = for (index <- Range(0, 6)) yield  {
      if (index == 0) 0.0 else 0.1 * math.pow(10, index-1)
    }
    val init = for (index <- Range(0,12)) yield if (index == 0)  1.0 else 0.0

    println(range.mkString(" , "))
    println(init.mkString(","))
    val eval = ivpsolver.solve(LineRange(range.toArray), init.toArray)
    /*
     * eval returns a lazy object which needs to be executed to get the values
     */
    val result = eval()
    val last = for (stack <- result) yield {
      stack.last
    }

    for ( l<-last) println(l.mkString(","))
    
    result.map(_.show)
  }
}

import com.kabouterlabs.ode.implicits.lsodes.LsodesImplicit._

object Ode2
{
  def main(args:Array[String])
  {

    LogIt().level.trace()
    Ode2Example()
  }

}
