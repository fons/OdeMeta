package com.kabouterlabs.ode.examples.pendulum


import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.kernel.OdeSolver._
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.LogIt

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._


/**
  *
  *  
  *
  *
  */


object PendulumExample {
  def apply[A](constraints:Int=0)(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}): Unit = {
    var called = 0
    LogIt().level.info()
    /*
     * Initialize the solver. This is a 2 d problem with 0 constraints (roots we need to satisfy) by default.
      *
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
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(0.000001)) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
          called =  called + 1
         ydot(0) = y(1)
         ydot(1) = - math.sin(y(0))
      }) + OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) +>  //+> is the termintor; this returns the solver


    val eval = ivpsolver.solve(LineRange(0.0, 10.0, 0.1), Array(math.Pi*999.0/1000.0, 0.0))

    eval().map(_.show)

    println("total number of calls " + called)
  }
}

import com.kabouterlabs.ode.implicits.lsodes.LsodesImplicit._

object Pendulum
{
  def main(args:Array[String])
  {

    LogIt().level.trace()
    PendulumExample()
  }

}
