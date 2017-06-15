package com.kabouterlabs

/**
  * = OdeMeta =
  *
  * OdeMeta provides solvers for Ordinary Differential Equations (ODE) and Differential Algebraic Equations.
  * It does so by using JavaOdeInt which provides an interface to well known Fortran solvers.
  *
  *
  * == Overview ==
  *
  * The following Fortan solvers are supported :
  *
  * Ode solvers :
  *  - odepack
  *  - vode
  *  - dopri5
  *  - dop853
  *  - rkf45
  *
  *
  * DAO solvers :
  *
  *  - radau5
  *  - bimd
  *  - gamd
  *
  * Symplectic integration :
  *
  *  - gnicodes
  *
  * The follwoing experimental pure scala solvers are provided :
  *
  *  - Various Runge Kutta methods, embeded or otherwise
  *  - Symplectic Implicit Runge Kutta (reimplementation of gni irk2 in Scala)
  *
  * == Usage ==
  *
  * Each solver has an associated implicit.
  *
  * The implicit object implements an ODE type class.
  *
  * The same interface is used for all solvers, accounting for differences in the underlying Fortran interface.
  *
  * You should be able to use all solvers interchangeably, but the solver may disregard some of the options you provide.
  *
  * Have a look at the various examples
  *
  * == Synopsis ==
  *
  * {{{
  * import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
  * import com.kabouterlabs.ode.OdeSolver._
  * import com.kabouterlabs.ode.config._
  * import com.kabouterlabs.ode.{FuncParams, Ivp}
  * import com.kabouterlabs.ode.linerange.LineRange
  * import com.kabouterlabs.ode.util.{ConvertArrayToFortranMatrix, LogIt}
  * *
 import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
  * import com.kabouterlabs.ode.implicits.OdeImplicits._
  * *
import scala.collection.immutable.Range
  * /**
  *
  *  Chemical kinetics problem taken from the dlsoda and dlsode fortran code.
  *
  *  Three rate equations :
  *
  *  dy1/dt = -0.04 y1 + 1.0e4 y2 * y3
  *  dy2/dt = 0.04 * y1 - 1.0e4 * y2 * y3 - 3.0e7 * y2 * y2
  *  dy3/dt = 3.0e7 * y2 * y2
  *
  *  This problem is stiff.
  *
  * */
  * *
  *
object Ode1Example {
  * def apply[A](constraints:Int=0)(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}): Unit = {
  *
  *     LogIt().level.info()
  *
  * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
  * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
  *
  *  val ivpsolver = Ivp(dim = 3, constraints=constraints) +
  *
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
  *
  *    (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(Array(0.000000001, 0.000000000000001, 0.000001))) +
  *
  * The parameters used in the functions. This is a standardized way of supplying data to the ode functions and bypasses the way fortran code.
  *
  *    FuncParams("alpha" -> -0.04, "beta" -> "1.0e4".toDouble, "gamma" -> "3.0e7".toDouble) +
  *
  * This is the ODE callback function.
  *
  * It implements the differential equation.
  * x is the independent variable; y are the depdent variables.
  * ydot are the new values of the differentials
  * Only ydot is marshalled and unmarshalled, so that's the only data set that is allowed to change
  *
  * ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
  * ydot(0) = (params -> "alpha") * y(0) + (params -> "beta") * y(1) * y(2)
  * ydot(2) = (params -> "gamma") * y(1) * y(1)
  * ydot(1) = -ydot(0) - ydot(2)
  * }) +
  *
  * this is the jacobian callback function.  Returns the Jacobian in column order in Array pd;
  * Also shown is the use of the ConvertArrayToFortranMatrix
  * This enables fortran like indexing (1 based) into the array pd
  * ConvertArrayToMatrix uses 0 based indexing
  *
  * ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {
  * val alpha = params -> "alpha"
  * val beta = params -> "beta"
  * val gamma = params -> "gamma"
  * val converted = ConvertArrayToFortranMatrix(pd)
  * converted(1,1,alpha)
  * converted(1,2, beta*y(2))
  * converted(1,3, beta * y(1))
  * *
converted(2,1, -alpha)
  * //(2,2) => see below
  * converted(2,3, -beta * y(1))
  * *
converted(3,1, 0.0)
  * converted(3,2, 2.0 * gamma * y(1))
  * converted(3,3,0.0)
  * *
converted(2,2, -converted(1,2) - converted(3,2))
  * *
  *
//        pd(0) = alpha
  * //        pd(1) = -alpha
  * //        pd(2) = 0.0
  * //
  * //   pd(3) = beta * y(2)
  * //        pd(4) = -beta * y(2) - 2.0 * gamma * y(1)
  * //        pd(5) = 2.0 * gamma * y(1)
  * //
  * // pd(6) = beta * y(1)
  * //        pd(7) = -1.0 * beta * y(1)
  * //        pd(8) = 0.0
  * *
}) +
  *
  * Constraint callback function to find roots by sodar.
  * gout are the constraint fucntions and the determine the zeros'
  *
  * ((dim:Int, x:Double, y:Array[Double], ng:Int, gout:Array[Double], params: FuncParams[Double]) => {
  * gout(0) = y(0) - 0.0001
  * gout(1) = y(2) - 0.01
  * }) +
  *
  * Event function called when roots are found
  * The array of dependent variables can be changed here.
  *
  * ((dim: Int, x:Double, y:Array[Double], ng:Int, jroot:Array[Int], params: FuncParams[Double])=>{
  * *
println("root found : " + x + " " + y.mkString(" , ") + " add jroot : [ " + jroot.mkString(" , ") + " ]")
  * *
}) +
  *
  * Settting optional parameters. Those differ per solver but the most common ones have been factored out.
  * In the setting below the diagnostic messages have not been suppressed and an initial step size is provided.
  * The optional parameters are highly specific to the fortran ode solver being called.
  * Use the OptionalParameters to pass in iwork or rwark arrays if that's needed.
  *
  *   (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +>  //+> is the termintor; this returns the solver
  *
  *
  * This creates an array of independent variables (time in this case).
  *
  *  val linerange = for (index <- Range(0, 13)) yield  {
  *    if (index == 0) 0.0 else 0.04 * math.pow(10.0, index)
  *  }
  *
  *  val eval = ivpsolver.solve(LineRange(linerange.toArray), Array(1.0, 0.0, 0.0))
  *
  * eval returns a lazy object which needs to be executed to get the values
  *
  * val result = eval()
  * val last = for (stack <- result) yield {
  *stack.last
  * }
  * *
println(last)
  *result.map(_.show)
  * }
  * }}}
  *
  *
  *
  */
package object ode {

}
