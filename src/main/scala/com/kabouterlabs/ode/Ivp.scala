package com.kabouterlabs.ode


import com.kabouterlabs.ode.config.{Config, DaeIndexVariables, OptionalParameters}
import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.kernel._
import com.kabouterlabs.ode.util.LogIt

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}

/** Ivp is the collection of inputs to the ode solver
  * 
  * @constructor  Use the companion object to create an instance
  *
  * New inputs can be added using the + operator.
  *
  * @example Use the Ivp in conjunction with [[com.kabouterlabs.ode.implicits.OdeImplicits]]
  *          
  *          {{{
  *  // for convenient conversions from lambda's to classes
  *  import com.kabouterlabs.ode.implicits.OdeImplicits._
  *
             *  // import ODE implicit
  *  import com.kabouterlabs.ode.implicits.dvode.DvodeImplicit._
  *
  *  [...]
  *
  *  val alpha = -0.04
  *  val beta  = "1.0e4".toDouble
  *  val gamma =  "3.0e7".toDouble
  *  val params =  FuncParams +\ ("alpha" -> alpha, "beta" -> beta, "gamma" -> gamma)
  *
  *  val w = Ivp(dim=3, constraints=0) +
  *           (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.000000001)) +
  *           FuncParams("alpha" -> -0.04, "beta" -> "1.0e4".toDouble, "gamma" -> "3.0e7".toDouble ) +
  *    ( (dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params:FuncParams[Double]) => {
  *      ydot(0) = (params -> "alpha") * y(0) + (params -> "beta") * y(1) * y(2)
  *      ydot(2) = (params -> "gamma") * y(1)*y(1)
  *      ydot(1) = -ydot(0) - ydot(2)
  *    } ) +
  *           ((dim:Int, x:Double, y:Array[Double], mul:Int, mpk:Int, pd:Array[Double], pdr:Int, params:FuncParams[Double])=> {
  *    val alpha  = params -> "alpha"
  *    val beta   = params -> "beta"
  *    val gamma  = params -> "gamma"
  *    pd(0) = alpha
  *    pd(1) = -alpha
  *    pd(2) = 0.0
  *    pd(3) = beta * y(2)
  *    pd(4) = -beta*y(2) - 2.0 * gamma * y(1)
  *    pd(5) = 2.0 * gamma * y(1)
  *    pd(6) = beta * y(1)
  *    pd(7) = -1.0 * beta * y(1)
  *    pd(8) = 0.0
  *  } ) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.000001) ) +>
  *
  *  w.solve(LineRange(0.0, 4.0, 0.1), Array(1.0, 0.0,0.0,0.0))().map(_.show)
  *          }}}
  *
  */


case class Ivp[U,A](private val dim:Int,
               private val func:OdeFuncM[U],
               private val jac:JacobianFuncM[U],
               private val constraints:Option[Int],
               private val con:ConstraintFuncM[U],
               private val ev:EventFuncM[U],
               private val params:FuncParams[U],
               private val config:Config,
               private val daeIndex:DaeIndexVariables,
               private val mass:MassMatrixFuncM[U],
               private val opt:OptionalParameters)(implicit ev$1:OdeSolverTC[A]{type SolverDataType=U})
{
 
  def +(func1:OdeFuncM[U]) = new Ivp(dim, func1, jac, constraints, con,ev,params,config, daeIndex, mass, opt)

  def +(jac1:JacobianFuncM[U]) = new Ivp(dim, func, jac1, constraints, con,ev,params,config, daeIndex, mass, opt)
  def +(con1:ConstraintFuncM[U]) = constraints match {
      case Some(_) => new Ivp(dim, func, jac, constraints, con1, ev, params,config, daeIndex, mass, opt)
      case None    => new Ivp(dim, func, jac, constraints, ConstraintFuncM.none, ev, params,config, daeIndex, mass, opt)
    }

  def +(ev1:EventFuncM[U]) =  constraints match {
    case Some(_) => new Ivp(dim, func, jac, constraints, con, ev1, params,config, daeIndex, mass, opt)
    case None    => new Ivp(dim, func, jac, constraints, ConstraintFuncM.none, EventFuncM.none, params,config, daeIndex, mass, opt)
  }

  def +(mass1:MassMatrixFuncM[U]) = new Ivp(dim, func, jac, constraints, con,ev,params,config, daeIndex, mass1, opt)

  def +(params1:FuncParams[U])  = new Ivp(dim, func, jac, constraints, con,ev,params1, config, daeIndex, mass, opt)

  def +(config1:Config) = new Ivp(dim, func, jac, constraints, con,ev, params, config1, daeIndex, mass, opt)

  def +(daeIndex1:DaeIndexVariables) =  new Ivp(dim, func, jac, constraints, con,ev, params, config, daeIndex1, mass, opt)
  def +(opt1:OptionalParameters)     = new Ivp(dim, func, jac, constraints, con, ev, params, config.addOpt(opt1), daeIndex, mass, opt1)

  /**
    *
    * @return an instance of the ODE solver whose implicit type class implementation is in scope
    */
  def +> = apply()

  /**
    *
    * @return  an instance of the ODE solver whose implicit type class implementation is in scope
    */
  def apply() = {
    LogIt().trace("Instantiating ode : func : " + func + " constraints : " + constraints + "dao index " + daeIndex)
    (func, constraints, daeIndex) match {

      case (OdeFuncM(None), _, _) => {
        LogIt().error("Unable to instantiate an solver without a function : " + func + " constraints : " + constraints + " DaoIndex : " + daeIndex)
        ev$1.none
      }
      case (OdeFuncM(Some(_)), None, DaeIndexVariables(None, None, None)) => ev$1.createIvpSolver(dim, func, jac, params, config)
      case (OdeFuncM(Some(_)), Some(0), DaeIndexVariables(None, None, None)) => ev$1.createIvpSolver(dim, func, jac, params, config)
      case (OdeFuncM(Some(_)), Some(constr), DaeIndexVariables(None, None, None)) => ev$1.createIvpEventSolver(dim, func, jac, constr, con, ev, params, config)
      case (OdeFuncM(Some(_)), _, DaeIndexVariables(Some(_), _, _)) => {
        LogIt().info("instantiating dae instance with " +  daeIndex)
        ev$1.createDaeSolver(dim, func, jac, mass, daeIndex, params, config)
      }
      case (_, _, _) => {
        LogIt().error("Unable to instantiate an solver with function : " + func + " constraints : " + constraints + " DaoIndex : " + daeIndex)
        ev$1.none
      }
    }
  }
}

/** Factory for [[com.kabouterlabs.ode.Ivp]] instances
  *
  */
object Ivp
{

  /**
    *
    * @param dim    : dimension of the problem
    * @param ev     : implicit instance of the Ode Solver. Instances can be e.g. in com.kabouterlabs.ode.implicits
    * @tparam U     : numeric data type
    * @tparam A     : type of the solver
    * @return       : Ivp instance
    */
  def apply[U,A] (dim:Int)(implicit ev:OdeSolverTC[A]{type SolverDataType=U}) =  new Ivp[U,A](dim, OdeFuncM.none, JacobianFuncM.none, None, ConstraintFuncM.none, EventFuncM.none,
                                  FuncParams.empty, Config(), DaeIndexVariables(), MassMatrixFuncM.none, OptionalParameters())

  def apply[U,A] (dim:Int, constraints:Int)(implicit ev$1:OdeSolverTC[A]{type SolverDataType=U}) = {
    val (newdim, newconstraints) = (dim, constraints) match {
      case (d, n) if d > 0 && n> -1 => (d, Some(n))
      case (d,_) if d > 0 => {
        LogIt().error("unable to intialize with constraints : " + constraints + " setting to 0")
        (d, None)
      }
      case (_,_)          => {
        LogIt().error("unable to intialize with dim : " + dim + " and constraints : " + constraints + " setting dim -100000 (this will blow things up) and constraints to 0")
        (-10000, None)
      }
    }

    new Ivp[U, A] (newdim, OdeFuncM.none, JacobianFuncM.none,
      newconstraints, ConstraintFuncM.none, EventFuncM.none, FuncParams.empty,
      Config (), DaeIndexVariables (), MassMatrixFuncM.none, OptionalParameters () )
  }

}


