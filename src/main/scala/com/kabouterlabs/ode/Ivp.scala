package com.kabouterlabs.ode


import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables, OptionalParameters}
import com.kabouterlabs.ode.util.LogIt

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}

/**
  * Created by fons on 1/19/17.
  */


class Ivp[U,A](val dim:Int,
               val func:OdeFuncM[U],
               val jac:JacobianFuncM[U],
               val constraints:Option[Int],
               val con:ConstraintFuncM[U],
               val ev:EventFuncM[U],
               val params:FuncParams[U],
               val config:Config,
               val daeIndex:DaeIndexVariables,
               val mass:MassMatrixFuncM[U],
               val opt:OptionalParameters)(implicit ev$1:OdeSolverTC[A]{type SolverDataType=U})
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

  def +(daoIndex1:DaeIndexVariables) =  new Ivp(dim, func, jac, constraints, con,ev, params, config, daoIndex1, mass, opt)
  def +(opt1:OptionalParameters)     = new Ivp(dim, func, jac, constraints, con, ev, params, config.addOpt(opt1), daeIndex, mass, opt1)


  def +> = apply()

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

object Ivp
{
  def apply[U,A] (dim:Int)(implicit ev$1:OdeSolverTC[A]{type SolverDataType=U}) =  new Ivp[U,A](dim, OdeFuncM.none, JacobianFuncM.none, None, ConstraintFuncM.none, EventFuncM.none,
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


