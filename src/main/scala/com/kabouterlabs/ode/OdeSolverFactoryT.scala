package com.kabouterlabs.ode

import com.kabouterlabs.ode.config.{DaeIndexVariables, Config}
import com.kabouterlabs.ode.util.LogIt


/**
  * Created by fons on 1/6/17.
  */
trait OdeSolverFactoryT[U]
{
  type OdeSolverT
  type ElemT=U

  def create(dim:Int, f:OdeFuncM[U], j:JacobianFuncM[U], p:FuncParams[U], cf:Config):Option[OdeSolverT]
  def create(dim:Int, f:OdeFuncM[U], j:JacobianFuncM[U], constraints:Int, c:ConstraintFuncM[U], e:EventFuncM[U], p:FuncParams[U], cf:Config):Option[OdeSolverT] = {
    LogIt().warn("constraints not supported so no solver instantiated")
    None
  }
  def create(dim: Int, f: OdeFuncM[U], j: JacobianFuncM[U], m:MassMatrixFuncM[U], d:DaeIndexVariables, p: FuncParams[U], cf: Config): Option[OdeSolverT]  = {
    LogIt().warn("dao is not supported so no version instantiated")
    None
  }
}
