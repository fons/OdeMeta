package com.kabouterlabs.ode

import com.kabouterlabs.ode.config.{DaeIndexVariables, Config}

/**
  * Created by fons on 1/5/17.
  */



case class OdeM[U] (ode:Option[U])
{

    def map[W](f:(U)=>W):OdeM[W] = ode match {
        case Some(solver) =>  new OdeM[W](Some(f(solver)))
        case _ => new OdeM[W](None)
    }

}

object OdeM
{

  def apply[U](dim:Int, f:OdeFuncM[U], j: JacobianFuncM[U], n: Int, c:ConstraintFuncM[U], e:EventFuncM[U] ,p: FuncParams[U], cf:Config)(implicit ev$factory:OdeSolverFactoryT[U])
        = new OdeM[ev$factory.OdeSolverT](ev$factory.create(dim, f, j,n, c, e, p, cf))

  def apply[U](dim:Int, f:OdeFuncM[U], j: JacobianFuncM[U], p: FuncParams[U], cf:Config)(implicit ev$factory:OdeSolverFactoryT[U])
                = new OdeM[ev$factory.OdeSolverT](ev$factory.create(dim, f, j, p, cf))

  def apply[U](dim:Int, f:OdeFuncM[U], j: JacobianFuncM[U], m:MassMatrixFuncM[U], d:DaeIndexVariables, p: FuncParams[U],
               cf:Config)(implicit ev$factory:OdeSolverFactoryT[U]) = new OdeM[ev$factory.OdeSolverT](ev$factory.create(dim, f, j, m, d, p, cf))

  def none[U](implicit ev$factory:OdeSolverFactoryT[U]) = new OdeM[ev$factory.OdeSolverT](None)

}