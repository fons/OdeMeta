package com.kabouterlabs.ode.implicits.rkf

import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.kernel._
import com.kabouterlabs.ode.util.HandleException
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.rkf.Dop853Basic
import com.kabouterlabs.ode.stack.StackT

/**
  * Created by fons on 3/2/17.
  */
object Dop853BasicImplicit
{
  implicit val ev$FactoryT = new OdeSolverFactoryT[Double] {
    override type OdeSolverT = Dop853Basic
    override type ElemT = Double


    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT], p: FuncParams[ElemT], cf: Config): Option[OdeSolverT]
    = HandleException {
      Some(new Dop853Basic(dim, f, p, cf))
    }

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT],
                        cc: Int, c: ConstraintFuncM[ElemT], e: EventFuncM[ElemT], p: FuncParams[ElemT], cf: Config): Option[OdeSolverT] = None


  }


  implicit class Ev$OdeRunT(solver: OdeM[Dop853Basic]) extends OdeRunT[Dop853Basic] {

    override type ElemT = Double
    override type OdeSolverRetType = Option[StackT]

    override type RangeTypeT = Double

    override def apply(range: LineRangeT[RangeTypeT], init: Array[ElemT]) = new {

      override def toString: String = super.toString + "lazy result for " + solver.toString + "[" + range.start + ",end " + range.end + " ]{" + init.mkString(",") + "}"

      private lazy val result = solver.ode match {
        case Some(s) => s.run(range, init)
        case _ => None
      }

      def apply(): Option[StackT] = result

    }
  }

  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[Dop853Basic]]
  {

    override type SolverDataType = Double

    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                      c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[Dop853Basic] = OdeM(dim,f,j,n,c,e,p,cf)

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[Dop853Basic] = OdeM(dim,f,j,m,d,p,cf)

    override def solve(s: OdeM[Dop853Basic], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = new OdeM[Dop853Basic](None)

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[Dop853Basic] = OdeM(dim,f,j,p,cf)
  }
}
