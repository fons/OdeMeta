package com.kabouterlabs.ode.implicits.rkf

import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.util.HandleException
import com.kabouterlabs.ode.{OdeM, _}
import com.kabouterlabs.ode.rkf.Rkf45

/**
  * Created by fons on 3/2/17.
  */
object Rkf45Implicit
{
  implicit val ev$FactoryT = new OdeSolverFactoryT[Double] {

    override type OdeSolverT = Rkf45

    override type ElemT = Double

    override def create(dim: Int, f:OdeFuncM[ElemT], j:JacobianFuncM[ElemT], p:FuncParams[ElemT], cf:Config): Option[OdeSolverT]
    =  HandleException{Some(new Rkf45(dim,f, p, cf))}

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT],
                        cc:Int, c: ConstraintFuncM[ElemT], e:EventFuncM[ElemT], p: FuncParams[ElemT], cf :Config): Option[OdeSolverT] = None

  }


  implicit class Ev$OdeRunT(solver:OdeM[Rkf45]) extends OdeRunT[Rkf45]
  {

    override type ElemT = Double
    override type OdeSolverRetType = Option[StackT]

    override type RangeTypeT = Double

    override def apply(range:LineRangeT[RangeTypeT], init: Array[ElemT]) = new  {

      override def toString: String = super.toString + "lazy result for " + solver.toString + "[" + range.start + ",end " + range.end + " ]{" + init.mkString(",") + "}"
      private lazy val result =  solver.ode match {
        case Some(s) => s.run(range, init)
        case _       => None
      }

      def apply(): Option[StackT] = result

    }


  }
  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[Rkf45]]
  {

    override type SolverDataType = Double

    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                      c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[Rkf45] = OdeM(dim,f,j,n,c,e,p,cf)

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[Rkf45] = OdeM(dim,f,j,m,d,p,cf)

    override def solve(s: OdeM[Rkf45], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = new OdeM[Rkf45](None)

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[Rkf45] = OdeM(dim,f,j,p,cf)
  }

}
