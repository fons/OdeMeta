package com.kabouterlabs.ode.implicits.gamd

import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.gamd.Gamd
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}

/**
  * Created by fons on 3/9/17.
  */
object GamdImplicit
{


  implicit val ev$OdeSolverFactoryT = new OdeSolverFactoryT[Double] {

    override type OdeSolverT = Gamd

    override type ElemT = Double

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT], m: MassMatrixFuncM[ElemT],
                        d:DaeIndexVariables, p: FuncParams[ElemT], cf: Config): Option[OdeSolverT]
    =   HandleException{Some(new Gamd(dim,f, j, m, d, p, cf))}
    override def create(dim: Int, f:OdeFuncM[ElemT], j:JacobianFuncM[ElemT], p:FuncParams[ElemT], cf:Config): Option[OdeSolverT]
    =  HandleException{Some(new Gamd(dim,f, j, MassMatrixFuncM.none, DaeIndexVariables(), p, cf))}

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT],
                        cc:Int, c: ConstraintFuncM[ElemT], e:EventFuncM[ElemT], p: FuncParams[ElemT], cf :Config): Option[OdeSolverT] = None

  }


  implicit class Ev$OdeRunT(solver:OdeM[Gamd]) extends OdeRunT[Gamd]
  {

    override type ElemT = Double
    override type OdeSolverRetType = Option[StackT]

    override type RangeTypeT = Double

    override def apply(range:LineRangeT[RangeTypeT], init: Array[ElemT]) = new  {

      override def toString: String = super.toString + "lazy result for " + solver.toString + "[" + range.start + ",end " + range.end + " ]{" + init.mkString(",") + "}"

      private lazy val result =  (solver.ode, NonValueChecker(init).hasNonValue) match {
        case (Some(s), false) => s.run(range, init)
        case (Some(s), true)       => {
          LogIt().error("invalid input detected : " + init.mkString(",") + " not proceeding")
          None
        }
        case (_, _) => None
      }

      def apply(): Option[StackT] = result

    }


  }

  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[Gamd]]
  {

    override type SolverDataType = Double

    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                      c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[Gamd] = OdeM(dim,f,j,n,c,e,p,cf)

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[Gamd] = OdeM(dim,f,j,m,d,p,cf)

    override def solve(s: OdeM[Gamd], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = new OdeM[Gamd](None)

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[Gamd] = OdeM(dim,f,j,p,cf)
  }
}
