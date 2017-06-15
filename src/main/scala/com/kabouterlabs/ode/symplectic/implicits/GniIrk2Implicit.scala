package com.kabouterlabs.ode.symplectic.implicits

import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.kernel._
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.StackT
import com.kabouterlabs.ode.symplectic.gniirk2.GniIrk2
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}

/**
  * Created by fons on 3/10/17.
  */
object GniIrk2Implicit
{


  implicit val ev$OdeSolverFactoryT = new OdeSolverFactoryT[Double] {

    override type OdeSolverT = GniIrk2

    override type ElemT = Double


    override def create(dim: Int, f:OdeFuncM[ElemT], j:JacobianFuncM[ElemT], p:FuncParams[ElemT], cf:Config): Option[OdeSolverT]
    =  HandleException{Some(new GniIrk2(dim,f, p, cf))}


  }


  implicit class Ev$OdeRunT(solver:OdeM[GniIrk2]) extends OdeRunT[GniIrk2]
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

  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[GniIrk2]]
  {

    override type SolverDataType = Double

    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                      c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[GniIrk2] = OdeM(dim,f,j,n,c,e,p,cf)

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[GniIrk2] = OdeM(dim,f,j,m,d,p,cf)

    override def solve(s: OdeM[GniIrk2], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = new OdeM[GniIrk2](None)

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[GniIrk2] = OdeM(dim,f,j,p,cf)
  }

}
