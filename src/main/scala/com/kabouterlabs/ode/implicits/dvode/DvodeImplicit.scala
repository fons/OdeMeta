package com.kabouterlabs.ode.implicits.dvode

import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.dvode.Dvode
import com.kabouterlabs.ode.kernel._
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.StackT


/**
  *
  * 
  */
object DvodeImplicit {
  implicit val ev$FactoryT = new OdeSolverFactoryT[Double] {

    override type OdeSolverT = Dvode

    override type ElemT = Double

    override def create(dim: Int, f:OdeFuncM[ElemT], j:JacobianFuncM[ElemT], p:FuncParams[ElemT], cf:Config): Option[OdeSolverT]
    =  HandleException{Some(new Dvode(dim,f, j, p, cf))}

  }


  implicit class Ev$OdeRunT(solver:OdeM[Dvode]) extends OdeRunT[Dvode]
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

  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[Dvode]]
  {

    override type SolverDataType = Double

    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                 c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[Dvode] = OdeM(dim,f,j,n,c,e,p,cf)

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[Dvode] = OdeM(dim,f,j,m,d,p,cf)

    override def solve(s: OdeM[Dvode], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = new OdeM[Dvode](None)

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[Dvode] = OdeM(dim,f,j,p,cf)
  }
}
