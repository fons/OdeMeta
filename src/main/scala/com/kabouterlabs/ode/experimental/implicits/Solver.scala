package com.kabouterlabs.ode.experimental.implicits

import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.experimental.{BS23, Euler, ExplicitMidPoint, ExplicitRungeKutta4, OdeStepSolverT, RKEmbedded23, RKEmbedded56, RKEmbeddedCashKarp, RKEmbeddedDormandPrince, RKEmbeddedDormandPrince54, RKEmbeddedFehlberg56, RKEmbeddedFehlberg78, RKEmbeddedVerner, RungeKutta42, RungeKuttaWithRk5Tableau, OdeSolver => ExperimentalOdeSolver}
import com.kabouterlabs.ode.kernel._
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.StackT
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}

/**
  * Created by fons on 5/1/17.
  */


class Solver(odeStepSolver: OdeStepSolverT)
{

    implicit val ev$FactoryT = new OdeSolverFactoryT[Double] {
      private def handleTolerance(config: Config): Double = {

        (config.relativeTolerance.get, config.absoluteTolerance.get) match {
          case ((Some(rtolv), None), (Some(atolv), None)) => {
            0.5 * (rtolv + atolv)
          }

          case ((None, Some(rtola)), (None, Some(atola))) => {
            ((0.0 /: rtola) {
              _ + _
            }) / (1.0 + rtola.length) + ((0.0 /: atola) {
              _ + _
            }) / (atola.length + 1.0) * 0.5
          }

          case ((Some(rtolv), None), (None, Some(atola))) => {
            val sum = rtolv + (0.0 /: atola) {
              _ + _
            }
            rtolv + sum / (atola.length + 1.0)
          }

          case ((None, Some(rtola)), (Some(atolv), None)) => {
            (((0.0 /: rtola) {
              _ + _
            }) / (1 + rtola.length) + atolv) * 0.5
          }
        }
      }

      override type OdeSolverT = ExperimentalOdeSolver

      override type ElemT = Double

      override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT], p: FuncParams[ElemT], cf: Config): Option[OdeSolverT] = {
        f.map((func) => new ExperimentalOdeSolver(odeStepSolver, dim, func, p, handleTolerance(cf)))
      }
    }


    implicit class Ev$OdeRunT(solver:OdeM[ExperimentalOdeSolver]) extends OdeRunT[ExperimentalOdeSolver]
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

    implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[ExperimentalOdeSolver]]
    {

      override type SolverDataType = Double

      override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                        c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[ExperimentalOdeSolver] = OdeM(dim,f,j,n,c,e,p,cf)

      override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                   p: FuncParams[Double], cf: Config): OdeM[ExperimentalOdeSolver] = OdeM(dim,f,j,m,d,p,cf)

      override def solve(s: OdeM[ExperimentalOdeSolver], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


      override def none = new OdeM[ExperimentalOdeSolver](None)

      override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                   p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[ExperimentalOdeSolver] = OdeM(dim,f,j,p,cf)
    }


}

case object RKEmbedded56Implicit extends Solver(RKEmbedded56)

case object ExplicitMidPointImplicit  extends Solver(ExplicitMidPoint)

case object RungeKuttaWithRk5TableauImplicit extends Solver(RungeKuttaWithRk5Tableau)

case object RKEmbeddedFehlberg56Implicit extends Solver(RKEmbeddedFehlberg56)

case object RKEmbeddedFehlberg78Implicit extends Solver(RKEmbeddedFehlberg78)

case object RKEmbeddedDormandPrince54Implicit  extends Solver(RKEmbeddedDormandPrince54)

case object RKEmbeddedDormandPrinceImplicit extends Solver(RKEmbeddedDormandPrince)

case object RKEmbeddedVernerImplicit extends  Solver( RKEmbeddedVerner)

case object RKEmbeddedCashKarpImplicit extends  Solver(RKEmbeddedCashKarp)

case object RungeKutta42Implicit extends Solver(RungeKutta42)

case object RKEmbedded23Implcit  extends   Solver(RKEmbedded23)

case object RungeKutta4Implicit extends Solver(ExplicitRungeKutta4)

case object BS23Implicit extends Solver(BS23)

case object EulerImplicit extends Solver(Euler)
