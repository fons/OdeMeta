package com.kabouterlabs.ode.kernel

import com.kabouterlabs.ode._
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.StackT

/**
  * Created by fons on 3/15/17.
  */
object OdeSolver {

  trait OdeSolverTC[A] {
    type SolverDataType 

    def createIvpEventSolver(dim: Int, f: OdeFuncM[SolverDataType], j: JacobianFuncM[SolverDataType], n: Int, c: ConstraintFuncM[SolverDataType],
                        e: EventFuncM[SolverDataType], p: FuncParams[SolverDataType], cf: Config): A

    def createIvpSolver(dim: Int, f: OdeFuncM[SolverDataType], j: JacobianFuncM[SolverDataType], p: FuncParams[SolverDataType], cf: Config): A

    def createDaeSolver(dim: Int, f: OdeFuncM[SolverDataType], j: JacobianFuncM[SolverDataType], m: MassMatrixFuncM[SolverDataType],
                        d: DaeIndexVariables, p: FuncParams[SolverDataType], cf: Config): A

    def none: A

    def solve(s: A, range: LineRangeT[SolverDataType], init: Array[SolverDataType]): {def apply(): Option[StackT]}
    
  }

  implicit class OdeSolverSelfOps$[A](solver: A)(implicit ev1: OdeSolverTC[A]{type SolverDataType=Double}) {
    type SolverDataType = OdeSolverTC[A]#SolverDataType
    
    def solve(range: LineRangeT[ev1.SolverDataType], init: Array[ev1.SolverDataType]) = ev1.solve(solver, range, init)

  }

  def solve[A](lhs: A, range: LineRangeT[Double], init: Array[Double])(implicit ev: OdeSolverTC[A] {type SolverDataType = Double}) = lhs.solve(range, init)

}