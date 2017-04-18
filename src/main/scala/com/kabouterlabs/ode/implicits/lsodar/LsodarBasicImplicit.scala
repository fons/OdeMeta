package com.kabouterlabs.ode.implicits.lsodar

/**
  * Created by fons on 1/16/17.
  */


import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.jodeint.codepack.CodepackLibrary._
import com.kabouterlabs.ode.{OdeM, _}
import com.kabouterlabs.ode.odepack.{MethodFlagConfigT, OdePackBasic}
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables}

import scala.language.reflectiveCalls
import com.kabouterlabs.jodeint.codepack.CodepackLibrary.lsodar_basic
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}


/**
  * Created by fons on 1/6/17.
  */


object LsodarBasicImplicit {


  implicit val ev$FactoryT = new OdeSolverFactoryT[Double]
  {

    implicit val ev$MfConfig = new MethodFlagConfigT {
      override def set_mf(config:Config): codepack_method_e = codepack_method_e.ADAMS_BASIC //it doesn't matter
    }

    override type OdeSolverT = OdePackBasic

    override type ElemT = Double

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT], p: FuncParams[ElemT], cf:Config): Option[OdeSolverT] =
          HandleException { Some(new OdePackBasic(dim,f,lsodar_basic , p))}

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT],
                        cc:Int, c: ConstraintFuncM[ElemT], e:EventFuncM[ElemT], p: FuncParams[ElemT], cf:Config): Option[OdeSolverT] = None


  }


  implicit class Ev$OdeRunT(solver:OdeM[OdePackBasic]) extends OdeRunT[OdePackBasic]
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

  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[OdePackBasic]]
  {

    override type SolverDataType = Double

    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                      c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[OdePackBasic] = OdeM(dim,f,j,n,c,e,p,cf)

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[OdePackBasic] = OdeM(dim,f,j,m,d,p,cf)

    override def solve(s: OdeM[OdePackBasic], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = new OdeM[OdePackBasic](None)

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[OdePackBasic] = OdeM(dim,f,j,p,cf)
  }

}

