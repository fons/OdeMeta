package com.kabouterlabs.ode.implicits.lsode

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.jodeint.codepack.CodepackLibrary._
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.odepack.{MethodFlagConfigT, OdePackBasic}
import com.kabouterlabs.ode.config.{Config, DaeIndexVariables, Methods}
import com.kabouterlabs.ode.util.{HandleException, LogIt, NonValueChecker}
import com.kabouterlabs.ode.{OdeM, _}

/**
  * Created by fons on 1/22/17.
  */
object LsodeBasicImplicit
{


  implicit val ev$FactoryT = new OdeSolverFactoryT[Double]
  {
    override type OdeSolverT = OdePackBasic
    override type ElemT = Double

    implicit val ev$MfConfig = new MethodFlagConfigT
    {
      override def set_mf(config:Config):CodepackLibrary.codepack_method_e = config.method match {
        //mf = 10 * meth + miter
        // adams ->1; bdf -> 2; 2-> internally generated jacobian
        case Methods.ADAMS    => CodepackLibrary.codepack_method_e.ADAMS_INTERNAL_FULL_JAC
        case Methods.BDF      => CodepackLibrary.codepack_method_e.BDF_INTERNAL_FULL_JAC
        case _                => CodepackLibrary.codepack_method_e.ADAMS_BASIC
      }
    }

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT], p: FuncParams[ElemT], conf:Config): Option[OdeSolverT]
    = HandleException { Some(OdePackBasic(dim,f,lsode_basic,p,conf))}

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
