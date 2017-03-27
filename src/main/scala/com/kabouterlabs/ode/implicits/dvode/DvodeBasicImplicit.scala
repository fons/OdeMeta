package com.kabouterlabs.ode.implicits.dvode

//import com.kabouterlabs.jodeint.codepack.CodepackLibrary._
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.config.{DaeIndexVariables, Config}
import com.kabouterlabs.ode.dvode.DvodeBasic
import com.kabouterlabs.ode.util.HandleException
import com.kabouterlabs.ode._


/**
  * Created by fons on 2/28/17.
  */
object DvodeBasicImplicit
{
  implicit val ev$FactoryT = new OdeSolverFactoryT[Double]
  {
    override type OdeSolverT = DvodeBasic
    override type ElemT = Double


    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT], p: FuncParams[ElemT], cf:Config): Option[OdeSolverT]
    = HandleException { Some(new DvodeBasic(dim,f,p,cf))}

    override def create(dim: Int, f: OdeFuncM[ElemT], j: JacobianFuncM[ElemT],
                        cc:Int, c: ConstraintFuncM[ElemT], e:EventFuncM[ElemT], p: FuncParams[ElemT], cf:Config): Option[OdeSolverT] = None


  }


  implicit class Ev$OdeRunT(solver:OdeM[DvodeBasic]) extends OdeRunT[DvodeBasic]
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

  implicit object Ev$OdeSolverTC extends OdeSolverTC[OdeM[DvodeBasic]]
  {

    override type SolverDataType = Double
    private lazy val none_ = new OdeM[DvodeBasic](None)


    override def createIvpEventSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], n: Int,
                                 c: ConstraintFuncM[Double], e: EventFuncM[Double], p: FuncParams[Double], cf: Config): OdeM[DvodeBasic] = none_

    override def createDaeSolver(dim: Int, f: OdeFuncM[Double], j: JacobianFuncM[Double], m: MassMatrixFuncM[Double], d: DaeIndexVariables,
                                 p: FuncParams[Double], cf: Config): OdeM[DvodeBasic] = none_

    override def solve(s: OdeM[DvodeBasic], range: LineRangeT[Ev$OdeSolverTC.SolverDataType], init: Array[Ev$OdeSolverTC.SolverDataType]) = s(range, init)


    override def none = none_

    override def createIvpSolver(dim: Int, f: OdeFuncM[Ev$OdeSolverTC.SolverDataType], j: JacobianFuncM[Ev$OdeSolverTC.SolverDataType],
                                 p: FuncParams[Ev$OdeSolverTC.SolverDataType], cf: Config): OdeM[DvodeBasic] = OdeM(dim,f,j,p,cf)
  }
}
