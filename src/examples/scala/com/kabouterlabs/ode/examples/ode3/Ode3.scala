package com.kabouterlabs.ode.examples.ode3

/**
  * Created by fons on 4/3/17.
  */
import com.kabouterlabs.ode.OdeSolver.{OdeSolverTC, _}
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.{ConvertArrayToFortranMatrix, LogIt}
import com.kabouterlabs.ode.{FuncParams, Ivp}

import scala.collection.immutable.Range
import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
/**
  *
  *  Chemical kinetics problem taken from the dlsoda fortran code. This uses a full jacobian which is different from the lsodes example in Ode2
  *
  *  Three rate equations :
  *
  *  dy1/dt  =  - rk1*y1
  *  dy2/dt  =  rk1*y1 + rk11*rk14*y4 + rk19*rk14*y5 - rk3*y2*y3 - rk15*y2*y12 - rk2*y2
  *  dy3/dt  =  rk2*y2 - rk5*y3 - rk3*y2*y3 - rk7*y10*y3 + rk11*rk14*y4 + rk12*rk14*y6
  *  dy4/dt  =  rk3*y2*y3 - rk11*rk14*y4 - rk4*y4
  *  dy5/dt  =  rk15*y2*y12 - rk19*rk14*y5 - rk16*y5
  *  dy6/dt  =  rk7*y10*y3 - rk12*rk14*y6 - rk8*y6
  *  dy7/dt  =  rk17*y10*y12  - rk20*rk14*y7 - rk18*y7
  *  dy8/dt  =  rk9*y10 - rk13*rk14*y8 - rk10*y8
  *  dy9/dt  =  rk4*y4 + rk16*y5 + rk8*y6 + rk18*y7
  *  dy10/dt =  rk5*y3 + rk12*rk14*y6 + rk20*rk14*y7 + rk13*rk14*y8 - rk7*y10*y3 - rk17*y10*y12 - rk6*y10 - rk9*y10
  *  dy11/dt =  rk10*y8
  *  dy12/dt =  rk6*y10 + rk19*rk14*y5 + rk20*rk14*y7 - rk15*y2*y12 - rk17*y10*y12
  *
  *  rk1  = rk5 = 0.1
  *  rk4  = rk8 = rk16 = rk18 = 2.5
  *  rk10 = 5
  *  rk2 = rk6 = 10
  *  rk14 = 30
  *  rk3 = rk7 = rk9 = rk11 = rk12 = rk13 = rk19 = rk20 = 50
  *  rk15 = rk17 = 100
  *
  *  This problem is sparse and stiff.
  *
  */


object Ode3Example {
  def apply[A](constraints:Int=0)(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}): Unit = {

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 12 d problem with 0 constraints (roots we need to satisfy) by default.
      *
     */
    val ivpsolver = Ivp(dim = 12) +
      /*
        * Configure the solver. Notice that Backward Differention (BDF) is selected.
        * The solver will ignore options it does not require, e.g. the soda slover will automtically detect the right differention scheme and will ignore this option.
        *
        * The following options are used :
        *
        *  - Method       : This is the basic linear mutistep method : BDF (backward differentiation) or Adams. The full method flag is determined in combination
        *                   with othet configuration parameters provided, notably the jocobian type.
        *
        *  - JacobianType : In this case a full jacobian is assumed. If the jacobian function is present the suer supplied jobian option will be selected
        *                   in the codepack wrapper. Other options as banded and sparse.
        *
        *  - Tolerances   : The Tolerance class can be used to simply provide the same relative and absolute tolerances. Below the absolute tolerance
        *                    is different for each dependent variable and the relative tolerance is the same
        *
       */
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(0.000001)) +
      /*
        * The parameters used in the functions. This is a standardized way of supplying data to the ode functions and bypasses the way fortran code.
       */
      FuncParams("rk1" -> 0.1,"rk5" -> 0.1,
        "rk4"->2.5,  "rk8"-> 2.5, "rk16" -> 2.5, "rk18"->2.5,
        "rk10" -> 5.0,
        "rk2" -> 10.0, "rk6" -> 10.0,
        "rk14" -> 30.0,
        "rk3" -> 50.0, "rk7" -> 50.0,"rk9" -> 50.0,"rk11" -> 50.0,"rk12" -> 50.0,"rk13" -> 50.0,"rk19" -> 50.0,"rk20" -> 50.0,
        "rk15" -> 100.0, "rk17" -> 100.0
      ) +
      /*
        * This is the ODE callback function.
        *
        * It implements the differential equation.
        * x is the independent variable; y are the depdent variables.
        * ydot are the new values of the differentials
        * Only ydot is marshalled and unmarshalled, so that's the only data set that is allowed to change
       */
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        val rk1  = params->"rk1"
        val rk2  = params->"rk2"
        val rk3  = params->"rk3"
        val rk4  = params->"rk4"
        val rk5  = params->"rk5"
        val rk6  = params->"rk6"
        val rk7  = params->"rk7"
        val rk8  = params->"rk8"
        val rk9  = params->"rk9"
        val rk10 = params->"rk10"
        val rk11 = params->"rk11"
        val rk12 = params->"rk12"
        val rk13 = params->"rk13"
        val rk14 = params->"rk14"
        val rk15 = params->"rk15"
        val rk16 = params->"rk16"
        val rk17 = params->"rk17"
        val rk18 = params->"rk18"
        val rk19 = params->"rk19"
        val rk20 = params->"rk20"


        ydot(0)  = -rk1*y(0)
        ydot(1)  = rk1*y(0) + rk11*rk14*y(3) + rk19*rk14*y(4) - rk3*y(1)*y(2) - rk15*y(1)*y(11) - rk2*y(1)
        ydot(2)  = rk2*y(1) - rk5*y(2) - rk3*y(1)*y(2) - rk7*y(9)*y(2) + rk11*rk14*y(3) + rk12*rk14*y(5)
        ydot(3)  = rk3*y(1)*y(2) - rk11*rk14*y(3) - rk4*y(3)
        ydot(4)  = rk15*y(1)*y(11) - rk19*rk14*y(4) - rk16*y(4)
        ydot(5)  = rk7*y(9)*y(2) - rk12*rk14*y(5) - rk8*y(5)
        ydot(6)  = rk17*y(9)*y(11) - rk20*rk14*y(6) - rk18*y(6)
        ydot(7)  = rk9*y(9) - rk13*rk14*y(7) - rk10*y(7)
        ydot(8)  = rk4*y(3) + rk16*y(4) + rk8*y(5) + rk18*y(6)
        ydot(9)  = rk5*y(2) + rk12*rk14*y(5) + rk20*rk14*y(6) + rk13*rk14*y(7) - rk7*y(9)*y(2) - rk17*y(9)*y(11) - rk6*y(9) - rk9*y(9)
        ydot(10) = rk10*y(7)
        ydot(11) = rk6*y(9) + rk19*rk14*y(4) + rk20*rk14*y(6) - rk15*y(1)*y(11) - rk17*y(9)*y(11)

      }) +
      /*
        * this is the jacobian callback function.
        *  pdr : column id  for which the jacobian is requested
       */
      ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {
        val rk1  = params->"rk1"
        val rk2  = params->"rk2"
        val rk3  = params->"rk3"
        val rk4  = params->"rk4"
        val rk5  = params->"rk5"
        val rk6  = params->"rk6"
        val rk7  = params->"rk7"
        val rk8  = params->"rk8"
        val rk9  = params->"rk9"
        val rk10 = params->"rk10"
        val rk11 = params->"rk11"
        val rk12 = params->"rk12"
        val rk13 = params->"rk13"
        val rk14 = params->"rk14"
        val rk15 = params->"rk15"
        val rk16 = params->"rk16"
        val rk17 = params->"rk17"
        val rk18 = params->"rk18"
        val rk19 = params->"rk19"
        val rk20 = params->"rk20"
        val pdr = ConvertArrayToFortranMatrix(pd)

        pdr(1 ,1 , -rk1)

        pdr(2 ,1 , rk1)
        pdr(2 ,2 , -rk3 * y(2) - rk15 * y(11) - rk2)
        pdr(2 ,3 , -rk3 * y(1))
        pdr(2 ,4 , rk11 * rk14)
        pdr(2 ,5 , rk19 * rk14 )
        pdr(2 ,12, -rk15 * y(1))

        pdr(3 ,2 , rk2 - rk3 * y(2))
        pdr(3 ,3 , -rk5 - rk3 * y(1) - rk7 * y(9))
        pdr(3, 4 , rk11 * rk14)
        pdr(3, 6 , rk12 * rk14  )
        pdr(3, 10, -rk7 * y(2))

        pdr(4 ,2 , rk3 * y(2))
        pdr(4 ,3 , rk3 * y(1))
        pdr(4,4,-rk11 * rk14 - rk4)

        pdr(5 ,2 , rk15 * y(11))
        pdr(5 ,3 , rk7 * y(9))
        pdr(5 ,5 , -rk19 * rk14 - rk16)
        pdr(5,12, rk15 * y(1))

        pdr(6, 6, -rk12 * rk14 - rk8)
        pdr(6, 10, rk7 * y(2))
        //6.3

        pdr(7 ,7 , -rk20 * rk14 - rk18)
        pdr(7 ,10,  rk17 * y(11))
        pdr(7, 12,  rk17 * y(9))
        
        pdr(8, 8,-rk13 * rk14 - rk10)
        pdr(8, 10, rk9)

        pdr(9 ,4 , rk4)
        pdr(9 ,5 , rk16)
        pdr(9 ,6 , rk8)
        pdr(9 ,7 , rk18)

        pdr(10,3 , rk5 - rk7 * y(9))
        pdr(10,6 , rk12 * rk14)
        pdr(10,7 , rk20 * rk14)
        pdr(10,8 , rk13 * rk14)
        pdr(10,10,  -rk7 * y(2) - rk17 * y(11) - rk6 - rk9)
        pdr(10,12, -rk17 * y(9))

        pdr(11, 8, rk10)

        pdr(12,2 , -rk15 * y(11))
        pdr(12,5 , rk19 * rk14)
        pdr(12,7 , rk20 * rk14)
        pdr(12,10, rk6 - rk17 * y(11))
        pdr(12,12, -rk15*y(1) - rk17 * y(9))

      }) +
      /*
        * Settting optional parameters. Those differ per solver but the most common ones have been factored out.
        * In the setting below the diagnostic messages have been suppressed and an initial step size is provided.
        * The optional parameters are highly specific to the fortran ode solver being called.
        * Use the OptionalParameters to pass in iwork or rwark arrays if that's needed.
       */
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.MAX_STEPS, 1000))  +>  //+> is the termintor; this returns the solver

    /*
     * This creates an array of independent variables (time in this case).
     */
    val range = for (index <- Range(0, 6)) yield  {
      if (index == 0) 0.0 else 0.1 * math.pow(10, index-1)
    }
    val init = for (index <- Range(0,12)) yield if (index == 0)  1.0 else 0.0

    println(range.mkString(" , "))
    println(init.mkString(","))
    val eval = ivpsolver.solve(LineRange(range.toArray), init.toArray)
    /*
     * eval returns a lazy object which needs to be executed to get the values
     */
    val result = eval()
    val last = for (stack <- result) yield {
      stack.last
    }

    for ( l<-last) println(l.mkString(","))

    result.map(_.show)
  }
}

import com.kabouterlabs.ode.implicits.lsoda.LsodaImplicit._

object Ode3
{
  def main(args:Array[String])
  {

    LogIt().level.trace()
    Ode3Example()
  }

}

