package com.kabouterlabs.ode.main

import com.kabouterlabs.ode.linerange.LineRange

import scala.math.{acos,cos}

object Main extends App
{
  //import com.kabouterlabs.ode.implicits.lsodes.LsodesImplicit._
  //import com.kabouterlabs.ode.kernel.OdeSolver.OdeSolverTC
  import com.kabouterlabs.ode.kernel.OdeSolver._
  import com.kabouterlabs.ode.config._
  import com.kabouterlabs.ode.{FuncParams, Ivp}
  import com.kabouterlabs.ode.linerange.LineRange
  import com.kabouterlabs.ode.util.LogIt

  import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}
  import com.kabouterlabs.ode.implicits.OdeImplicits._

  import com.kabouterlabs.ode.implicits.lsodes.LsodesImplicit._


  val ivpsolver = Ivp(dim = 2)
  val config = (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(0.000001))
  val option = OptionalParameters(OptionalParameterType.DIAGNOSTICS, false)
  val func = (dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
    ydot(0) = y(1)
    ydot(1) = -math.sin(y(0))
  }
  //+  +  +  +>  //+> is the termintor; this returns the solver
  //
  val ivps = ivpsolver + config + option + func


  val eval =  (for (index <- Range(1, 9)) yield {
    println("===>", index ,index * 0.1 * Math.PI)
    (ivps +>).solve(LineRange(0.0, 0.2, 0.1), Array(index * 0.1 * Math.PI, 0.0))().get.toArray.map((x) => (index, x(0), x(1), x(2)))
  }).toArray
  val nval = eval.drop(0).foldLeft(eval(0))((accum, x) => accum ++ x)
  println(nval.mkString(","))
  //eval().map(_.toFile("/tmp/pend.txt"))

  //val data = eval().get.toArray.map((x)=>(x(0), acos(cos(x(1))), x(2)))
  //println(data(0))
}
