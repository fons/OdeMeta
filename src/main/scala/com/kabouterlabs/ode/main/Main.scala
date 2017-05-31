package com.kabouterlabs.ode.main


import com.kabouterlabs.ode.OdeSolver._

import com.kabouterlabs.ode.config._

import com.kabouterlabs.ode.{FuncParams, Ivp}

import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.LogIt

import scala.language.existentials
import scala.language.reflectiveCalls
import scala.language.higherKinds
import scala.language.postfixOps


import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.symplectic.implicits.GniLmm2Implicit._

object Main extends App
{
  var called = 0
  LogIt().level.info()

  val ivpsolver = Ivp(dim = 1) + (Config(Methods.SYMPLECTIC_801_STAGES) -> AbsoluteTolerance(Array[Double]())) +
    ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
      called =  called + 1
      //ydot(0) = y(1)
      //ydot(1) = - math.sin(y(0))
      ydot(0) = - math.sin(y(0))
    }) + OptionalParameters(OptionalParameterType.MIN_STEPS, 10) +>  //+> is the termintor; this returns the solver

  val init =  Array(math.Pi*999.0/1000.0, 0.0)
  val eval = ivpsolver.solve(LineRange(0.0, 10.0, 0.1), init)

  eval().map(_.show)

  println("total number of calls " + called)
  val hamil0 = 0.5 * init(1)*init(1) - math.cos(init(0))

  for (stack <- eval()) yield {
    for (item <- stack.toArray) yield {
      val hamil = 0.5 * item(2)*item(2) - math.cos(item(1))
      val drift = hamil0 - hamil
      println(item.mkString(",") + " hamil : " + hamil + "  drift : " + drift)
    }
  }

  /*
  .000000000000e+00 ,3.138451060936e+00 ,0.000000000000e+00 ,
1.000000000000e-01 ,3.138435297247e+00 ,-3.151027426619e-04 ,
2.000000000000e-01 ,3.138387835329e+00 ,-6.335446780852e-04 ,
3.000000000000e-01 ,3.138308216310e+00 ,-9.581057471176e-04 ,
4.000000000000e-01 ,3.138195738558e+00 ,-1.291856188377e-03 ,
5.000000000000e-01 ,3.138049230871e+00 ,-1.638464611450e-03 ,
6.000000000000e-01 ,3.137867223050e+00 ,-2.001524071964e-03 ,
7.000000000000e-01 ,3.137647898771e+00 ,-2.384721926404e-03 ,
   */
}
