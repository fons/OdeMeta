package com.kabouterlabs.ode.experimental.ode

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/4/13
 * Time: 6:33 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Success, Try}

object ExplicitRungeKutta4 extends OdeStepSolverT {
  //val step : Double
  //val init : (Double, List[Double])
  //, val Func : List[(Double, Double*)=>Double] ) extends com.mhsw.com.github.fons.nr.ode.OdeSolverT {
  private val alfa: List[Double] = List(0, 0.5, 0.5, 1)
  private val beta: List[Double] = List(0, 0.5, 0.5, 1)
  private val Coeff: List[Double] = List(1.0 / 6.0, 2.0 / 6.0, 2.0 / 6.0, 1.0 / 6.0)


  @tailrec private def kcalc(Args: (Double, List[Double]), step: Double, Func: (Double, List[Double]) => List[Double], alfa: List[Double], beta: List[Double], Accum: List[List[Double]]):List[List[Double]] = {
    if ((beta == Nil) || (beta isEmpty)) {
      Accum
    }
    else {
      val (t, args) = Args
      val time_step = t + (alfa head) * step
      val L = Accum.head.map(x => (beta head) * step * x)
      // x_i + beta_i0*h*k0_i etc: new x values
      val arg_step = (args, L).zipped map (_ + _)
      val Kn = Func(time_step, arg_step)
      kcalc(Args, step, Func, (alfa tail), (beta tail), (Kn :: Accum))
    }
  }

  private def next_step(step: Double, Args: (Double, List[Double]), Func: (Double, List[Double]) => List[Double]) = {
    // K is a list of list of K coefficients, each seperate list represens a step in the RK scheme
    val K = kcalc(Args, step, Func, alfa tail, beta tail, List(Func(Args._1, Args._2)))
    // Need to reverse K as the results were consed to the head
    // The coeff list has the factors which are applied to the elements of each step in the scheme
    // Zip the coeff with the K list=> tuple of coeff plus list.
    // Then apply the coeff to each element of the list
    val apply_coeff = (Coeff, K reverse).zipped map ((coeff, list) => list.map(x => coeff * x))
    // now reduce the list of lists by adding each corresponding item in the list
    // k = k1 + k2 + ....
    val results = apply_coeff reduceLeft ((x, y) => (x, y).zipped map ((u, v) => u + v))
    // xi = xi-1 + step * ki
    val next = (Args._2, results).zipped map ((arg0, k) => arg0 + step * k)
    (Args._1 + step, next)
  }

  override def nextStep(Step: Double, x: Double, yargs: List[Double], Func: (Double, List[Double]) => List[Double]): (Try[(Double, List[Double])], Option[List[Double]]) = {
    val res1 = next_step(Step, (x, yargs), Func)
    //half the step size so need to take two steps
    //this should be optimized as previous resukts can be reused.
    val res2 = next_step(Step * 0.5, (x, yargs), Func)
    val res3 = next_step((Step * 0.5), res2, Func)
    val diff = (res1._2, res3._2).zipped map (_ - _)
    return (Success(res3), Some(diff))
  }

//  private def next(Step: Double, Args: (Double, List[Double]), Func: (Double, List[Double]) => List[Double]): Try[(Double, List[Double])] = {
//    val res = next_step(Step, Args, Func)
//    Success(res)
//  }

  override def stepSolverName = className(this) + " extends " + super.stepSolverName + " with hard coded tableau"

}


