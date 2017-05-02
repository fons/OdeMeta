package com.kabouterlabs.ode.experimental.ode

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 10/28/13
 * Time: 6:17 PM
 *
 * com.mhsw.com.github.fons.nr.ode.BS23 algorithm; see MATLAB Chapter 7 Ordinary Differential Equations
 *
 */


import scala.util.{Success, Try}


object BS23 extends OdeStepSolverT {

  private val alfa: List[Double] = List(0, 1.0 / 2.0, 3.0 / 4.0)
  private val beta: List[Double] = List(0, 1.0 / 2.0, 3.0 / 4.0)
  private val Coeff: List[Double] = List(2.0 / 9.0, 3.0 / 9.0, 4.0 / 9.0)
  private val err: List[Double] = List(-5.0 / 72.0, 6.0 / 72.0, 8.0 / 72.0, -9.0 / 72.0)


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
      kcalc(Args, step, Func, alfa tail, beta tail, Kn :: Accum)
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
    ((Args._1 + step, next), K)
  }

  private def error(step: Double, Klist: List[List[Double]], err: List[Double]): Option[List[Double]] = {

    val apply_coeff = (err, Klist reverse).zipped map ((coeff, list) => list.map(x => coeff * x * step))
    val results = apply_coeff reduceLeft ((x, y) => (x, y).zipped map ((u, v) => (u + v)))
    //println("errors :", results)
    Some(results)
  }



  override def nextStep(step: Double, x: Double, yargs: List[Double], Func: (Double, List[Double]) => List[Double]): (Try[(Double, List[Double])], Option[List[Double]]) = {
    //println("with error...")
    next_step(step, (x, yargs), Func) match {
      case (res, klist) => (Success(res), error(step, Func(res._1, res._2) :: klist, err))
    }
  }

  override def stepSolverName = className(this) + " extends " + super.stepSolverName + " with hard coded tableau"
}

