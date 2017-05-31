package com.kabouterlabs.ode.experimental

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 10/23/13
 * Time: 9:18 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.util.{Failure, Success, Try}

object Euler extends OdeStepSolverT {
  private def next(Step: Double, time:Double, args:List[Double], Func: (Double, List[Double]) => List[Double]): Try[(Double, List[Double])] = {
    val delta = Func(time, args.map(_ * Step))
    val newargs = (args, delta).zipped map (_ + _)
    Success((time + Step, newargs))
  }

  def nextStep(Step: Double, x: Double, yargs: List[Double], Func: (Double, List[Double]) => List[Double]): (Try[(Double, List[Double])], Option[List[Double]]) = {

    val res1 = next(Step, x, yargs, Func)
    val res2 = next(Step * 0.5, x,yargs, Func)
    val res3 = res2.flatMap((x) => next(Step * 0.5, x._1, x._2 , Func))
    val diff = res1 match {
      case Failure(f) => None
      case Success(r1) => {
        res3 match {
          case Failure(g) => None
          case Success(r3) => Some((r1._2, r3._2).zipped map (_ - _))
        }
      }
    }
    (res3, diff)
  }
  

  override def stepSolverName = className(this) + " extends " + super.stepSolverName + " (no tableau)"
}

