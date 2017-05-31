package com.kabouterlabs.ode.experimental

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 11:17 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.Try

trait OdeStepSolverT
{

  def nextStep(step: Double, x: Double, yargs: List[Double], func: (Double, List[Double]) => List[Double]): (Try[(Double, List[Double])], Option[List[Double]])

  protected def className[A](a: A)(implicit m: Manifest[A]):String = m.toString

  protected def stepSolverName:String = className(this)
}
