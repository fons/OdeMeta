package com.kabouterlabs.ode.experimental

import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.{StackDouble, StackT}
import com.kabouterlabs.ode.FuncParams
import com.kabouterlabs.ode.util.{HandleException, LogIt}

import scala.annotation.tailrec


/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 10:29 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Failure, Success, Try}
private object AdaptiveStepTransition extends Enumeration {
  type RunState = Value
  val HALT, CONTINUE, RETRY = Value
}

private case class AdaptiveStep(accuracy:Double)
{
  private val accuracy_ = if (accuracy == 0) 1.0e-16 else accuracy

  private val safety = 0.90
  private val pgrow = -0.20
  private val pshrink = -0.25
  private val maxdelta = 0.1
  private val maxgrow = 5
  private val errcon = scala.math.pow((5.0 / safety), (1.0 / pgrow))

  private def increaseStep(ratio: Double, step: Double) = {

    val new_step = if (ratio > errcon) safety * step * scala.math.pow(ratio, pgrow) else 5.0 * step
    //println("increase step" , new_step)
    new_step
  }

  private def decreaseStep(ratio: Double, step: Double) = {
    //println("decrease step")
    val hn = safety * step * scala.math.pow(ratio, pshrink)
    if (step > 0) scala.math.max(hn, 0.1 * step) else scala.math.min(hn, 0.1 * step)
  }

  private def reviseStepSize(new_step_size: Double, target: Double, newx:Double) = {
    val revised_step_size = if (newx + new_step_size > target) (target - newx) else new_step_size
    //println("==>continuing with revised step size ", revised_step_size, newx, target)
    revised_step_size
  }

  private def calcStepSizeFromErrorsEstimates(target:Double, newx:Double, step: Double, errors: List[Double]): (AdaptiveStepTransition.Value, Double) = {
    //println("errors : " + errors)
    val maxerr = errors reduceLeft ((x, y) => if (x > y) x else y)
    //println("current step size : ", step)
    //println("maxerr :" + maxerr.toString + "  required : " + accuracy.toString)
    val ratio = scala.math.abs(maxerr / accuracy_)
    //println("ratio :", ratio)
    if (ratio <= 1.0) (AdaptiveStepTransition.CONTINUE, reviseStepSize(increaseStep(ratio, step), target , newx) )
    else {
      (AdaptiveStepTransition.RETRY, decreaseStep(ratio, step))
    }
  }

  def nextStepSize(target:Double,
                   xPrev: Double,
                   yargsPrev: List[Double],
                   x:Double,
                   yargs: List[Double],
                   step: Double,
                   errors: Option[List[Double]]): (AdaptiveStepTransition.Value, Double) = {
    // without errors there is not much to do but to continue..
    //
    errors match {
      case Some(errs) => calcStepSizeFromErrorsEstimates(target, x, step, errs)
      case None => (AdaptiveStepTransition.CONTINUE, step)
    }
  }
}

private case class FuncConverter(dim:Int, func:(Int, Double, Array[Double], Array[Double], FuncParams[Double])=>Unit, params:FuncParams[Double])
{
  private val arr:Array[Double] = Array.ofDim[Double](dim)
  def apply():(Double, List[Double])=>List[Double] = (x:Double, y:List[Double]) => {
    func(dim, x, y.toArray, arr, params)
    arr.toList
  }

}

case class OdeSolver(odeSolver: OdeStepSolverT, dim:Int, Func: (Double, List[Double]) => List[Double], accuracy: Double)
{
  
  def this(odeSolver: OdeStepSolverT, dim:Int, f:(Int, Double, Array[Double], Array[Double], FuncParams[Double])=>Unit, params:FuncParams[Double], accuracy:Double) {
    this(odeSolver, dim, FuncConverter(dim, f, params)(), accuracy )
  }

  private object Memoize {
    private var list: Vector[(Double, List[Double])] = Vector[(Double, List[Double])]()

    def toList = list.toList

    def store(x: Double, yargs: List[Double]) = {
      list = list :+ (x, yargs)
      list
    }
    
    //lazy val memoizeString = "MemoizeImplT@" + hashCode().toString + "@entries:" + (list length).toString
  }


  private val adaptiveStep = AdaptiveStep(accuracy)
  private val max_retry = 100
  private val step_offset = 1.0E-16 //should be minimum /smallest Double -1
  private def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  private def reached(Current: Double, Target: Double): Boolean = {
    Current + step_offset >= Target
  }

  // for now only check to see if the dependend variable is stuck
  private def trapped(xPrev: Double, yargsPrev: List[Double], x: Double, yargs: List[Double]) = {

    //val  c1 = (old_results, new_results).zipped map ((x:Double, y:Double)=> (math.abs(x - y) > step_offset))
    //val  crit2 = c1.foldLeft(true)(_ && _)
    //println("==> trapped : " ,crit1, old_results, new_results)

    math.abs(xPrev - x) < step_offset //&& crit2)
  }


  @tailrec
  private def run(target: Double, retry: Int, fstep: Double, x: Double, yargs: List[Double]): Try[(Double, List[Double])] = {
    if (reached(x, target)) Success((x, yargs))
    else {
      val (nv, error) = odeSolver.nextStep(fstep, x, yargs, Func)
      //println ("new value :", nv)
      nv match {
        case Failure(f) => Failure(f)
        case Success((new_x, new_yargs)) => {
          trapped(x, yargs, new_x, new_yargs) match {
            case true => Failure(new RuntimeException("results are trapped in ")) // + className(this)))
            case false => adaptiveStep.nextStepSize(target, x, yargs, new_x, new_yargs, fstep, error) match {
              case (AdaptiveStepTransition.HALT, _) => Failure(new RuntimeException("Result failed to converge"))
              case (AdaptiveStepTransition.CONTINUE, new_step_size) => {
                //println("new step size :" + new_step_size, " newx + new_step_size " + new_x + new_step_size + " target : " + target)
                run(
                  target,
                  0,
                  new_step_size,
                  new_x,
                  new_yargs
                )
              }
              case (AdaptiveStepTransition.RETRY, new_step_size) => {
                //println("retry with target", target, " step : ", new_step_size, "rtry :", retry, " val :" , value)
                retry match {
                  case `max_retry` => Failure(new RuntimeException("max number of retries " + max_retry + " exceeded"))
                  case _ => {
                    run(
                      target,
                      retry + 1,
                      new_step_size,
                      x,
                      yargs
                    )
                  }
                }
              }
              case (_,_) => {
                Failure(new RuntimeException("results are trapped in "))
              }
            }

          }
        }

      }
    }
  }

  @tailrec
  private def singleStep(steps:Int, initialStepSize:Double, start:Double, yargs:List[Double]): Try[List[(Double, List[Double])]] = {
    if (steps < 1) Success(Memoize.toList) else {
      run(start + initialStepSize, 0, initialStepSize, start, yargs) match {
        case Success(res) => {
          Memoize.store(res._1, res._2)
          singleStep(steps - 1, initialStepSize, res._1, res._2)
        }
        case Failure(e) => Failure(e)
      }
    }
  }

  def run(start: Double, end: Double, stepSize: Double, yargs: List[Double]): Try[List[(Double, List[Double])]] = {
    Memoize.store(start, yargs)
    if (start == end) {
      Success(Memoize.toList)
    }
    else {
      val span = end - start
      val steps = (0.5 + (span / stepSize).abs).toInt
      val initialStepSize = span / (1.0 * steps)
      //println("revised step ", fstep)
      singleStep(steps, initialStepSize,start, yargs)
    }
  }

  def run(range: LineRangeT[Double], init: Array[Double]): Option[StackT] = HandleException {
    LogIt().info("dimension :" + dim + " ; starting with linerange : " + range + " initial conditions : {" + init.mkString(",") + "}")
    val stack = StackDouble(dim, range)
    run(range.start, range.end, range.stepSize, init.toList) match {
      case Failure(_) => None
      case Success(results) => {
        for ((x, yargs) <- results) yield {
          stack.append(x)
          for (y <- yargs) yield {
            stack.append(y)
          }
        }
        Some(stack)
      }
    }
  }

  override lazy val toString:String = className(this) + " : " + adaptiveStep.toString

}
