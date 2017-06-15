package com.kabouterlabs.ode.linerange

import com.kabouterlabs.ode.util.LogIt

/**
  * This class represents a one dimesional grid in Doubles (a line in other words)
  */
case class LineRange(override val start:Double, override val end :Double, override val stepSize:Double, rangeOpt:Option[Array[Double]]=None) extends LineRangeT[Double]
{
  
  def this (range:Array[Double]) {
    this(range(0), range.last, (range.last - range(0))/range.length, Some(range))
  }

  def this(s:Double,e:Double, sz:Double) {
    this(s,e,sz,None)
  }

  private val dispersion = 0.0001 * stepSize;

  private val _steps: Option[Int] = {
      rangeOpt match {
        case Some(range) => Some(range.length)
        case _ => if (stepSize != 0) Some(((end - start) / stepSize).toInt) else None
      }
  }   

  private def get_next(res:Double, step:Int, stepSize:Double) = {
      rangeOpt match {
        case Some(range) => if (step < range.length) range(step) else range.last
        case None        => res + stepSize
      }
  }

  override val steps: Int = _steps.getOrElse(0)

  override def withRange(f: (Double) => Option[Double]): Option[Double] = {
    var guard = steps + 10 // a little over
    var next = start
    var res  = start
    var step = 0
    var ret:Option[Double] = Some(start)
    while ( res < end) {
      guard = guard - 1
      step  = step + 1
      next  = get_next(res, step, stepSize)
      f(next) match {
        case Some(r) => {
          res = r
          ret = Some(r)
          if (math.abs(res - next) > dispersion) {
            LogIt().warn("value returned is : " + res + " expected : " + next + " (maybe because of root finder); using this value : " + next)
            res = next
            ret = Some(res)
          }

        }
        case None    => {
          res = end
          ret = None
        }
      }
      if (guard < 0) {
        LogIt().error("number of steps exceeds expected value based on input parameters  :" + steps + " stop looping")
        res = end
        ret = None
      }
    }
    ret
  }
}

object LineRange
{
  def apply(start:Double, end:Double, stepSize:Double) = new LineRange(start, end, stepSize)
  def apply(range:Array[Double]) = new LineRange(range)
}