package com.kabouterlabs.ode.range

import com.kabouterlabs.ode.LineRangeT
import com.kabouterlabs.ode.util.LogIt

/**
  * Created by fons on 1/8/17.
  */
case class LineRange(override val start:Double, override val end :Double, override val stepSize:Double) extends LineRangeT[Double](start,end,stepSize)
{
    private val dispersion = 0.0001 * stepSize;
    private val _steps: Option[Int] = if (stepSize != 0) {
      val s = ((end - start)/stepSize).toInt
      if ( s > 0) Some(s) else None} else None

  override def steps: Int = _steps.getOrElse(0)

  override def withRange(f: (Double) => Option[Double]): Option[Double] = {
    var guard = steps + 10 // a little over
    var next = start
    var res  = start
    var ret:Option[Double] = Some(start)
    while ( res < end) {
      guard = guard - 1
      next = res + stepSize
      f(next) match {
        case Some(r) => {
          res = r
          ret = Some(r)
          if (math.abs(res - next) > dispersion) {
            LogIt().warn("value returned is : " + res + " expected : " + next + " assuming out of work error and advancing to next grid point")
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
