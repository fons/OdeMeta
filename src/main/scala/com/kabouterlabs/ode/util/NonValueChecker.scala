package com.kabouterlabs.ode.util

import com.kabouterlabs.ode.NonValueCheckerT

/**
  * Created by fons on 4/17/17.
  */
case class NonValueChecker(array: Array[Double]) extends NonValueCheckerT
{
  private def check(agregate:Boolean, value:Double) = agregate || value.isNaN || value.isInfinite
  override def hasNonValue: Boolean = (false /: array)(check)
}
