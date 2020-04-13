package com.kabouterlabs.ode.util

/**
  * Created by fons on 4/17/17.
  */
case class NonValueChecker(array: Array[Double]) extends NonValueCheckerT
{
  private def check(agregate:Boolean, value:Double) = agregate || value.isNaN || value.isInfinite
  override def hasNonValue: Boolean = (array.foldLeft(false))(check)
}
