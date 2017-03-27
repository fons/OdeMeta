package com.kabouterlabs.ode.config

/**
  * Created by fons on 3/10/17.
  */
class BandWidth(val width:Int)
object BandWidth
{
  def apply(width:Int) = new BandWidth(width)
}
case class LowerBandWidth(override val width:Int) extends BandWidth(width)
case class UpperBandWidth(override val width:Int) extends BandWidth(width)
