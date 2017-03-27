package com.kabouterlabs.ode.config

/**
  * Created by fons on 1/22/17.
  */
import scala.language.implicitConversions
class Tolerance(val tol: Either[Double, Array[Double]]) {

  def get: (Option[Double], Option[Array[Double]]) = {
    if (tol.isLeft) (Some(tol.left.get), None) else (None, Some(tol.right.get))
  }
}

object Tolerance
{
  private def conv(arr:Array[Double]) = {

    arr.length match {
      case 1 => new Tolerance(Left(arr.head))
      case _ => new Tolerance(Right(arr))
    }
  }
  def apply(tol:Double*) = conv(tol.toArray)
  def apply(tol:Array[Double]) = conv(tol)
  implicit def convertTolerance(tol:Array[Double]):Tolerance = conv(tol)
}

case class AbsoluteTolerance(override val tol:Either[Double, Array[Double]]) extends Tolerance(tol)
{
  def this(tol: Double) {
    this(Left(tol))
  }

  def this(tol: Array[Double]) {
    this(Right(tol))
  }
}

object AbsoluteTolerance
{
  def apply(tol:Double)        = new AbsoluteTolerance(tol)
  def apply(tol:Array[Double]) = new AbsoluteTolerance(tol)
  def apply(tol:Double*)       = new AbsoluteTolerance(tol.toArray)
  def apply(tol:Tolerance)     = new AbsoluteTolerance(tol.tol)
  implicit def convertTolerance(tol:Tolerance):AbsoluteTolerance = new AbsoluteTolerance(tol.tol)
}

case class RelativeTolerance(override val tol:Either[Double, Array[Double]]) extends Tolerance(tol)
{
  def this(tol: Double) {
    this(Left(tol))
  }

  def this(tol: Array[Double]) {
    this(Right(tol))
  }
}

object RelativeTolerance
{
  def apply(tol:Double)        = new RelativeTolerance(tol)
  def apply(tol:Array[Double]) = new RelativeTolerance(tol)
  def apply(tol:Double*)       = new RelativeTolerance(tol.toArray)
  def apply(tol:Tolerance)     = new RelativeTolerance(tol.tol)

  implicit def convertTolerance(tol:Tolerance):RelativeTolerance = new RelativeTolerance(tol.tol)
}