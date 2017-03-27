package com.kabouterlabs.ode.config

/**
  * Created by fons on 3/10/17.
  */


sealed trait MassMatrixType

object MassMatrixType
{
  case object IdentityMatrix extends MassMatrixType
  case object FullMassMatrix extends MassMatrixType
  case class BandedMassMatrix(mlmas:LowerBandWidth,mumas:UpperBandWidth) extends MassMatrixType
}
