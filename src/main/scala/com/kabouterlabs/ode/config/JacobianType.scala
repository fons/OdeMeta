package com.kabouterlabs.ode.config

import com.kabouterlabs.ode.util.LogIt

/**
  * Created by fons on 1/24/17.
  */


sealed trait JacobianType

object JacobianType
{

  case object DiagonalJacobian extends JacobianType

  case class BandedJacobian (ml:LowerBandWidth, mu:UpperBandWidth)   extends JacobianType

  case object BandedJacobian extends JacobianType
  {
    def nonzero(dim:Int, ml:LowerBandWidth, mu:UpperBandWidth) = (dim >= ml.width, dim >= mu.width) match {
      case (true,  true)  => dim +   mu.width * (dim  - (1 + mu.width)/2 ) + ml.width * (dim  - (1 + ml.width)/2 )
      case (false, false) => dim * dim
      case (false, true)  => mu.width * (dim  - (1 + mu.width)/2 ) + dim * (dim +1)/2
      case (true,  false) => ml.width * (dim  - (1 + ml.width)/2 ) + dim * (dim +1)/2
    }

    def toSparseJacobianStructure(dim:Int, ml:LowerBandWidth, mu:UpperBandWidth):SparseJacobianStructure = {
      val lo = scala.math.min(dim - 1, ml.width)
      val up = scala.math.min(dim - 1, mu.width)
      val jab = for (col <- Range (1, dim +1)) yield {
        col match {
          case col if col <= up => Range(1, scala.math.min(dim, col + lo) + 1)
          case col if col > dim - lo => Range(col - up , dim+1)
          case _ => Range(col-up, col + lo+1)
        }
      }
      LogIt().trace("ja base/precurser : " + jab.mkString(","))
      val aib = jab.map(_.length)
      LogIt().trace("ja base/precurser : " + aib)
      val ai = (for (hi <- Range(1, aib.length + 1)) yield {
        aib.slice(0,hi).sum+1
      }).toArray
      LogIt().trace("ai : " + ai.mkString(","))
      val ja = jab.flatten.toArray
      LogIt().trace("ja : " + ja.mkString(","))
      SparseJacobianStructure(ai,ja)
    }

  }

  case object FullJacobian extends JacobianType

  case object PreferNoJacobian extends JacobianType

  case object SparseJacobian extends JacobianType

  case class  SparseJacobian(nonZero:Int) extends JacobianType

  case class SparseJacobianStructure(ia:Array[Int], ja:Array[Int]) extends JacobianType

}

