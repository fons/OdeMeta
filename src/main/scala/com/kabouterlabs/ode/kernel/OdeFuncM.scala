package com.kabouterlabs.ode.kernel

import com.kabouterlabs.ode.{FuncParams, kernel}
import com.kabouterlabs.ode.util.HandleException

import scala.language.higherKinds


/** Contains the call back fucntion for the ODE routines
  *
  * @constructor : Option of type (Int,Double, Array[U], Array[U], FuncParams[U]=> Unit)
  *
  * This is the function signature used by most of the Fortran routines.
  * @see [[kernel.OdeFuncM[U].FuncT]] for a description of the arguments
  *
  * @see type converters in [[com.kabouterlabs.ode.implicits.OdeImplicits]] for convenient conversions
  *
  * @example {{{
  *            val func = (d:Int, x:Double, y:Array[Double], ydot:Array[Double], p:FuncParams[Double]) => {ydot(0) = y(0)/(x+1)
  *            val fm = OdeFuncM[Double](Some(func))
               val next:Option[Array[Double]] = for (f <- fm) yield {
                  val res = Array.ofDim[Double](1)
                  f(1, 2.0 , Array(2.0), res, FuncParams[Double]())
                  res
          }
  * }}}
  *
  */


case class OdeFuncM[U](private val funcOption:Option[(Int, Double, Array[U], Array[U], FuncParams[U])=>Unit]) {
  /** Dependent parameter type
    *
    */
  type ElemT = U

  /** Function signature
    *
    */
  type FuncT =  (Int, Double, Array[U], Array[U], FuncParams[U])=>Unit

  /** Map FuncT to an Option of a new type
    *
    * @param t  : function of type FuncT => M
    * @tparam M : new type parameter
    * @return   : Option[M]
    */

  def map[M](t : FuncT => M)  : Option[M] = HandleException{for (f <- funcOption) yield t(f)}

  /** Call the function
    *
    *
    * @param dim    : dimensionality
    * @param x      : independent variable
    * @param y      : Array of dimension dim holding the depent variable values
    * @param ydot   : Array of dimension dim holding the new derivative values
    * @param params : Parameters; @see  [[com.kabouterlabs.ode.FuncParams]]
    * @return       : Unit Option
    */
  def apply(dim:Int, x:Double, y:Array[U], ydot:Array[U], params:FuncParams[U]):Option[Unit] =
      HandleException {funcOption.map(_(dim, x, y, ydot,params))}

}

/**
  * Companion object
  */

object OdeFuncM
{
  /**
    *
    * @tparam U : Dependent variable type
    * @return   : no-op function
    */
  def none[U] = new OdeFuncM[U](None)
}