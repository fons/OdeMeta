package com.kabouterlabs.ode.kernel

import com.kabouterlabs.ode.linerange.LineRangeT

/**
  * Created by fons on 1/6/17.
  */
trait OdeRunT[OdeSolverT]
{
  type OdeSolverRetType
  type RangeTypeT
  type ElemT
  def apply(range:LineRangeT[RangeTypeT], init:Array[ElemT]): {def apply():OdeSolverRetType}
  final def apply(init:Array[ElemT], range:LineRangeT[RangeTypeT]): {def apply():OdeSolverRetType}   = apply(range, init)
  final def apply(init:Array[ElemT]):(LineRangeT[RangeTypeT])=>{def apply():OdeSolverRetType}        = (range:LineRangeT[RangeTypeT])=>apply(range, init)
  final def apply(range:LineRangeT[RangeTypeT]):(Array[ElemT])=>{def apply():OdeSolverRetType}       = (init:Array[ElemT])=>apply(range,init)
}
