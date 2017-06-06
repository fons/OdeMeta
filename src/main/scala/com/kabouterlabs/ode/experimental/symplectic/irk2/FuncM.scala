package com.kabouterlabs.ode.experimental.symplectic.irk2

/**
  * Created by fons on 5/14/17.
  */
case class FuncM(funcM: Option[(Int,Double, Array[Double])=>Array[Double]])
{
  def apply(dim:Int, x:Double, positions:Array[Double]) = {
    funcM match {
      case Some(f)=>f(dim,x,positions)
      case _ => Array[Double]()
    }
  }


}
