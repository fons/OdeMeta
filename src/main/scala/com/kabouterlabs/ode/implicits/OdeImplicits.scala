package com.kabouterlabs.ode.implicits


import scala.language.implicitConversions
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.kernel._

/**
  * Created by fons on 1/6/17.
  */
object OdeImplicits
{

  type OdeFuncInt = (Int, Double, Array[Double], Array[Double]) => Int
  type OdeFuncDouble = (Int, Double, Array[Double], Array[Double]) => Double
  type OdeFuncUnit = (Int, Double, Array[Double], Array[Double]) => Unit

  type OdeFuncUnitParam = (Int, Double, Array[Double], Array[Double], FuncParams[Double]) => Unit

  implicit def convertFunc(f: OdeFuncUnit): OdeFuncUnitParam = (n: Int, x: Double, y: Array[Double], ydot: Array[Double], p: FuncParams[Double]) => f(n, x, y, ydot)

  implicit def convertFm(f: OdeFuncUnit): OdeFuncM[Double] = OdeFuncM[Double](Some({f}))

  implicit def convertFmParam(f: OdeFuncUnitParam): OdeFuncM[Double] = OdeFuncM[Double](Some({f}))

  //implicits def convertFm2(f:OdeFuncInt):OdeFuncM[Double] = OdeFuncM[Double](Some({f}))

  implicit def convertIt(f: OdeFuncInt): OdeFuncUnit = (n: Int, x: Double, y: Array[Double], ydot: Array[Double]) => {
    f(n, x, y, ydot); ()
  }

  /*
    * Conversion functions for jacobians
    * Converts lambda expresssions to FunM
    *
    *
   */



  type JacFuncUnitDouble = (Int, Double, Array[Double], Int, Int, Array[Double], Int) => Unit

  type JacFuncUnitDoubleParam = (Int, Double, Array[Double], Int, Int, Array[Double], Int, FuncParams[Double]) => Unit

  
  implicit def convertFunc(f: JacFuncUnitDouble): JacFuncUnitDoubleParam = (n: Int, x: Double, y: Array[Double], mu: Int, ml: Int,
                                                                            pd: Array[Double], pdr: Int, p: FuncParams[Double]) => f(n, x, y, mu, ml, pd, pdr)

  implicit def convertToJacFuncUnitDouble(f: JacFuncUnitDouble): JacobianFuncM[Double] = JacobianFuncM[Double](Some(f))

  implicit def convertToJacFuncUnitDoubleParam(f: JacFuncUnitDoubleParam): JacobianFuncM[Double] = JacobianFuncM[Double](Some(f))


  type ConstraintFuncDouble = (Int, Double, Array[Double], Int, Array[Double]) => Unit
  type ConstraintFuncDoubleParam = (Int, Double, Array[Double], Int, Array[Double], FuncParams[Double]) => Unit

  implicit def convertConstrFuncDouble(g: ConstraintFuncDouble): ConstraintFuncDoubleParam =
    (n: Int, x: Double, y: Array[Double], ng: Int, jr: Array[Double], p: FuncParams[Double]) => g(n, x, y, ng, jr)

  implicit def convertConstrDouble(f: ConstraintFuncDoubleParam): ConstraintFuncM[Double] = ConstraintFuncM[Double](Some({
    f
  }))


  type EventFuncDouble = (Int, Double, Array[Double], Int, Array[Int]) => Unit
  type EventFuncDoubleParam = (Int, Double, Array[Double], Int, Array[Int], FuncParams[Double]) => Unit

  implicit def convertEventFuncDouble(g: EventFuncDouble): EventFuncDoubleParam =
    (n: Int, x: Double, y: Array[Double], ng: Int, jr: Array[Int], p: FuncParams[Double]) => g(n, x, y, ng, jr)

  implicit def convertEventDouble(f: EventFuncDoubleParam): EventFuncM[Double] = EventFuncM[Double](Some({
    f
  }))

  type MassMatrixFuncDouble = (Int, Array[Double], Int, Int, Int) => Unit
  type MassMatrixFuncDoubleParam = (Int, Array[Double], Int, Int, Int, FuncParams[Double]) => Unit

  implicit def convertMassMatrixFuncDouble(g: MassMatrixFuncDouble): MassMatrixFuncDoubleParam =
    (n: Int, m: Array[Double], i: Int, j: Int, h: Int, p: FuncParams[Double]) => g(n, m, i, j, h)

  implicit def convertMassMatrixFuncDouble(m: MassMatrixFuncDoubleParam): MassMatrixFuncM[Double] = MassMatrixFuncM[Double](Some({
    m
  }))

}
