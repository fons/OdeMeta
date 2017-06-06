package com.kabouterlabs.ode.experimental.symplectic.irk2
/**
  * Created by fons on 5/26/17.
  */
abstract class ParametersBase
{
  val ns:Int
  val nsd:Int
  final val nm:Int  = 3
  final val nmd:Int = 3
  val C:Array[Double]
  val B:Array[Double]
  val BC:Array[Double]
  val SM:Array[Double]
  val AM:Array[Double]

  val AA:Array[Array[Double]]
  val E:Array[Array[Double]]

  def print() = {
    println("ns    : " + ns)
    println("nsd   : " + nsd)
    println("nm    : " + nm)
    println("nmd   : " + nmd)


    C.zipWithIndex.map((pair) => println("C (" + pair._2 + ") : " + pair._1))
    B.zipWithIndex.map((pair) => println("B (" + pair._2 + ") : " + pair._1))
    BC.zipWithIndex.map((pair) => println("BC (" + pair._2 + ") : " + pair._1))

    for (row <- Range(0, ns)) {
      for (col <- Range(0, ns)) {
        println("AA(" + row + "," + col + ") : " + AA(row)(col))
      }
    }
    for (row <- Range(0, ns)) {
      for (col <- Range(0, ns + nm)) {
        println("E(" + row + "," + col + ") : " + E(row)(col))
      }
    }

    SM.zipWithIndex.map((pair) => println("SM (" + pair._2 + ") : " + pair._1))
    AM.zipWithIndex.map((pair) => println("AM (" + pair._2 + ") : " + pair._1))
  }

}
