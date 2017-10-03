package com.kabouterlabs.ode.stack

import java.io._
import java.lang

import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.util.LogIt
import org.bridj.Pointer

/**
  * Created by fons on 1/7/17.
  */
case class StackDouble(dim:Int, range:LineRangeT[Double]) extends StackT
{
  override type ElemT = Double

  final val size = (dim + 1) * (range.steps + 1)

  private val ptr = new Array[ElemT](size)
  private var counter = 0;

  final def fromArray(arr:Array[Double]) = {
    val rem = scala.math.min(arr.length, size - counter)
    Array.copy(arr, counter, ptr, 0, rem)
    counter = counter + rem
    this
  }

  final def append(value:Double) = {
    if (counter < size) {
      ptr(counter) = value
      counter = counter + 1
    }

    this
  }

  final override def toFile(fn: String): Unit = {
    val pw = new PrintWriter(new File(fn))
    val result = ptr
    pw.write(csvHeader+ "\n")
    for (index <- result.indices) {
      pw.write(f"${result(index)}%.12e " + ",")
      if ((index + 1) % (dim + 1) == 0) {
        pw.write("\n")
      }

    }
    pw.close
  }

  private val space = String.valueOf("         ")
  private def fw(s:String) = (space + s + space).drop(s.length - 1)

  private lazy val csvHeader  = fw("X") +   "," + Range(1, dim + 1).foldLeft("")((accum,x) => (if (accum.toString.length == 0)  accum else accum + ",")   + fw("Y" + x.toString ))


  final override def show: Unit = {
    println(csvHeader)
    val result = ptr
    for (index <- Range(0, counter)) {
      val res = result(index)
      print(f"$res%.12e ")
      if ((index + 1) % (dim + 1) == 0) println() else print(",")
    }
  }

  final override def toArray: Array[Array[Double]] = ptr.sliding(dim + 1, dim+1).toArray

  def apply(index:Int):Array[ElemT] = {
    val start = index * (dim +1)
    val until = start + dim + 1
    if (start < counter && until <= counter) ptr.slice(start, until) else {
      LogIt().warn("problem getting stack data for start : " + start + " and end : " + until)
      Array[ElemT]()
    }
  }

  def last:Array[ElemT] = {
    apply((counter / (dim + 1)) - 1)
  }
  
  def first:Array[ElemT] = apply(0)
}
