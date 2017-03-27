package com.kabouterlabs.ode.stack

import com.kabouterlabs.ode.{LineRangeT, StackT}

import java.io._
import java.lang

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

  def fromArray(arr:Array[Double]) = {
    val rem = scala.math.min(arr.length, size - counter)
    Array.copy(arr, counter, ptr, 0, rem)
    counter = counter + rem
    this
  }

  def append(value:Double) = {
    if (counter < size) {
      ptr(counter) = value
      counter = counter + 1
    }

    this
  }

  final override def toFile(fn: String): Unit = {
    val pw = new PrintWriter(new File(fn))
    val result = ptr
    for (index <- Range(0, result.size)) {
      pw.write(result(index) + ",")
      if ((index + 1) % (dim + 1) == 0) {
        pw.write("\n")
      }

    }
    pw.close
  }

  final override def show: Unit = {
    val result = ptr
    for (index <- Range(0, counter)) {
      val res = result(index)
      print(f"$res%.12e ,")
      if ((index + 1) % (dim + 1) == 0) {
        println()
      }

    }
  }

  final override def toArray: Array[Array[Double]] = ptr.sliding(dim + 1).toArray

  def apply(index:Int):Option[Array[ElemT]] = {
    val start = index * (dim +1)
    val until = start + dim + 1
    if (start < counter && until <= counter) Some(ptr.slice(start, until)) else None
  }

  def last:Option[Array[ElemT]] = {
    apply((counter / (dim + 1)) - 1)
  }
  def first:Option[Array[ElemT]] = apply(0)
}
