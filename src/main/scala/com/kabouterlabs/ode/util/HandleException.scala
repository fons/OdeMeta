package com.kabouterlabs.ode.util

import java.io.{PrintWriter, StringWriter}

import scala.util.control.ControlThrowable

/**
  * Created by fons on 1/11/17.
  */
object HandleException {
  //
  //
  // https://www.sumologic.com/blog-code/why-you-should-never-catch-throwable-in-scala/
  //
  //
  def safely[T](handler: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] = {
    case ex: ControlThrowable => throw ex
    // case ex: OutOfMemoryError (Assorted other nasty exceptions you don't want to catch)

    //If it's an exception they handle, pass it on
    case ex: Throwable if handler.isDefinedAt(ex) => handler(ex)

    // If they didn't handle it, rethrow. This line isn't necessary, just for clarity
    case ex: Throwable => throw ex
  }



  def apply[A](f: =>Option[A]):Option[A] = try {
    f
  }
  catch safely{ case e: Throwable =>
    val sw = new StringWriter
    e.printStackTrace(new PrintWriter(sw))
    LogIt().error("exception caught :" + e + sw)
    None
  }


}
