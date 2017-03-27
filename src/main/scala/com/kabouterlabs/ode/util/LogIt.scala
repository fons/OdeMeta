package com.kabouterlabs.ode.util

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory



/**
  * Created by fons on 2/11/17.
  */
class LogIt(implicit enclosing:sourcecode.Enclosing, pack:sourcecode.Pkg)
{
  private val logger = Logger(LoggerFactory.getLogger(sourcecode.Enclosing()))

  private val rootLogger = org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]

  def info(foo: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    logger.info(s"$foo @ ${file.value}:${enclosing.value}:${pack.value}:${line.value} ")
  }

  def debug(foo: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    logger.debug(s"$foo @ ${file.value}:${enclosing.value}:${pack.value}:${line.value}")
  }

  def warn(foo: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    logger.warn(s"$foo @ ${file.value}:${enclosing.value}:${pack.value}:${line.value}")
  }

  def error(foo: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    logger.error(s"$foo @ ${file.value}:${enclosing.value}:${pack.value}:${line.value}")
  }

  def trace(foo: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    logger.trace(s"$foo @ ${file.value}:${enclosing.value}:${pack.value}:${line.value}")
  }

  def diagnostic(foo:String) = {
    logger.info(s"$foo ")
  }

  case object level
  {
    def info()   = rootLogger.setLevel(ch.qos.logback.classic.Level.INFO)
    def warn()   = rootLogger.setLevel(ch.qos.logback.classic.Level.WARN)
    def error()  = rootLogger.setLevel(ch.qos.logback.classic.Level.ERROR)
    def debug()  = rootLogger.setLevel(ch.qos.logback.classic.Level.DEBUG)
    def trace()  = rootLogger.setLevel(ch.qos.logback.classic.Level.TRACE)
    def all()    = rootLogger.setLevel(ch.qos.logback.classic.Level.ALL)
    def off()    = rootLogger.setLevel(ch.qos.logback.classic.Level.OFF)
  }

}

object LogIt
{
  def apply()(implicit enclosing:sourcecode.Enclosing, m:sourcecode.Pkg) = new LogIt
}