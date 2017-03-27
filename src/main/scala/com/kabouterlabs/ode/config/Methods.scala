package com.kabouterlabs.ode.config

/**
  * Created by fons on 1/22/17.
  */
object Methods {
  sealed trait Method
  case object Default    extends Method
  case object ADAMS      extends Method
  case object BDF        extends Method
  case object SPARSE_BDF extends Method
}
