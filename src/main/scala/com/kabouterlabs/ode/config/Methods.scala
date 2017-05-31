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
  case object SYMPLECTIC_2_STAGES extends Method
  case object SYMPLECTIC_4_STAGES extends Method
  case object SYMPLECTIC_6_STAGES extends Method
  case object SYMPLECTIC_801_STAGES extends Method
  case object SYMPLECTIC_802_STAGES extends Method
  case object SYMPLECTIC_803_STAGES extends Method
}
