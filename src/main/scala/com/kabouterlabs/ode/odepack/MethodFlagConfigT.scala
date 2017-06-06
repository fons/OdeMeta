package com.kabouterlabs.ode.odepack

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.ode.config.Config

/** Trait used to convert solver type to odepack method
  *
  */
trait MethodFlagConfigT
{
  def set_mf(config:Config):CodepackLibrary.codepack_method_e
}
