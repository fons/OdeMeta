package com.kabouterlabs.ode.odepack

import com.kabouterlabs.jodeint.codepack.CodepackLibrary
import com.kabouterlabs.ode.config.Config

/**
  * Created by fons on 1/23/17.
  */
trait MethodFlagConfigT
{
  def set_mf(config:Config):CodepackLibrary.codepack_method_e
}
