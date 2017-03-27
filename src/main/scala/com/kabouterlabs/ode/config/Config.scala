package com.kabouterlabs.ode.config

/**
  * Created by fons on 1/22/17.
  */
case class Config(val method:Methods.Method, relativeTolerance:RelativeTolerance, absoluteTolerance:AbsoluteTolerance,
                  jacobianType:JacobianType, mass:MassMatrixType, options:Option[OptionalParameters])
{
  def this() {this(Methods.Default, Defaults.rtol, Defaults.atol, JacobianType.FullJacobian, MassMatrixType.IdentityMatrix, None)}

  def this(m:Methods.Method) {this(m,Defaults.rtol,Defaults.atol,JacobianType.FullJacobian,MassMatrixType.IdentityMatrix, None)}

  def ->(meth1:Methods.Method)        = new Config(meth1,  relativeTolerance, absoluteTolerance, jacobianType,mass,options)
  def ->(jac1:JacobianType)           = new Config(method, relativeTolerance, absoluteTolerance, jac1, mass, options)
  def ->(tol:Tolerance)               = new Config(method, tol, tol, jacobianType,mass, options)
  def ->(tol:Double*)                 = new Config(method, Tolerance(tol.toArray), Tolerance(tol.toArray), jacobianType ,mass,options)
  def ->(reltol:RelativeTolerance)    = new Config(method,reltol,absoluteTolerance,jacobianType, mass, options)
  def ->(abstol:AbsoluteTolerance)    = new Config(method,relativeTolerance ,abstol,jacobianType, mass, options)
  def ->(mass1:MassMatrixType)        = new Config(method,relativeTolerance ,absoluteTolerance,jacobianType, mass1, options)
  def addOpt(options1:OptionalParameters) = new Config(method,relativeTolerance ,absoluteTolerance,jacobianType, mass, Some(options1))
}

object Config
{
  def apply() = new Config()
  def apply(mf:Methods.Method) = new Config(mf)
  def appy(mf:Methods.Method, tol: =>Tolerance) = new Config(mf, tol, tol, JacobianType.FullJacobian, MassMatrixType.IdentityMatrix, None)
  def apply(mf:Methods.Method, tol:Double*) = new Config(mf, Tolerance(tol.toArray), Tolerance(tol.toArray), JacobianType.FullJacobian,
    MassMatrixType.IdentityMatrix, None)
}