# OdeMeta

## Introduction

OdeMeta provides solvers for Ordinary Differential Equations (ODE) and Differential Algebraic Equations. It does so by using [JavaOdeInt](https://github.com/fons/JavaOdeInt) which provides an interface to well known Fortran solvers.



## Building

sbt compile

## Examples

sbt run

There are various examples available. After you do sbt run you' ll see a list like this

[warn] Multiple main classes detected.  Run 'show discoveredMainClasses' to see the list

Multiple main classes detected, select one to run:

 [1] com.kabouterlabs.ode.examples.daeexample.DaeExampleExample
 [2] com.kabouterlabs.ode.examples.ode2.Ode2
 [3] com.kabouterlabs.ode.main.Main
 [4] com.kabouterlabs.ode.examples.ode1.Ode1
 [5] com.kabouterlabs.ode.examples.henonheiles.HenonHeiles
 [6] com.kabouterlabs.ode.examples.arenstorf.Arenstorf
 [7] com.kabouterlabs.ode.examples.vanderpol.VanderPol
 [8] com.kabouterlabs.ode.examples.daependulum.DaePendulumExample
 [9] com.kabouterlabs.ode.examples.ode3.Ode3
 [10] com.kabouterlabs.ode.examples.daeamplifier.DaeAmplifierExample
 [11] com.kabouterlabs.ode.examples.bouncingball.BouncingBallExample
 [12] com.kabouterlabs.ode.examples.lorenz.Lorenz



## Ode/Dao libraries covered

- OdePack
- vode
- rfk45 
- dop853
- dopri5
- radau5
- gamd
- bimd

# Synopsis

## Initial Value Problem 

```scala
/*
 * Solving the Arenstorf equation.
 *
 * y_1'' = y1 + 2y_1' - mu_2 (y1 + mu_1)/D1 - mu_1 (y_1-mu_2)/D2
 * y_2'' = y2 + 2y_1' - mu_2 y2/D1 - mu_1 y_2/D2
 *
 * D1 = ((y_1 + mu_1)^2 + y_2^2)^(3/2)
 * D2 = ((y_1 - mu_2)^2 + y_2^2)^(3/2)
 * 
 
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val mu1 =  0.012277471

    val ivpsolver = Ivp(dim = 4) +
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.0000000000001)) +
      FuncParams[Double]("mu1" -> mu1, "mu2" -> (1.0 - mu1)) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        val mu1 = params->"mu1"
        val mu2 = params->"mu2"
        val d1  =  (y(0) + mu1)
        val D1  = math.pow(d1*d1 + y(1)*y(1), 3.0/2.0)
        val d2  = y(0) - mu2
        val D2  = math.pow(d2*d2 + y(1)*y(1), 3.0/2.0)
        ydot(0) = y(2)
        ydot(1) = y(3)
        ydot(2) = y(0) + 2*y(3) - mu2*(y(0)+mu1)/D1 - mu1 * (y(0) - mu2)/D2
        ydot(3) = y(1) - 2*y(2) - mu2*y(1)/D1 - mu1 * y(1)/D2
      }) +
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +
      ((dim: Int, x: Double, y: Array[Double], mul: Int, mpk: Int, pd: Array[Double], pdr: Int, params: FuncParams[Double]) => {
        val mu1 = params->"mu1"
        val mu2 = params->"mu2"

        val d1  =  (y(0) + mu1)
        val D1  = math.pow(d1*d1 + y(1)*y(1), 3.0/2.0)
        val d2  = y(0) - mu2
        val D2  = math.pow(d2*d2 + y(1)*y(1), 3.0/2.0)

        val conv = ConvertArrayToMatrix(pd)
        conv(0,0, 0.0)
        conv(0,1, 0.0)
        conv(0,2, 1.0)
        conv(0,3, 0.0)

        conv(1,0,0.0)
        conv(1,1, 0.0)
        conv(1,2, 0.0)
        conv(1,3, 0.0)

        conv(2,0, 1.0 - mu2/D1 - mu1/D2)
        conv(2,1,0.0)
        conv(2,2,0.0)
        conv(2,3,2.0)

        conv(3,0,0.0)
        conv(3,1,1.0 - mu2/D1 - mu1/D2)
        conv(3,2,-2.0)
        conv(3,3,0.0)

      }) +>

  
```



## DAE

```scala
 /**
  *
  *  pendulum dae
  *
  * dx/dt = u
  * dy/dt = v
  * du/dt = - lambda x
  * dv/dt = - lambda y - 9.81
  * 0     = x^2 + y^2 - 1
  *
  *
     * Initialize the solver. This is a 3 d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */
    val ivpsolver = Ivp(dim = 5) +
      (Config() -> JacobianType.FullJacobian -> MassMatrixType.FullMassMatrix -> Tolerance(0.000001)) +
      DaeIndexVariables(2,2,1) +
      FuncParams("g" -> 9.81, "lambda" -> 1.0) +
    ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        ydot(0) = y(2)
        ydot(1) = y(3)
        ydot(2) = - y(4) * y(0)
        ydot(3) = - y(4) * y(1) - (params->"g")
        ydot(4) = y(0)*y(0) + y(1)*y(1) - (params->"lambda")
      })  +
      ((dim:Int, am:Array[Double], lmass:Int, ml:Int, mu:Int, params:FuncParams[Double]) => {

        val conv = ConvertArrayToMatrix(am)
        for (index <- Range(0,4)) yield conv(index, index, 1.0)
        conv(4,4,0)
        //for (index <- Range(0,5*5)) println("index : " + index + " " + am(index))

    }) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.01)) +>

```



## Events

```scala
/**
  *
  *  Bouncing Ball
  *
  *  d^2y/dt^2 = -g
  *  y(0) = 0
  *  y'(00 = 10.0
  *  Loses 10 % of its energy on the return bounce
  *
  *
  */

val ivpsolver = Ivp(dim = 2, constraints=1) +
      /*
        * Configure the solver. Notice that Backward Differention (BDF) is selected.
        * The solver will ignore options it does not require, e.g. the soda slover will automtically detect the right differention scheme and will ignore this option.
        *
        * The following options are used :
        *
        *  - Method       : This is the basic linear mutistep method : BDF (backward differentiation) or Adams. The full method flag is determined in combination
        *                   with othet configuration parameters provided, notably the jocobian type.
        *
        *  - JacobianType : In this case a full jacobian is assumed. If the jacobian function is present the suer supplied jobian option will be selected
        *                   in the codepack wrapper. Other options as banded and sparse.
        *
        *  - Tolerances   : The Tolerance class can be used to simply provide the same relative and absolute tolerances. Below the absolute tolerance
        *                    is different for each dependent variable and the relative tolerance is the same
        *
       */
      (Config(Methods.BDF) -> JacobianType.FullJacobian -> RelativeTolerance(0.000001)  -> AbsoluteTolerance(Array(0.000000001, 0.000000000000001, 0.000001))) +
      /*
        * The parameters used in the functions. This is a standardized way of supplying data to the ode functions and bypasses the way fortran code.
       */
      FuncParams("g" -> -9.81) +
      /*
        * This is the ODE callback function.
        * 
        * It implements the differential equation.
        * x is the independent variable; y are the depdent variables.
        * ydot are the new values of the differentials
        * Only ydot is marshalled and unmarshalled, so that's the only data set that is allowed to change
       */
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        ydot(0) = y(1)
        ydot(1) = params->"g"
      })  +
      /*
      * Constraint callback function to find roots by sodar.
      * gout are the constraint fucntions and  determine the zeros'
      */
      ((dim:Int, x:Double, y:Array[Double], ng:Int, gout:Array[Double], params: FuncParams[Double]) => {
        gout(0) = y(0)
      }) +
      /*
        * Event function called when roots are found
        * The array of dependent variables can be changed here.
       */
      ((dim: Int, x:Double, y:Array[Double], ng:Int, jroot:Array[Int], params: FuncParams[Double])=>{

        println("root found : " + x + " " + y.mkString(" , ") + " add jroot : [ " + jroot.mkString(" , ") + " ]")
        y(0)  = 0.0
        y(1)  = -0.9 * y(1)
        println("new values : " + y.mkString(","))
      }) +
      /*
        * Settting optional parameters. Those differ per solver but the most common ones have been factored out.
        * In the setting below the diagnostic messages have not been suppressed and an initial step size is provided.
        * The optional parameters are highly specific to the fortran ode solver being called.
        * Use the OptionalParameters to pass in iwork or rwark arrays if that's needed.
       */
      (OptionalParameters(OptionalParameterType.DIAGNOSTICS, false) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.1)) +>

```



