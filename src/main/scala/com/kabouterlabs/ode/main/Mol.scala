package com.kabouterlabs.ode.main

import com.kabouterlabs.ode.OdeSolver._

import scala.language.{postfixOps, reflectiveCalls}
import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.{FuncParams, Ivp}
import com.kabouterlabs.ode.OdeSolver.OdeSolverTC
import com.kabouterlabs.ode.config.{Config, JacobianType, Methods, Tolerance}
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode.util.LogIt

/*
  * Created by fons on 3/20/17.
  */
class Mol[A](implicit ev1:OdeSolverTC[A]{type SolverDataType=Double})
{
  def solve(): Unit =
  {
     //MOL example Utt = 4Uxx

      val logIt = LogIt()
      logIt.level.info()
      val dim = (10  * 2) + 2
      val gridstepsize = 2.0 /(dim - 2)
      val solver = Ivp(dim) + (Config(Methods.BDF) -> Tolerance(0.000000000001) ->JacobianType.BandedJacobian )+ FuncParams("h" -> gridstepsize, "c" -> 4.0) + ((dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params: FuncParams[Double])=> {
        //du/dt = v
        //println(params->"c", params->"h")
        val factor = 12.0
        val coeff:Double =  (params->"c") / (factor * (params->"h") * (params ->"h"))
        //println("---------------------start------------")
        //println(params->"c", params->"h", coeff)
        for (i <- Range(0, dim/2)) {
          //val index = i + dim/2
          //println(" ------->" + index)
          ydot(i) = y(i + dim/2)
        }
        // enforce boundary conditions
        y(0)            = 0.0
        ydot(0)         = 0.0
        y(dim/2 - 1)    = 0.0
        ydot(dim/2 - 1) = 0.0
        //dv/dt
        for (i <- Range(dim/2, dim)) {

          val index = i - dim/2
         // println(index, dim/2, dim, i)
          i match {
            case c if c == dim/2      => ydot(c) = coeff * (45.0 * y(index) - 154.0*y(index+1)+214.0*y(index+2)-156.0*y(index+3)+61.0 * y(index + 4) -10.0 * y(index + 5))
            case c if c == dim/2 + 1  => ydot(c) = coeff * (10.0 * y(index-1) - 15.0 * y(index) - 4 * y(index + 1) + 14.0 * y(index + 2) - 6.0 * y(index + 3) + y(index + 4))

            case c if c == dim - 2    => ydot(c) =  coeff * (10.0 * y(index+1)  - 15.0*y(index) - 4*y(index-1) + 14.0*y(index-2) - 6.0 * y(index - 3) + y(index - 4))
            case c if c == dim - 1    => ydot(c) =  coeff * (45.0 * y(index) - 154.0*y(index-1)+214.0*y(index-2)-156.0*y(index-3)+61.0 * y(index - 4) -10.0 * y(index - 5))

            case c                 => ydot(c) =  coeff * (-1.0 * y(index-2) + 16.0*y(index-1) -30.0*y(index) + 16.0*y(index+1) -1.0 * y(index+2))
          }
//          i match {
//            case c if c == dim/2    => ydot(c) =  coeff * (2.0 * y(index) - 5.0*y(index+1)+ 4.0*y(index+2) - y(index+3))
//            case c if c == dim - 1  => ydot(c) =  coeff * (2.0 * y(index) - 5.0*y(index-1)+ 4.0*y(index-2) - y(index-3))
//            case c                  => ydot(c) =  coeff * (y(index-1) -2.0*y(index) + y(index+1))
//          }

          //println(index, dim/2, dim, i, y(index), ydot(i))
        }
        ydot(dim/2) = 0
        ydot(dim - 1) = 0
          //println("y(0) = ", y(0), " y(dim/2-1) = ", y(dim/2-1) )

          //println("===============================================================")
        }) +>
      val xgrid = for ( i <- Range(0,dim/2)) yield {
        i * 2.0 /(dim - 2)
      }
      println(xgrid)
      val init = (for ( i <- Range(0,dim)) yield if ( i < dim/2) math.sin(math.Pi * xgrid(i)) + math.sin(2 * math.Pi*xgrid(i)) else 0).toArray

      println(init)
      println(solver)
      val res = solver.solve(LineRange(0.0, 1.5, 0.05), init)
      res().map(_.show)
      res().map(_.toFile("/tmp/mol.csv"))
      def exact(x:Double, t:Double)  = math.sin(math.Pi * x) * math.cos(2*math.Pi*t) + math.sin(2*math.Pi * x) * math.cos(4*math.Pi*t)
      def exact2(x:Double, t:Double) = -2.0 * math.Pi* math.sin(math.Pi * x) * math.sin(2*math.Pi*t) - 4.0 *math.Pi* math.sin(2*math.Pi * x) * math.sin(4*math.Pi*t)
      val time = 1.5
      
      
    print(f"$time%.12e ,")
    for (x <- xgrid.seq) {
      val res = exact(x, time)
      print(f"$res%.12e ,")

    }
//    for (x <- xgrid.seq) {
//      val res = exact2(x, time)
//      print(f"$res%.12e ,")
//    }
      println("")

  }

  def solve2(): Unit =
  {
    //MOL example Utt = Uxx

    val logIt = LogIt()
    logIt.level.info()
    val dim = (10  * 2) + 2
    val gridstepsize = 2.0 /(dim - 2)
    val solver = Ivp(dim) + FuncParams("h" -> gridstepsize, "c" -> 1.0) + ((dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params: FuncParams[Double])=> {

      val factor = 1.0 //12.0
      val coeff:Double =  (params->"c") / (factor * (params->"h") * (params ->"h"))

      for (i <- Range(0, dim/2)) {
        ydot(i) = y(i + dim/2)
      }
      // enforce boundary conditions
      y(0)            = math.sin(math.Pi * x)
      ydot(0)         = math.Pi * math.cos(math.Pi * x)
      //y(dim/2 -1)     = math.sin(math.Pi * x)
      //ydot(dim/2 - 1)   = math.Pi * math.cos(math.Pi * x)
      //dv/dt
      for (i <- Range(dim/2, dim)) {

        val index = i - dim/2

        i match {
          case c if c == dim/2    => ydot(c) =  coeff * (2.0 * y(index) - 5.0*y(index+1)+ 4.0*y(index+2) - y(index+3))
          case c if c == dim - 1  => ydot(c) =  coeff * (2.0 * y(index) - 5.0*y(index-1)+ 4.0*y(index-2) - y(index-3))
          case c                  => ydot(c) =  coeff * (y(index-1) -2.0*y(index) + y(index+1))
        }

        //println(index, dim/2, dim, i, y(index), ydot(i))
      }
      //println("y(0) = ", y(0), " y(dim/2-1) = ", y(dim/2-1) )

      //println("===============================================================")
    }) +>
    
    val xgrid = for ( i <- Range(0,dim/2)) yield {
      i * 2.0 /(dim - 2)
    }
    println(xgrid)
    val init = (for ( i <- Range(0,dim)) yield if ( i < dim/2) 0 else math.Pi * math.cos(math.Pi * xgrid(i - dim/2))).toArray

    println(init.mkString(","))
    println(solver)
    val res = solver.solve(LineRange(0.0, 0.5, 0.1), init)
    res().map(_.show)
    //res().map(_.toFile("/tmp/mol.csv"))
    def exact(x:Double, t:Double) = math.cos(math.Pi*x)  * math.sin(math.Pi * t)
    def exact2(x:Double, t:Double) = math.Pi * math.cos(math.Pi*x)  * math.cos(math.Pi * t)
    val time = 0.5
   
    print(f"$time%.12e ,")
    for (x <- xgrid.seq) {
      val res = exact(x, time)
      print(f"$res%.12e ,")

    }
    for (x <- xgrid.seq) {
      val res = exact2(x, time)
      print(f"$res%.12e ,")
    }
    println("")
    println(" grid set size : " , gridstepsize)


  }

  def solve3(): Unit =
  {
    //MOL example Ut = Uxx

    val logIt = LogIt()
    logIt.level.info()
    val dim = 21
    val gridstepsize = 1.0 /(dim - 1)
    val solver = Ivp(dim) + FuncParams("h" -> gridstepsize, "c" -> 1.0) + ((dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params: FuncParams[Double])=> {

      val factor = 1.0 //12.0
      val coeff:Double =  (params->"c") / (factor * (params->"h") * (params ->"h"))

      for (i <- Range(0, dim)) {
        val index = i
        i match {
          case c if c == 0        => ydot(c) =  0
          case c if c == dim - 1  => ydot(c) =  coeff * 2.0 * ( y(index-1) - y(index))
          case c                  => ydot(c) =  coeff * (y(index-1) -2.0*y(index) + y(index+1))
        }

        //println(index, dim/2, dim, i, y(index), ydot(i))
      }
      //println("y(0) = ", y(0), " y(dim/2-1) = ", y(dim/2-1) )

      //println("===============================================================")
    }) +>

    val xgrid = for ( i <- Range(0,dim)) yield {
      i * 1.0 /(dim - 1)
    }
    println(xgrid)
    val init = (for ( i <- Range(0,dim)) yield math.sin(0.5*math.Pi * xgrid(i))).toArray

    println("initial : " + init.mkString(","))
    println(solver)
    val res = solver.solve(LineRange(0.0, 2.5, 0.025), init)
    res().map(_.show)
    //res().map(_.toFile("/tmp/mol.csv"))
    def exact(x:Double, t:Double)  = math.exp(-math.Pi*math.Pi*0.25*t)  * math.sin(math.Pi * x * 0.5)
    def exact2(x:Double, t:Double) = math.Pi *math.Pi * -0.25 *  math.exp(-math.Pi*math.Pi*0.25*t)  * math.sin(math.Pi * x * 0.5)
    val time = 2.5

    print(f"$time%.12e ,")
    for (x <- xgrid.seq) {
      val res = exact(x, time)
      print(f"$res%.12e ,")

    }
    for (x <- xgrid.seq) {
      val res = exact2(x, time)
      print(f"$res%.12e ,")
    }
    println("")
    println(" grid set size : " , gridstepsize)
    println(exact(0.5,0.0))

  }
}
