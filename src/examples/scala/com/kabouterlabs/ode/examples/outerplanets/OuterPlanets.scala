package com.kabouterlabs.ode.examples.outerplanets



import breeze.plot.plot
import com.kabouterlabs.ode.examples.outerplanets.OuterPlanetsExample.TQP



/**
  * Created by fons on 4/14/17.
  */


import breeze.plot._
import com.kabouterlabs.ode.kernel.OdeSolver.{OdeSolverTC, _}
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.linerange.LineRange
import com.kabouterlabs.ode.util.LogIt
import com.kabouterlabs.ode.{FuncParams, Ivp}

import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}


/** Solving the OuterPlanets equation.
 *
 * Autonomous Hamiltonian
 *
 * 
 *
 *
 */


object OuterPlanetsExample {
  /**
- Outer solar system; see Geometric Numerical Integration Hairer et. al. p 11

Masses (in terms of Solar mass) of
- Jupiter
- Saturn
- Uranus
- Neptune
- Pluto
- Sun plus inner planets
*/

  val Mold = Array(0.000954786104043E0,
    0.000285583733151E0,
    0.0000437273164546E0,
    0.0000517759138449E0,
    1.0E0 / 1.3E8,
    1.00000597682E0)

  val M = Array( 1.00000597682E0,
    0.000954786104043E0,
    0.000285583733151E0,
    0.0000437273164546E0,
    0.0000517759138449E0,
    1.0E0 / 1.3E8)

  /**
  Gravitational Constant
   */
  val GravConst = 2.95912208286E-4
  case class TQP(dim:Int)
  {

    var index = dim
    val T:Array[Double] = Array.ofDim(dim)
    val Qx:Array[Double] = Array.ofDim(dim)
    val Qy:Array[Double] = Array.ofDim(dim)
    val Qz:Array[Double] = Array.ofDim(dim)
    val Px:Array[Double] = Array.ofDim(dim)
    val Py:Array[Double] = Array.ofDim(dim)
    val Pz:Array[Double] = Array.ofDim(dim)


    def append(t:Double, q:Array[Double],  p:Array[Double]) = {
      if (index > 0) {
        val j = dim - index
        T(j)  = t
        Qx(j) = q(0)
        Qy(j) = q(1)
        Qz(j) = q(2)
        Px(j) = p(0)
        Py(j) = p(1)
        Pz(j) = p(2)

        index = index - 1
      }
    }


  }

  case class PhaseSpace(dim:Int, steps:Int)
  {

    def solarHamiltonian(Q: Array[Double], P: Array[Double]) = {
      val d = Array.ofDim[Double](6, 6)
      for (i <- Range(0, 5)) {
        val i1 = 3 * i //+1
        for (j <- Range(i + 1, 6)) yield {
          val j1 = 3 * j //+ 1
          val dx = Q(i1) - Q(j1)
          val dy = Q(i1 + 1) - Q(j1 + 1)
          val dz = Q(i1 + 2) - Q(j1 + 2)
          val r = math.sqrt(dx * dx + dy * dy + dz * dz)
          d(i)(j) = r * r * r
          d(j)(i) = d(i)(j)
        }
      }

      val kE = (for (i <- Range(0, 6)) yield {
        val i1 = 3 * i
        val px = P(i1) * P(i1)
        val py = P(i1 + 1) * P(i1 + 1)
        val pz = P(i1 + 2) * P(i1 + 2)
        M(i) * (px * px + py * py + pz * pz)
      }).sum

      val pE = (for (i <- Range(1, 6)) yield {
        for (j <- Range(0, i)) yield {
          M(i) * M(j) / d(i)(j)
        }
      }).flatten.sum
      kE - GravConst * pE
    }

    val Ha:Array[Double] = Array.ofDim(steps)
    
    var index = 0;
    val TQPS:Array[TQP] = Array.ofDim(dim)
    for (index <- Range(0,dim)) yield TQPS(index) = TQP(steps)

    def append(v:Array[Double]) = {

      val t   = v(0)
      val x   = v.drop(1)
      val l   = x.length/2
      val q   = x.slice(0, l)
      val p   = x.slice(l, 2*l)
      val qp  = q.sliding(3,3).zip(p.sliding(3,3)).toArray
      for (index <- Range(0, dim)) yield {
        val item = qp(index)
        TQPS(index).append(t, item._1, item._2)
        //println("index " + index + " q " + item._1.mkString(",") + " p " + item._2.mkString(","))
      }

      if ( index < steps) {
        Ha(index) = solarHamiltonian(q, p)
        index = index + 1
      }

      this
    }

    def apply(i:Int) = TQPS(i)

  }

  private def collect(accum: PhaseSpace, a: Array[Double]) = accum.append(a)



  def apply[A]()(implicit ev1: OdeSolverTC[A] {type SolverDataType = Double}): PhaseSpace = {



    def solarFunc(dim: Int, x: Double, Q: Array[Double]) = {


      val d = Array.ofDim[Double](6, 6)
      for (i <- Range(0, 5)) {
        val i1 = 3 * i //+1
        for (j <- Range(i + 1, 6)) yield {
          val j1 = 3 * j //+ 1
          val dx = Q(i1) - Q(j1)
          val dy = Q(i1 + 1) - Q(j1 + 1)
          val dz = Q(i1 + 2) - Q(j1 + 2)
          val r = math.sqrt(dx * dx + dy * dy + dz * dz)
          d(i)(j) = r * r * r
          d(j)(i) = d(i)(j)
        }
      }

      val F = Array.ofDim[Double](18)
      for (i <- Range(0, 6)) yield {
        val i1 = 3 * i //+1
        val r: Array[(Double, Double, Double)] = (for (j <- Range(0, 6)) yield {

          if (j != i) {
            val j1 = 3 * j // + 1
            (
              M(j) * (Q(j1) - Q(i1)) / d(i)(j),
              M(j) * (Q(j1 + 1) - Q(i1 + 1)) / d(i)(j),
              M(j) * (Q(j1 + 2) - Q(i1 + 2)) / d(i)(j)
            )
          }
          else {
            (0.0, 0.0, 0.0)
          }
        }).toArray
        //println(r.mkString(","))
        val r2 = r.foldLeft((0.0, 0.0, 0.0))((accum, elem) => (accum._1 + elem._1, accum._2 + elem._2, accum._3 + elem._3))
        F(i1) = GravConst * r2._1
        F(i1 + 1) = GravConst * r2._2
        F(i1 + 2) = GravConst * r2._3
      }
      F
    }




    val Qold = Array(
      -3.5023653E0,
      -3.8169847E0,
      -1.5507963E0,
      9.0755314E0,
      -3.0458353E0,
      -1.6483708E0,
      8.3101420E0,
      -16.2901086E0,
      -7.2521278E0,
      11.4707666E0,
      -25.7294829E0,
      -10.8169456E0,
      -15.5387357E0,
      -25.2225594E0,
      -3.1902382E0,
      0.0E0,
      0.0E0,
      0.0E0
    )
    val Q = Array(0.0E0, 0.0E0,0.0E0,
      -3.5023653E0,
      -3.8169847E0,
      -1.5507963E0,
      9.0755314E0,
      -3.0458353E0,
      -1.6483708E0,
      8.3101420E0,
      -16.2901086E0,
      -7.2521278E0,
      11.4707666E0,
      -25.7294829E0,
      -10.8169456E0,
      -15.5387357E0,
      -25.2225594E0,
      -3.1902382E0
    )

    val Pold = Array(
      0.00565429E0,
      -0.00412490E0,
      -0.00190589E0,
      0.00168318E0,
      0.00483525E0,
      0.00192462E0,
      0.00354178E0,
      0.00137102E0,
      0.00055029E0,
      0.00288930E0,
      0.00114527E0,
      0.00039677E0,
      0.00276725E0,
      -0.00170702E0,
      -0.00136504E0,
      0.0E0,
      0.0E0,
      0.0E0
    )
    val P = Array(
      0.0E0,
      0.0E0,
      0.0E0,
      0.00565429E0,
      -0.00412490E0,
      -0.00190589E0,
      0.00168318E0,
      0.00483525E0,
      0.00192462E0,
      0.00354178E0,
      0.00137102E0,
      0.00055029E0,
      0.00288930E0,
      0.00114527E0,
      0.00039677E0,
      0.00276725E0,
      -0.00170702E0,
      -0.00136504E0
    )

    LogIt().level.info()
    /*
     * Initialize the solver. This is a 18  d problem with 0 constraints (roots we need to satisfy) by default.
      * Only the lsodar variant has root finding capability and this is activiated if the number of constraints is larger than 0
     */


    val ivpsolver = Ivp(dim = Q.length) + (Config(Methods.SYMPLECTIC_6_STAGES) -> AbsoluteTolerance(Array[Double]())) +
      ((dim: Int, x: Double, y: Array[Double], ydot: Array[Double], params: FuncParams[Double]) => {
        val rf = solarFunc(dim, x, y)
        for (i <- Range(0, dim)) {
          ydot(i) = rf(i)
        }
      }) +>

        val range = LineRange(0.0, 500.0, 10.0)
        val eval = ivpsolver.solve(range, Q ++ P)
        //eval().map(_.show)
        /*
         * eval returns a lazy object which needs to be executed to get the values
         */
        val fold = for (result <- eval()) yield ( PhaseSpace(6, range.steps) /: result.toArray){collect(_,_)}

        fold match {
          case Some(phaseSpace) => phaseSpace
          case None => {
            LogIt().warn("no results returned for outer planets system")
            PhaseSpace(5,0)
          }
        }


      }

}
    private case class Plotter() {
      val fig = Figure("outer-planets")
      fig.width = (fig.width * 2).toInt
      fig.height = (fig.height * 2).toInt


      val plt = fig.subplot(2, 3, 0)
      plt.xlabel = "time"
      plt.ylabel = "x"

      val plt1 = fig.subplot(2, 3, 1)
      plt1.xlabel = "time"
      plt1.ylabel = "y"

      val plt2 = fig.subplot(2, 3, 2)
      plt2.xlabel = "x"
      plt2.ylabel = "y"

      val plt3 = fig.subplot(2, 3, 3)
      plt3.xlabel = "y"
      plt3.ylabel = "dy/dt"

      val plt4 = fig.subplot(2, 3, 4)
      plt4.xlabel = "x"
      plt4.ylabel = "dx/dt"

      val plt5 = fig.subplot(2, 3, 5)
      plt5.xlabel = "time"
      plt5.ylabel = "energy residual * 1000"

      def apply(tqp:TQP, ha:Array[Double]): Unit = {
        println("plotting")
        val (time, xval, yval, zval, pxval, pyval, pzval) = (tqp.T, tqp.Qx, tqp.Qy, tqp.Qz, tqp.Px, tqp.Py, tqp.Pz)
        plt += plot(time, xval)
        plt1 += plot(time, yval)
        plt2 += plot(xval, yval)
        plt3 += plot(yval, pyval)
        plt4 += plot(xval, pxval)

        plt5 += plot(time, ha)

        fig.refresh()
        println("done")
      }
    }


//import com.kabouterlabs.ode.symplectic.implicits.GniIrk2Implicit._
import com.kabouterlabs.ode.experimental.symplectic.implicits.Irk2Implicit._


    object OuterPlanets {
      def main(args: Array[String]) = {

        val plty = Plotter()

        val res1 = OuterPlanetsExample()
        //println(res1(1).Qx.mkString(","))
        //println(res1(1).Qy.mkString(","))
        plty(res1(3), res1.Ha)

        //println(res1.Ha.mkString(","))

        println("done")

      }
    }


  









