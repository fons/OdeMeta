package com.kabouterlabs.ode.experimental.symplectic.irk2

import com.kabouterlabs.ode.config.{Config, Methods}
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.LogIt
import com.kabouterlabs.ode.{FuncParams, LineRangeT, OdeFuncM, StackT}

import scala.annotation.tailrec


private case class Irk2Runner(dim:Int, meth:Stages, func:FuncM, xstart:Double, xend:Double, nsteps:Int, initQ:Array[Double], initP:Array[Double])
{

  val h = (xend - xstart)/(1.0 * nsteps)

  val params = meth match {
    case `_2Stages` =>  Parameters2Stages(h)
    case `_4Stages` => Parameters4Stages(h)
    case `_6Stages` => Parameters6Stages(h)
    case _ => Parameters2Stages(h)
  }

  val initF = func(dim,xstart, initQ)

  val fac = params.C.map((x)=>0.5*x*x)

  val ZQ = (for (i <- Range(0,dim)) yield {
    (for (is <- Range(0,params.ns)) yield {
      params.C(is) * initP(i) + fac(is)*initF(i)
    }).toArray
  }).toArray

  val PS = initP.clone()

  
  def converged(current:Double) = {
    val uround = 1.0E-16
    (0.0 <= current) && (current < 10.0 * uround)
  }

  @tailrec
  final def rknite(x:Double, Q:Array[Double], P:Array[Double], ZQ:Array[Array[Double]], iter:Int):RkResult = {

    val Flist:Array[Double] = (for (js <- Range(0, params.ns)) yield {
      val newQ = Q.zipWithIndex.map((pair) => pair._1 + ZQ(pair._2)(js))
      func(dim, x + params.C(js), newQ)
    }).flatten.toArray

    //Flist.map((x)=> println("Flist : " + x))

    val newZQ = Array.ofDim[Double](dim, params.ns)

    val newdynoarr = for (i <- Range(0, dim)) yield {
      val dnom = math.max(0.1, math.abs(Q(i)))
      //println(" i " + i + " dnom " + dnom)

      val sumarr = for (is <- Range(0, params.ns)) yield {
        //val sum = params.C(is) * P(i)
//        params.AA(is).zipWithIndex.map((pair) => println("1 : " + pair._1 + "  2 : " + pair._2))
        val sum = params.AA(is).zipWithIndex.foldLeft(params.C(is) * P(i))((accum, pair) => accum + pair._1 * Flist(i + pair._2 * dim)  )
        newZQ(i)(is) = sum
        val p = (sum - ZQ(i)(is))/dnom
        p*p
      }
      sumarr.foldLeft(0.0)(_ + _)
    }

    val newDyno = math.sqrt(newdynoarr.foldLeft(0.0)(_ + _) / (params.ns * dim))
//    println("new dyno : ", newDyno)
//    for (row <- Range(0, dim)) {
//      for (col <- Range(0, params.ns)) {
//        println("new ZQ(" + row + "," + col + ") : " + newZQ(row)(col))
//      }
//    }
//    println("............................\n")
    (converged(newDyno), iter) match {
      case (true,  _) => RkResult(newDyno, newZQ, Flist)
      case (false, 0) => {
        println("convergence failed")
        RkResult(newDyno, Array[Array[Double]](),  Array[Double]())
      }
      case (false, _ ) => rknite(x, Q, P, newZQ, iter - 1)
    }

  }

  case class RkResult(converge:Double, ZQ:Array[Array[Double]], F:Array[Double])

  def startb(x: Double, Q: Array[Double], P: Array[Double], F: Array[Double], ZQ: Array[Array[Double]], FS: Array[Double], PS: Array[Double]) = {
    val ns1 = params.ns
    val ns2 = params.ns + 1
    val nsm = params.ns + params.nm - 1

    val YH = Array.ofDim[Double](dim)

    val newZQ = Array.ofDim[Double](dim, params.ns)

    for (i <- Range(0, dim)) {
      val sav = ZQ(i).zipWithIndex.foldLeft(0.0)((accum, pair) => accum + pair._1 * params.AM(pair._2))
      YH(i) = params.AM(ns1) * PS(i) + params.AM(ns2) * P(i) + Q(i) + sav
      for (is <- Range(0, params.ns)) yield {
        val sav = (for (js <- Range(0, params.ns)) yield {
          params.E(is)(js) * F(i + js * dim)
        }).foldLeft(0.0)(_ + _)
        //println("====> sav " + sav)
        newZQ(i)(is) = params.E(is)(ns1) * FS(i) + sav
      }
    }

//    YH.map((x) => println("YH , " + x))
//    for (row <- Range(0, dim)) {
//      for (col <- Range(0, meth)) {
//        println("newZQ(" + row + "," + col + ") : " + newZQ(row)(col))
//      }
//    }
    val newFS = func(dim, x + h, Q)

    val newF = func(dim, x + h * params.SM(params.nm - 1), YH)

    val newPS = P.clone()
    val newZQ2 = (for (i <- Range(0, dim)) yield {
      (for (is <- Range(0, params.ns)) yield {
        newZQ(i)(is) + params.E(is)(ns2) * newFS(i) + params.E(is)(nsm) * newF(i) + params.C(is) * P(i)
      }).toArray
    }).toArray

//    for (row <- Range(0, dim)) {
//      for (col <- Range(0, meth)) {
//        println("newZQ2(" + row + "," + col + ") : " + newZQ2(row)(col))
//      }
//    }
//    newF.map((x)=> println(" newF : " + x))
//    newFS.map((x)=> println(" newFS : " + x))
//    newPS.map((x)=> println(" newPS : " + x))
    RkParams(newFS, newPS, newZQ2)
  }

  //case class RkParams(F:Array[Double], FS:Array[Double], PS:Array[Double], ZQ:Array[Array[Double]])
  case class RkParams(FS:Array[Double], PS:Array[Double], ZQ:Array[Array[Double]])

  case class XQP(x:Double, Q:Array[Double], P:Array[Double])

  def updateXPQ(step:Int, Q:Array[Double], P:Array[Double], F:Array[Double]) = {

    val newQ = (for (i<- Range(0, dim)) yield {
      params.BC.zipWithIndex.foldLeft(Q(i) + h * P(i) )((acc, pair) => acc + pair._1 * F(i + pair._2*dim))
    }).toArray

    val newP = (for (i<- Range(0, dim)) yield {
      (for (is <- Range(0, params.ns/2)) yield {
        params.B(is) * (F(i + is * dim) + F(i + (params.ns - 1 - is)*dim))
      }).toArray.foldLeft(P(i))((accum, x) => accum + x)
    }).toArray

    val newX = xstart + h * (1 + step)
    //println(" x :" + newX + " Q : " + Q.mkString(",") + "\nP : " + P.mkString(","))
    XQP(newX, newQ, newP)
  }

  @tailrec
  final def fullRun(step:Int, params:RkParams, accum:Array[XQP]):Array[XQP] = if (step >= nsteps) {
     accum
  }
  else {
    val xpq = accum(step)
    //println("run rknite")
    val rkp = rknite(xpq.x, xpq.Q, xpq.P, params.ZQ, 500)
    //println("update xpq")
    val xqp1 = updateXPQ(step, xpq.Q, xpq.P, rkp.F)
    val params1 = startb(xqp1.x, xqp1.Q, xqp1.P, rkp.F, rkp.ZQ, params.FS, params.PS)
    fullRun(step + 1, params1, accum ++ Array(xqp1))
  }

  def run () = fullRun(0, RkParams(initF,PS, ZQ), Array(XQP(xstart, initQ, initP)))

}

//private case class Irk2Runner(func:FuncM)
//{
//  def apply(method:Stages, xstart:Double, xend:Double, nsteps:Int, initQ:Array[Double], initP:Array[Double]):Array[Double] = {
//    if (initP.length == initQ.length) {
//      Irk2Runner(initQ.length, method, func, xstart, xend, nsteps, initQ, initP).run().map((x) => Array(x.x) ++ x.Q ++ x.P).flatten
//    }
//    else {
//      Array[Double]()
//    }
//  }
//}

case class Irk2(dim:Int, func:OdeFuncM[Double], p:FuncParams[Double], config:Config)
{
  private def transForm():FuncM = {
    val newg = func.funcOption match {
      case Some(f1) => {
        val g =   (neq:Int,x:Double, y:Array[Double]) => {
                  val F:Array[Double] = Array.ofDim[Double](dim)
                  f1(neq, x, y, F, p)
                  F
                }
        Some(g)
      }
      case _ => None
    }
    FuncM(newg)
  }
  private val method:Stages = {
      config.method match {
        case Methods.SYMPLECTIC_2_STAGES => _2Stages
        case Methods.SYMPLECTIC_4_STAGES => _4Stages
        case Methods.SYMPLECTIC_6_STAGES => _6Stages
        case _ => _2Stages
      }
  }
  private val funct = transForm()

  private def runIrk2(method:Stages, xstart:Double, xend:Double, nsteps:Int, initQ:Array[Double], initP:Array[Double]):Array[Double] = {
    if (initP.length == initQ.length) {
      Irk2Runner(initQ.length, method, funct, xstart, xend, nsteps, initQ, initP).run().map((x) => Array(x.x) ++ x.Q ++ x.P).flatten
    }
    else {
      Array[Double]()
    }
  }
  def run(range: LineRangeT[Double], init: Array[Double]) :Option[StackT] = {
    LogIt().info("dimension :" + dim + " ; starting with range : " + range + " initial conditions : {" + init.mkString(",") + "}")
    val stack = StackDouble(2*dim, range)
    val result = runIrk2(method, range.start, range.end, range.steps, init.slice(0, dim), init.slice(dim, 2*dim))
    Some(stack.fromArray(result))
  }
}