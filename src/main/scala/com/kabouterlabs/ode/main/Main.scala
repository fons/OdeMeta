package com.kabouterlabs.ode.main


import com.kabouterlabs.ode.config.{LowerBandWidth, UpperBandWidth}
import com.kabouterlabs.ode.config._
import com.kabouterlabs.ode.range.LineRange
import com.kabouterlabs.ode._
import com.kabouterlabs.ode.implicits.OdeImplicits._
import com.kabouterlabs.ode.implicits.lsoda.LsodaImplicit._
import com.kabouterlabs.ode.Ivp
import com.kabouterlabs.ode.util.LogIt
import scala.language.existentials
import scala.language.reflectiveCalls
import scala.language.higherKinds
import scala.language.postfixOps


/**
  * Created by fons on 1/5/17.
  */



object Main extends App {

//  val m = OptionalParameters()++(OptionalParameterType.INITIAL_STEP_SIZE, 0.1)
//  println(m)

////  def log(logger:Logger, foo: String)(implicit line: sourcecode.Line, file: sourcecode.File, enclosing:sourcecode.Enclosing) = {
////    logger.info(s"${file.value}:${enclosing.value}:${line.value} $foo")
////  }
////
//
//
//  val logger = Logger(LoggerFactory.getLogger("name"))
//
//
//
//  val c = Config() -> JacobianType.BandedJacobian(LowerBandWidth(9), UpperBandWidth(9)) //-> Methods.ADAMS -> Tolerance(0.000001, 0.000001)

//  Option
//
//  val v = JacobianBandWidth(LowerBandWidth(2), UpperBandWidth(4))
//
//  v match {
//    case JacobianBandWidth(_,_) => println("has ml and mu")
//    case JacobianUnknownBandWidth() => println("don't know band witdth")
//  }

//  val c = new ConfigElements(Methods.Adams())
//  c.methodflag()
//
//
//  val p:Either[Double, Array[Double]] = Left(1.0)
//  println(p.left)


//  val pl =  FuncParams +| ("h" -> 10, "g"->90) +\ ("c" -> 90.0, "h" -> 89.78)
//
//  //test(pl)
//  println(pl \ "h" , pl \ "g")

//  val solver = Ivp(2, constraints=1) + Config() + FuncParams("g" -> - 9.81, "beta" -> -0.9) +
//    ((dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params: FuncParams[Double])=> {
//    ydot(0) = y(1)
//    ydot(1) = params -> "g"
//  }) + ((dim: Int, x:Double, y:Array[Double], ng:Int, gout:Array[Double], params: FuncParams[Double])=> {
//    gout(0) = y(0)
//  }) +((dim: Int, x:Double, y:Array[Double], ng:Int, jroot:Array[Int], params: FuncParams[Double])=>{
//    y(0) = 0.0
//    y(1) = (params -> "beta") * y(1)
//  }) +>
//
//  println(solver)
//
//  val res = solver(Array(0.0, 10.0))(LineRange(0.0, 20.0, 0.1))
//  res().map(_.show)
  //val rootLogger = org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
  //rootLogger.setLevel(ch.qos.logback.classic.Level.OFF)
 val logIt = LogIt()
  logIt.level.info()
//
  val alpha = -0.04
  val beta  = "1.0e4".toDouble
  val gamma =  "3.0e7".toDouble
  val params =  FuncParams +\ ("alpha" -> alpha, "beta" -> beta, "gamma" -> gamma)

  val w = Ivp(dim=3, constraints=0)+ (Config(Methods.BDF) -> JacobianType.FullJacobian -> Tolerance(0.001)) + FuncParams("alpha" -> -0.04, "beta" -> "1.0e4".toDouble, "gamma" -> "3.0e7".toDouble ) +
    ( (dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params:FuncParams[Double]) => {
            ydot(0) = (params -> "alpha") * y(0) + (params -> "beta") * y(1) * y(2)
            ydot(2) = (params -> "gamma") * y(1)*y(1)
            ydot(1) = -ydot(0) - ydot(2)
        } ) + ((dim:Int, x:Double, y:Array[Double], mul:Int, mpk:Int, pd:Array[Double], pdr:Int, params:FuncParams[Double])=> {
          val alpha  = params -> "alpha"
          val beta   = params -> "beta"
          val gamma  = params -> "gamma"
          pd(0) = alpha
          pd(1) = -alpha
          pd(2) = 0.0
          pd(3) = beta * y(2)
          pd(4) = -beta*y(2) - 2.0 * gamma * y(1)
          pd(5) = 2.0 * gamma * y(1)
          pd(6) = beta * y(1)
          pd(7) = -1.0 * beta * y(1)
          pd(8) = 0.0
        } ) + (OptionalParameters(OptionalParameterType.DIAGNOSTICS, true) ++ (OptionalParameterType.INITIAL_STEP_SIZE, 0.0001) ) +>

   // w
   val ivpsolver = w
   //println(w)
  val rt = ivpsolver(LineRange(0.0, 4.0, 0.1), Array(1.0, 0.0,0.0,0.0))

  println(rt)

  //rt().map(_.show)

  val p1 = Some(8)
  val p2 = Some(9)
  for (x<-p1;y<-p2) {
    println("=====> ", x,y)
  }

  val tm = OdeTest()
  println(tm)
   //tm()
  new Mol().solve()
//  val pend = Ivp(dim=2) + FuncParams("alpha" ->1.0 ) + (Config() -> Tolerance(0.000000001)) + ((dim:Int, x:Double, y:Array[Double], ydot:Array[Double], params:FuncParams[Double]) => {
//    val alpha = params->"alpha"
//    ydot(0) = y(1)
//    ydot(1) = -alpha * scala.math.sin(y(0))
//  } ) +>
//
//
//  val pendsol = pend(LineRange(0.0, 100.0, 0.1))(Array(scala.math.Pi* 999.0/1000.0, 0.0))
//  println(pendsol)
//  //pendsol().map(_.show)
//
//  val fn = sourcecode.File()
//  val line = sourcecode.Line()


//  val dao = Ivp(2) + ((dim:Int, x:Double, y:Array[Double],ydot:Array[Double], params:FuncParams[Double]) =>{
//    ydot(0) = y(1)
//    ydot(1) = y(0) - math.cos(x)
//    //LogIt().info("calling function")
//  }) + ((dim:Int, mass:Array[Double], lmass:Int, lu:Int, lm:Int, params:FuncParams[Double])=> {
//    mass(0) = 1
//    mass(1) = 0
//    mass(2) = 0
//    mass(3) = 0
//
//  }) + ( Config() -> MassMatrixType.FullMassMatrix)  +OptionalParameters(OptionalParameterType.INITIAL_STEP_SIZE, 0.0001) + DaeIndexVariables(1,1)   +>
//  val simple = dao(LineRange(0,10,0.1))(Array(math.cos(0), -math.sin(0)))
//  println(simple)
//  simple().map(_.show)

//  //logIt.level.trace()
//  logIt.info("hello this is a problem :" + fn + " " + line + " " + sourcecode.FullName())
//  logIt.debug("hello this is a problem :" + fn + " " + line + " " + sourcecode.FullName())
//  logIt.error("hello this is a problem :" + fn + " " + line + " " + sourcecode.FullName())
//  logIt.warn("hello this is a problem :" + fn + " " + line + " " + sourcecode.FullName())
//  logIt.trace("hello this is a problem :" + fn + " " + line + " " + sourcecode.FullName())
////    ivpsolver
//
//
//
//  val dim = 7
//  val mu = UpperBandWidth(5)
//  val ml = LowerBandWidth(1)
//  val lo = scala.math.min(dim - 1, ml.width)
//  val up = scala.math.min(dim - 1, mu.width)
//  val p = for (col <- Range (1, dim +1)) yield {
//    col match {
//      case _ if col <= up => Range(1, scala.math.min(dim, col + lo) + 1)
//      case _ if col > dim - lo => Range(col - up , dim+1)
//      case _ => Range(col-up, col + lo+1)
//    }
//  }
//  println(p.mkString(","))
//  val op = p.map(_.length)
//  println(op)
//  val kl = for (hi <- Range(1, op.length + 1)) yield {
//    op.slice(0,hi).sum+1
//  }
//  println(kl)
//  println(p.flatten.mkString(","))

//    val solver4d = OdeM(3, (dim:Int, x:Double, y:Array[Double], ydot:Array[Double]) => {
//        ydot(0) = alpha*y(0) + beta * y(1) * y(2)
//        ydot(2) = gamma * y(1)*y(1)
//        ydot(1) = -ydot(0) - ydot(2)
//    }, (dim:Int, x:Double, y:Array[Double], mul:Int, mpk:Int, pd:Array[Double], pdr:Int)=> {
//      pd(0) = alpha
//      pd(1) = -alpha
//      pd(2) = 0.0
//      pd(3) = beta * y(2)
//      pd(4) = -beta*y(2) - 2.0 * gamma * y(1)
//      pd(5) = 2.0 * gamma * y(1)
//      pd(6) = beta * y(1)
//      pd(7) = -1.0 * beta * y(1)
//      pd(8) = 0.0
//    })
//
//  val vxy = solver4d(LineRange(0.0, 4.0, 0.1))(Array(1.0, 0.0,0.0,0.0))() //.get(500).get.mkString(",")
//  LsodaImplicit.Ev$OdeRunT(solver4d)

//    def solver3d = OdeM(3)
//    def s3d = solver3d( (dim:Int, x:Double, y:Array[Double], ydot:Array[Double]) => {
//
//      ydot(0) = -0.04*y(0) + "1.0e4".toDouble * y(1) * y(2)
//      ydot(2) = "3.0e7".toDouble * y(1)*y(1)
//      ydot(1) = -ydot(0) - ydot(2)
//    })
//    //println(implicitly(s3d))
//  val v = s4d(LineRange(0.0, 4.0, 0.1))(Array(1.0, 0.0,0.0,0.0))() //.get(500).get.mkString(",")
//
//  for (c <- v) {
//
//      //println("===> ", c(0))
//      //println(c(1000).map(_.mkString(",")))
//      println(c.last.map(_.mkString(",")))
//    }
//

//  val grav   = 9.80665
//  val length = 1.0
//  val coeff1 = - 1.0 //- grav/length
//
//  def solver2d = OdeM(2)
//
//  val pend = solver2d((dim: Int, x: Double, y: Array[Double], ydot: Array[Double]) => {
//    ydot(0) = y(1)
//    ydot(1) = coeff1 * scala.math.sin(y(0))
//
//  })
//  ,
//    (dim:Int, x:Double, y:Array[Double], mul:Int, mpk:Int, pd:Array[Double], pdr:Int)=> {
//      println("calling jacobian")
//      pd(0) = 0.0
//      pd(1) = -scala.math.cos(y(0))
//      pd(2) = 1.0
//      pd(3) = 0.0
//    })

//  val pv = pend(LineRange(0.0, 100.0, 0.1))(Array(Math.PI * 999 / 1000.0, 0.0,0.0,0.0))() //.get(500).get.mkString(",")
//  for (c <- pv) {
//    //println("===> ", c(0))
//    //println(c(1000).map(_.mkString(",")))
//    println(c.last.map(_.mkString(",")))
//  }

}
