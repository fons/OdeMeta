package com.kabouterlabs.ode.rkf

import com.kabouterlabs.jodeint.crkf45.Crkf45Library._
import com.kabouterlabs.ode.FuncParams
import com.kabouterlabs.ode.config.Config
import com.kabouterlabs.ode.kernel.OdeFuncM
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.{StackDouble, StackT}
import com.kabouterlabs.ode.util.HandleException

/**
  * Created by fons on 3/2/17.
  */
class Rkf45 (dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{

  import java.lang

  import com.kabouterlabs.ode.config.Config
  import com.kabouterlabs.ode.util.LogIt
  import org.bridj.Pointer

  private val logger = LogIt()

  implicit class ev$config(config:Config) {

    def rtolDim(dim: Int): Int = config.relativeTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_,_)  => 1
    }

    def atolDim(dim: Int) = config.absoluteTolerance.get match {
      case (Some(_), None) => 1
      case (None, Some(arr)) => arr.length
      case (_,_)=> 1
    }

    def set_err(relerr: Pointer[lang.Double], aabserr: Pointer[lang.Double]) = {
      (config.relativeTolerance.get, config.absoluteTolerance.get) match {
        case ((Some(rtolv), None), (Some(atolv), None)) => {

          relerr.set(rtolv)
          abserr.set(atolv)
        }

        case (_,_) => {
          LogIt().error("only two scalars allowed")
          relerr.set(-100.0)
          abserr.set(-99999.999)
        }
      }
    }

  }//end of implicits

  private def lrw(dim:Int) = 6 * dim + 3

  private def liw() = 5


  private val func = new rkf45_f_callback   {


    override def apply(t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double]): Unit = {

    val ydot_r:Array[Double] = ydot.getDoubles(dim)
  logger.trace("calling function")
  funcM(dim, t.getDouble,y.getDoubles(dim), ydot_r, params) match {
  case Some(_) => ydot.setDoubles(ydot_r)
  case None    => logger.error("error in the function call back")
}
}
}

  private val fcn: Pointer[rkf45_f_callback] = Pointer.getPointer(func)

  private val neqn:Pointer[Integer]            = Pointer.pointerToInt(dim)
  private val t:Pointer[lang.Double]           = Pointer.allocateDouble()
  private val y:Pointer[lang.Double]           = Pointer.allocateDoubles(dim)
  private val tout:Pointer[lang.Double]        = Pointer.allocateDouble()

  private val relerr:Pointer[lang.Double]        = Pointer.allocateDoubles(config.rtolDim(dim))
  private val abserr:Pointer[lang.Double]        = Pointer.allocateDoubles(config.atolDim(dim))
  private val iflag:Pointer[lang.Integer]       = Pointer.allocateInt()
  private val work:Pointer[lang.Double]        = Pointer.allocateDoubles(lrw(dim))
  private val iwork:Pointer[lang.Integer]      = Pointer.allocateInts(liw())

  for (i<-Range(0, lrw(dim))) {
    work.set(i,0.0)
  }

  for (i<-Range(0, liw())) {
    iwork.set(i,0)
  }
  config.set_err(relerr, abserr)
  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] = HandleException {
    LogIt().info("starting with linerange : " + range + " initial conditions : {" + init.mkString(",") + "}")
    logger.info("===> rell err " + relerr.get() + " abserr " + abserr.get())
    val stack = StackDouble(dim,range)
    y.setDoubles(init.slice(0, neqn.get()))
    t.set(range.start)
    tout.set(range.start)
    stack.append(range.start)
    for (yval <- y.getDoubles(neqn.get())) stack.append(yval)

    range.withRange((next:Double) => {
      tout.set(next)

      logger.trace(" x : " + t.get() + " xend " + tout.get() + " flag " + iflag.get())
      iflag.set(rkf45_iflag_in_e.NORMAL.value.toInt)
      rkf45(fcn, neqn, y, t, tout,relerr,abserr,iflag,work, iwork)

      rkf45_retval_e.fromValue(iflag.get()) match {
        case c if c == rkf45_retval_e.SUCCESS || c == rkf45_retval_e.ONE_STEP_SUCCES   => {
          stack.append(t.get())
          for (yval <- y.getDoubles(neqn.get())) stack.append(yval)
          Some(t.get())
        }

        case c if c == rkf45_retval_e.INVALID_INPUT =>  {
          LogIt().error("input not valid + istate : " + c)
          None
        }
        case c if c == rkf45_retval_e.RELATIVE_ERROR_TOO_SMALL =>  {
          LogIt().error("relative error too small + istate : " + c)
          None
        }
        case c if c == rkf45_retval_e.REQUESTED_ACCURACY_TOO_SMALL =>  {
          LogIt().error("requested accuracy too small + istate : " + c)
          None
        }

        case c if c == rkf45_retval_e.SOLUTION_VANISHED =>  {
          LogIt().error("solution vanished+ istate : " + c)
          None
        }

        case c if c == rkf45_retval_e.TOO_MANY_EVAKIUATIONS =>  {
          LogIt().error("too many evalutations + istate : " + c)
          None
        }

      }

    }) match {
      case Some(_) =>   Some(stack)
      case None => {
        LogIt().warn("error occured; no results returned")
        None
      }
    }

  }



}
