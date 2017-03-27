package com.kabouterlabs.ode.rkf

import java.lang



import com.kabouterlabs.jodeint.cdopri5.Cdopri5Library.{dopri5_iout_e, dopri5_idid_e, dopri5_fcn_callback,dopri5, dopri5_itol_e}


import com.kabouterlabs.ode.config.Config
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.util.{HandleException, LogIt}
import com.kabouterlabs.ode._
import org.bridj.Pointer

/**
  * Created by fons on 3/1/17.
  */
case class Dopri5(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{
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

      def set_itol(itol: Pointer[lang.Integer], rtol: Pointer[lang.Double], atol: Pointer[lang.Double]) = {
        (config.relativeTolerance.get, config.absoluteTolerance.get) match {
          case ((Some(rtolv), None), (Some(atolv), None)) => {
            itol.set(dopri5_itol_e.ALL_SCALAR.value.toInt)
            rtol.set(rtolv)
            atol.set(atolv)
          }

          case ((None, Some(rtola)), (None, Some(atola))) => {
            itol.set(dopri5_itol_e.ALL_ARRAY.value.toInt)
            rtol.setDoubles(rtola.slice(0,dim))
            atol.setDoubles(atola.slice(0,dim))
          }
          case (_,_) => {
            LogIt().error("only two scalars or two vectors allowed")
            itol.set(-10)
            rtol.set(-100.0)
            atol.set(-99999.999)
          }
        }
      }

    }//end of implicit

    private def lrw(dim:Int) = 8 * dim + 21

    private def liw() = 21


    private val func = new dopri5_fcn_callback   {

      override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double], rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {

        val ydot_r:Array[Double] = ydot.getDoubles(neq.get())
        logger.trace("calling function")
        funcM(neq.get, t.getDouble,y.getDoubles(neq.get()), ydot_r, params) match {
          case Some(_) => ydot.setDoubles(ydot_r)
          case None    => logger.error("error in the function call back")
        }
      }
    }

    private val fcn: Pointer[dopri5_fcn_callback] = Pointer.getPointer(func)

    private val n:Pointer[Integer]               = Pointer.pointerToInt(dim)
    private val x:Pointer[lang.Double]           = Pointer.allocateDouble()
    private val y:Pointer[lang.Double]           = Pointer.allocateDoubles(dim)
    private val xend:Pointer[lang.Double]        = Pointer.allocateDouble()
    private val itol:Pointer[lang.Integer]       = Pointer.allocateInt()
    private val rtol:Pointer[lang.Double]        = Pointer.allocateDoubles(config.rtolDim(dim))
    private val atol:Pointer[lang.Double]        = Pointer.allocateDoubles(config.atolDim(dim))
    private val iout:Pointer[lang.Integer]       = Pointer.pointerToInt(dopri5_iout_e.NEVER_CALLED.value.toInt)

    private val lwork:Pointer[lang.Integer]      = Pointer.pointerToInt(lrw(dim))
    private val work:Pointer[lang.Double]        = Pointer.allocateDoubles(lwork.get().toLong)
    private val liwork:Pointer[lang.Integer]     = Pointer.pointerToInt(liw())
    private val iwork:Pointer[lang.Integer]      = Pointer.allocateInts(liwork.get().toLong)
    private val idid:Pointer[lang.Integer]       = Pointer.allocateInt()

    config.set_itol(itol, rtol, atol)
    for (i<-Range(0, lwork.get())) {
      work.set(i,0.0)
    }

    for (i<-Range(0, liwork.get())) {
     iwork.set(i,0)
    }

    def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] = HandleException {
      LogIt().info("starting with range : " + range + " initial conditions : {" + init.mkString(",") + "}")
      val stack = StackDouble(dim,range)
      y.setDoubles(init.slice(0, n.get()))
      x.set(range.start)
      xend.set(range.start)
      stack.append(range.start)
      for (yval <- y.getDoubles(n.get())) stack.append(yval)

      range.withRange((next:Double) => {
        xend.set(next)
        logger.trace("===> itol " + itol.get() + " rtol "+ rtol.get() + " itol " + itol.get())
        logger.trace(" x : " + x.get() + " xend " + xend.get() + " idid " + idid.get())

        dopri5(n, fcn, x, y, xend,rtol,atol,itol,null,iout,work,lwork,iwork,liwork,null, null, idid)

        dopri5_idid_e.fromValue(idid.get()) match {
          case c if c == dopri5_idid_e.SUCCESS || c == dopri5_idid_e.SUCCESS_INTR   => {
            stack.append(x.get())
            for (yval <- y.getDoubles(n.get())) stack.append(yval)
            Some(x.get())
          }

          case c if c == dopri5_idid_e.INPUT_INCONSISTENT =>  {
            LogIt().error("input not cosistent + istate : " + c)
            None
          }

          case c if c == dopri5_idid_e.NMAX_TOO_SMALL =>  {
            LogIt().error("larger nmax needed + istate : " + c)
            None
          }

          case c if c == dopri5_idid_e.STEP_TOO_SMALL =>  {
            LogIt().error("step size too small + istate : " + c)
            None
          }

          case c if c == dopri5_idid_e.STIFF_PROBLEM =>  {
            LogIt().error("stiff problem + istate : " + c)
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
