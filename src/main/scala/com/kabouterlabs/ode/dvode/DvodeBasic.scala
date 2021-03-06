package com.kabouterlabs.ode.dvode

import java.lang

import com.kabouterlabs.jodeint.cdvode.CdvodeLibrary._
import com.kabouterlabs.jodeint.cdvode.CdvodeLibrary.cdvode_ode_func
import com.kabouterlabs.ode.config.{Config, Methods}
import com.kabouterlabs.ode.kernel.OdeFuncM
import com.kabouterlabs.ode.linerange.LineRangeT
import com.kabouterlabs.ode.stack.{StackDouble, StackT}
import com.kabouterlabs.ode.util.{HandleException, LogIt}
import com.kabouterlabs.ode.FuncParams
import org.bridj.Pointer

/** Simplified Dvode interface
  *
  *  Provides a sensible set of defaults so only the ode function needs to be provided 
  *
  *
  * @see for more information on the underlying algorithm follow this link and look for dvode [[https://computation.llnl.gov/casc/odepack/]].
  *
  * @constructor  Basic Dvode Ode Solver instance.
  * @param  dim    : Dimension of the ODE
  * @param  funcM  : ODE solver call back
  * @param  params : Parameters
  * @param  config : Configuration parameters
  *
  */
case class DvodeBasic(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{

  private val func1 = new cdvode_ode_func   {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double],
                       rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {


      val ydot_r:Array[Double] = ydot.getDoubles(neq.get())

      funcM(neq.get, t.get(),y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => LogIt().error("error in function callback")
      }
    }
  }

  private val func_sp: Pointer[cdvode_ode_func] = Pointer.getPointer(func1)

  /** Solves the ODE on a 1D grid (i.e. line) for a set of initial conditions
    *
    * @param range : Range of the independent variable. Cannot be infinite.
    * @param init  : Initial conditions in the same order as the variables returned by the class back function
    * @return  a stack containing the solution on each grid point
    *
    *
    */
  
  def run(range:LineRangeT[Double], init:Array[Double]):Option[StackT] =
    HandleException {
      val stack = StackDouble(dim, range)
      val init_ptr = Pointer.allocateDoubles(dim)
      init_ptr.setDoubles(init.slice(0,dim))
      val mf:cdvode_method_e = config.method match {
        case Methods.ADAMS => cdvode_method_e.ADAMS
        case Methods.BDF   => cdvode_method_e.BDF
        case _             => cdvode_method_e.BDF
      }

      val ptr: Pointer[lang.Double] = Pointer.allocateDoubles(stack.size)
      dvode_basic(ptr, init_ptr, func_sp, dim, range.start, range.end, range.stepSize, mf) match {
        case cdvode_ode_err_e.SUCCESS => Some(stack.fromArray(ptr.getDoubles(stack.size)))
        case err => {
          LogIt().error("error encountered in dvode basic :" + err.value())
          None
        }
      }
    }
}
