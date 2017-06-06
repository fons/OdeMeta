package com.kabouterlabs.ode.radau5

import java.lang

import com.kabouterlabs.jodeint.cradau5.Cradau5Library._
import com.kabouterlabs.ode.config.Config
import com.kabouterlabs.ode.stack.StackDouble
import com.kabouterlabs.ode.{FuncParams, OdeFuncM, StackT, LineRangeT}
import com.kabouterlabs.ode.util.{HandleException, LogIt}
import org.bridj.Pointer

/** Basic Radau5 interface
  *
  * Implicit Runge-Kutta method of order 5 (Radau IIA) for problems of the form y'=f(x,y). Provides a simple interface to radau5, with sensible defaults.
  *
  * @note : for more information on the underlying algorithm : [[http://www.unige.ch/~hairer/prog/stiff/radau5.f]] or [[http://www.unige.ch/~hairer/software.html]]
  *
  * @constructor  Radau Ode Solver instance.
  * @param  dim    : Dimension of the ODE
  * @param  funcM  : ODE solver call back
  * @param  params : Parameters
  * @param  config : Configuration parameters
  *
  *
  */
class Radau5Basic(dim:Int, funcM:OdeFuncM[Double], params:FuncParams[Double], config:Config)
{
  private val func1 = new radau5_ode_func   {

    override def apply(neq: Pointer[Integer], t: Pointer[lang.Double], y: Pointer[lang.Double], ydot: Pointer[lang.Double],
                       rpar: Pointer[lang.Double], ipar: Pointer[Integer]): Unit = {


      val ydot_r:Array[Double] = ydot.getDoubles(neq.get())

      funcM(neq.get, t.get(),y.getDoubles(neq.get()), ydot_r, params) match {
        case Some(_) => ydot.setDoubles(ydot_r)
        case None => LogIt().error("error in function callback")
      }
    }
  }

  private val func_sp: Pointer[radau5_ode_func] = Pointer.getPointer(func1)

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


      val ptr: Pointer[lang.Double] = Pointer.allocateDoubles(stack.size)
      radau5_basic(ptr, init_ptr, func_sp, dim, range.start, range.end, range.stepSize) match {
        case radau5_idid_e.SUCCESS => Some(stack.fromArray(ptr.getDoubles(stack.size)))
        case err => {
          LogIt().error("error encountered in dvode basic :" + err)
          None
        }
      }
    }
}
