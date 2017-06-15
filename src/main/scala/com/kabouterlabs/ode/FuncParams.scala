package com.kabouterlabs.ode

/** Function parameters
  *
  * Use the companion object to create an instance.
  *
  * FuncParams can be used to pass parameters to the ODE or Jacobian function.
  * It recognizes two parameter types : Int and V (which is typicallly Double).
  * @example {{{
  *            val params =  FuncParams +\ ("alpha" -> alpha, "beta" -> beta, "gamma" -> gamma)
  *            val alpha  = params -> "alpha"
  *            
  * }}}
  */

class FuncParams[V]
{

  private var rpar_ = scala.collection.mutable.Map[String, V]()
  private var ipar_ = scala.collection.mutable.Map[String, Int]()

  /** Add a named parameter of type V
    *
    * @param tv : key-value pair with the value of type V
    * @return updated instance
    */
  def +\(tv:(String, V))   = {
    rpar_ += tv
    this
  }

  /** Add a list of named parameters of type V
    *
    * @param tv : List of key value pairs
    * @return : updated instance
    */
  def +\(tv:(String, V)*)   = {
    for (e<-tv) yield rpar_ += e
    this
  }

  /** Add a named parameter of type V
    *
    * @param tv
    * @return updated instance
    */
  def rpar(tv:(String, V)) = +\(tv)

  /** Add a named Integer parameter
    *
    * @param tv : named integer parameter.
    * @return : updated instance
    */

  def +|(tv:(String,Int))  = {
    ipar_ += tv
    this}

  /** Add a list of named integer parameter
    *
    * @param tv : list of named integer parameters
    * @return
    */
  def +|(tv:(String,Int)*)  = {
    for (e <- tv) yield ipar_ += e
    this

  }
  /** Add a named Integer parameter
    *
    * @param tv : named integer parameter.
    * @return : updated instance
    */

  def ipar(tv:(String, Int)) = +|(tv)

  /** get the value of an integer parameter
    *
    * @param vv : parameter name
    * @return : value of the parameter
    */
  def ipar(vv:String)      = ipar_(vv)

  /** get the value of an integer parameter
    *
    * @param vv : parameter name
    * @return : value of the parameter
    */
  def ~>(vv:String)        = ipar(vv)

  /** get the value of a parameter of type V
    *
    * @param vv : parameter name
    * @return : value of the parameter
    */
  def rpar(vv:String)      = rpar_(vv)

  /** get the value of a parameter of type V
    *
    * @param vv : parameter name
    * @return : value of the parameter
    */
  def ->(vv:String):V      = rpar(vv)

  /** check the presenc eof parameters
    *
    * @return boolean indicating if there are no parameters
    */
  def isEmpty:Boolean = rpar_.size == 0 &&  ipar_.size == 0

}

/** Factory object for function parameters.
  *
  * See example under the class documentation
  *
  */
object FuncParams
{
  //  def /\[U](pp:(String , U)) = {
  //    val v = new FP[U]
  //    v /\ pp
  //  }

  def +\[V](pp:(String , V)*) = {
    val v = new FuncParams[V]
    for (p <- pp) yield {
      v +\ p
    }
    v
  }

  /** Create an instance for a list of named parameters
    *
    * @param pp  : List of named integer parameters
    * @tparam V  : Type of parameter to hold
    * @return    : Instance
    */
  def +|[V](pp:(String , Int)*) = {
    val v = new FuncParams[V]
    for (p <- pp) yield {
      v +| p
    }
    v
  }

  /** Create an instance for a list of named parameters
    *
    * @param pp : list of named parameters of type V
    * @tparam V : parameter data type
    * @return   : fucntion parameter instance
    */
  def apply[V](pp:(String, V)*) = {
    val v = new FuncParams[V]
    for (p <- pp) yield v +\ p
    v
  }

  /** create an empty instance
    *
    * @tparam U : data type
    * @return    : empty instance
    */
  def apply[U]() = new FuncParams[U]

  /** create an empty instance
    *
    *
   */
  def empty[U]   = new FuncParams[U]
}
