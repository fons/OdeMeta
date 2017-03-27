package com.kabouterlabs.ode.config

/**
  * Created by fons on 3/10/17.
  */
case class DaeIndexVariables(index1:Option[Int], index2:Option[Int], index3:Option[Int])
{
  def this() {this(None,None,None)}
  def this(index1:Option[Int]) {this(index1, None, None)}
  def this(index1:Option[Int], index2:Option[Int]) {this(index1, index2, None)}

  //override def toString: String = "dao index variables "
}

object DaeIndexVariables
{
   def apply()             = new DaeIndexVariables()
   def apply(v:Int)        = new DaeIndexVariables(Some(v))
   def apply(v:Int, w:Int) = new DaeIndexVariables(Some(v), Some(w))
   def apply(v:Int, w:Int, u:Int) = new DaeIndexVariables(Some(v), Some(w), Some(u))
}