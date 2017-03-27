package com.kabouterlabs.ode

/**
  * Created by fons on 1/17/17.
  */

class FuncParams[V]
{

  private var rpar_ = scala.collection.mutable.Map[String, V]()
  private var ipar_ = scala.collection.mutable.Map[String, Int]()

  def +\(tv:(String, V))   = {
    rpar_ += tv
    this
  }
  def +\(tv:(String, V)*)   = {
    for (e<-tv) yield rpar_ += e
    this
  }
  def rpar(tv:(String, V)) = +\(tv)

  def +|(tv:(String,Int))  = {
    ipar_ += tv
    this}

  def +|(tv:(String,Int)*)  = {
    for (e <- tv) yield ipar_ += e
    this
  }
  def ipar(tv:(String, Int)) = +|(tv)

  def ipar(vv:String)      = ipar_(vv)
  def ~>(vv:String)        = ipar(vv)

  def rpar(vv:String)      = rpar_(vv)
  def ->(vv:String):V      = rpar(vv)

  def isEmpty:Boolean = rpar_.size == 0 &&  ipar_.size == 0

}

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
  def +|[V](pp:(String , Int)*) = {
    val v = new FuncParams[V]
    for (p <- pp) yield {
      v +| p
    }
    v
  }
  def apply[V](pp:(String, V)*) = {
    val v = new FuncParams[V]
    for (p <- pp) yield v +\ p
    v
  }

  def apply[U]() = new FuncParams[U]
  def empty[U]   = new FuncParams[U]
}
