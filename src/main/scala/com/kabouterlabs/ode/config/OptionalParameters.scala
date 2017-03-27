package com.kabouterlabs.ode.config

/**
  * Created by fons on 3/14/17.
  */
sealed trait OptionalParameterType
object OptionalParameterType
{
  case object INITIAL_STEP_SIZE extends OptionalParameterType
  case object MAX_STEP_SIZE     extends OptionalParameterType
  case object MIN_STEP_SIZE     extends OptionalParameterType
  case object RWORK             extends OptionalParameterType
  case object IWORK             extends OptionalParameterType

  case object VERBOSE            extends OptionalParameterType
  case object MAX_STEPS          extends OptionalParameterType
  case object MIN_ORDER          extends OptionalParameterType
  case object MAX_ORDER          extends OptionalParameterType
  case object MAX_ORDER_STIFF    extends OptionalParameterType
  case object MAX_ORDER_NONSTIFF extends OptionalParameterType

  case object DIAGNOSTICS        extends OptionalParameterType
}

case class OptionalParameters(initialStepSize:Option[Double], minStepSize:Option[Double], maxStepSize:Option[Double],
                              maxSteps:Option[Int], minOrder:Option[Int], maxOrder:Option[Int], maxOrderStiff:Option[Int], maxOrderNonStiff:Option[Int],
                              diagnostics:Option[Boolean],
                              rwork:Option[Array[Double]], iwork:Option[Array[Int]])
{
  def this() {this(None,None,None, None,None,None, None,None,None,None, None)}

  private def strd[U](desc:String, d:Option[U]) = {
    d match {
      case Some(x) => "{" + desc + " : " + x.toString + "} "
      case None    => ""
    }
  }


  private def stra[U](desc:String, d:Option[Array[U]]) = {
    d match {
      case Some(x) => "{" + desc + " : " + x.mkString(",") + "} "
      case None => ""
    }
  }


  override def toString: String = {
    val line = strd("initial step size", initialStepSize) +
      strd("minimum step size", minStepSize) +
      strd("maximum step size", maxStepSize) +
      strd("maximum steps taken", maxSteps) +
      strd("maximum order", minOrder) +
      strd("maximum order", maxOrder) +
      strd("maximum order for stiff problems", maxOrderStiff) +
      strd("maximum order for non-stiff problems", maxOrderNonStiff) +
      strd("diagnostics : ", diagnostics) +
      stra("real work array ", rwork) +
      stra("real work array ", iwork)
      if (line.length > 10) "Optional parameters : " + line else "No optional parameters provided"
  }
  def this(initialStep:Option[Double], minStepSize:Option[Double], maxStepSize:Option[Double],
             maxSteps:Option[Int],minOrder:Option[Int], maxOrder:Option[Int], maxOrderStiff:Option[Int], maxOrderNonStiff:Option[Int],
           diagnostics:Option[Boolean]
            ) {
      this(initialStep,minStepSize,maxStepSize,maxSteps,minOrder, maxOrder,maxOrderStiff,maxOrderNonStiff,diagnostics,None,None)
    }

    def this(r:Option[Array[Double]],l:Option[Array[Int]]) {
      this(None,None,None,None,None,None,None,None,None,r,l)
    }

    def ++(tp:OptionalParameterType,  vl:Double) = (tp, rwork) match {
      case (_, Some(_)) => this
      case (OptionalParameterType.INITIAL_STEP_SIZE,None) => new OptionalParameters(Some(vl), minStepSize, maxStepSize,maxSteps,minOrder, maxOrder,maxOrderStiff,
        maxOrderNonStiff, diagnostics)
      case (OptionalParameterType.MIN_STEP_SIZE,    None) => new OptionalParameters(initialStepSize, Some(vl), maxStepSize,maxSteps,minOrder, maxOrder,
        maxOrderStiff,maxOrderNonStiff, diagnostics)
      case (OptionalParameterType.MAX_STEP_SIZE,    None) => new OptionalParameters(initialStepSize, minStepSize, Some(vl),maxSteps,minOrder, maxOrder,maxOrderStiff,
        maxOrderNonStiff, diagnostics)
      case (_,_) => this
    }

    def ++(tp:OptionalParameterType,  vl:Int) = (tp, iwork) match {
      case (_, Some(_)) => this

      case (OptionalParameterType.MAX_STEPS,None) => new OptionalParameters(initialStepSize, minStepSize, maxStepSize,Some(vl),minOrder, maxOrder,
        maxOrderStiff,maxOrderNonStiff, diagnostics)

      case (OptionalParameterType.MIN_ORDER,None) => new OptionalParameters(initialStepSize, minStepSize, maxStepSize,maxSteps,Some(vl),maxOrder,
        maxOrderStiff,maxOrderNonStiff, diagnostics)

      case (OptionalParameterType.MAX_ORDER,None) => new OptionalParameters(initialStepSize, minStepSize, maxStepSize,maxSteps,minOrder, Some(vl),
        maxOrderStiff,maxOrderNonStiff, diagnostics)

      case (OptionalParameterType.MAX_ORDER_STIFF,None) => new OptionalParameters(initialStepSize, minStepSize, maxStepSize,maxSteps,minOrder, maxOrder,
        Some(vl),maxOrderNonStiff, diagnostics)

      case (OptionalParameterType.MAX_ORDER_NONSTIFF,None) => new OptionalParameters(initialStepSize, minStepSize, maxStepSize,maxSteps,minOrder, maxOrder,
        maxOrderStiff,Some(vl), diagnostics)

      case (_,_) => this
    }
  def ++ (tp:OptionalParameterType, vl:Boolean) = tp match {
    case OptionalParameterType.DIAGNOSTICS => new OptionalParameters(initialStepSize, minStepSize, maxStepSize,maxSteps,minOrder,maxOrder,
      maxOrderStiff,maxOrderNonStiff, Some(vl))
    case _ => this
  }
  def ++ (tp:OptionalParameterType, vl:Array[Double])  =  tp match {
       case OptionalParameterType.RWORK =>  new OptionalParameters(Some(vl), iwork)
       case _ => this
   }

    def ++ (tp:OptionalParameterType, vl:Array[Int])  =  tp match {
      case OptionalParameterType.RWORK =>  new OptionalParameters(rwork, Some(vl))
      case _ => this
    }


}

object OptionalParameters
{
  def apply() = new OptionalParameters()

  def apply(optionalParameterType: OptionalParameterType, v:Double) = new OptionalParameters() ++ (optionalParameterType, v)

  def apply(optionalParameterType: OptionalParameterType, v:Int) = new OptionalParameters() ++ (optionalParameterType, v)

  def apply(optionalParameterType: OptionalParameterType, v:Boolean) = new OptionalParameters() ++ (optionalParameterType, v)
}