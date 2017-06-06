
package com.kabouterlabs.ode.experimental.symplectic.irk2



case class Parameters2Stages(hstep:Double) extends ParametersBase
{
  
  val ns = 2
  private val hstep2 = hstep * hstep
  val nsd  = 6

  val C:Array[Double] = Array(
    0.21132486540518711775E+00,
    0.78867513459481288225E+00
  ).map(_ * hstep)

  val B = Array (
    0.50000000000000000000E+00,
    0.50000000000000000000E+00
  ).map(_*hstep)

  val BC = Array (
    0.39433756729740644113E+00,
    0.10566243270259355887E+00
  ).map(_*hstep2)

  val AA = {
    val aa = Array.ofDim[Double](2, 2)
    aa(0)(0) = 0.41666666666666666667E-01
    aa(0)(1) = -0.19337567297406441127E-01
    aa(1)(0) = 0.26933756729740644113E+00
    aa(1)(1) = 0.41666666666666666667E-01
    aa.map(_.map(_ * hstep2))
  }
  
  val E = {
    val e = Array.ofDim[Double](ns, ns+nm)
    e(0)(0) = -0.28457905077110526160E-02
    e(0)(1) = -0.63850024471784160410E-01
    e(0)(2) = 0.48526095198694517563E-02
    e(0)(3) = 0.11305688530429939012E+00
    e(0)(4) = -0.28884580475413403312E-01
    e(1)(0) = 0.41122751744511433137E-01
    e(1)(1) = -0.18654814888622834132E+00
    e(1)(2) = -0.18110185277445209332E-01
    e(1)(3) = 0.36674109449368040786E+00
    e(1)(4) = 0.10779872188955481745E+00
    e.map(_.map(_ * hstep2))
    //    e.map(_.map(_ * hstep2)).map(_.zipWithIndex.map((x) => if (x._2 < 2) x._1 else x._1 * hstep2))
//    val e1 = (for (is <- Range(0, ns)) yield {
//      (for (js <- Range(0,ns+nm)) yield {
//        if (is < ns) hstep2*e(is)(js) else e(is)(js)
//      }).toArray
//    }).toArray
//    e1.map(_.zipWithIndex.map((x) => if (x._2 < 2) x._1 else x._1 * hstep2))
  }
  val SM = Array(
    0.00000000000000000000E+00,
    0.10000000000000000000E+01,
    0.16000000000000000000E+01
  )

  val AM = Array (
    0.25279583039343438291E+02,
    -0.86907830393434382912E+01,
    -0.80640000000000000000E+00,
    0.29184000000000000000E+01,
    0.00000000000000000000E+00
  ).zipWithIndex.map( x => if (x._2 < 2) x._1 else x._1*hstep )

}

