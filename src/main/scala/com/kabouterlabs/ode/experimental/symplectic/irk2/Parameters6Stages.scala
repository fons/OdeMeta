package com.kabouterlabs.ode.experimental.symplectic.irk2
/**
  * Created by fons on 5/25/17.
  */
/*
 C(1)= 0.33765242898423986094E-01
         C(2)= 0.16939530676686774317E+00
         C(3)= 0.38069040695840154568E+00
         C(4)= 0.61930959304159845432E+00
         C(5)= 0.83060469323313225683E+00
         C(6)= 0.96623475710157601391E+00
         B(1)= 0.85662246189585172520E-01
         B(2)= 0.18038078652406930378E+00
         B(3)= 0.23395696728634552369E+00
         B(4)= 0.23395696728634552369E+00
         B(5)= 0.18038078652406930378E+00
         B(6)= 0.85662246189585172520E-01
         BC(1)= 0.82769839639769234611E-01
         BC(2)= 0.14982512785597570103E+00
         BC(3)= 0.14489179419935320895E+00
         BC(4)= 0.89065173086992314743E-01
         BC(5)= 0.30555658668093602753E-01
         BC(6)= 0.28924065498159379092E-02
         aa(0)(1)= 0.90625420195651151857E-03
         aa(0)(2)= -0.72859711612531400024E-03
         aa(0)(3)= 0.79102695861167691135E-03
         aa(0)(4)= -0.70675390218535384182E-03
         aa(0)(5)= 0.45647714224056921122E-03
         aa(0)(6)= -0.14836147050330408643E-03
         aa(1)(1)= 0.11272367531794365387E-01
         aa(1)(2)= 0.39083482447840698486E-02
         aa(1)(3)= -0.14724868010943911900E-02
         aa(1)(4)= 0.10992669056588431310E-02
         aa(1)(5)= -0.67689040729401428165E-03
         aa(1)(6)= 0.21677950347174141516E-03
         aa(2)(1)= 0.30008019623627547434E-01
         aa(2)(2)= 0.36978289259468146662E-01
         aa(2)(3)= 0.65490339168957822692E-02
         aa(2)(4)= -0.16615098173008262274E-02
         aa(2)(5)= 0.84753461862041607649E-03
         aa(2)(6)= -0.25877462623437421721E-03
         aa(3)(1)= 0.49900269650650898941E-01
         aa(3)(2)= 0.82003427445271620462E-01
         aa(3)(3)= 0.54165111295060067982E-01
         aa(3)(4)= 0.65490339168957822692E-02
         aa(3)(5)= -0.11352871017627472322E-02
         aa(3)(6)= 0.28963081055952389031E-03
         aa(4)(1)= 0.68475836671617248304E-01
         aa(4)(2)= 0.11859257878058808400E+00
         aa(4)(3)= 0.10635984886129551097E+00
         aa(4)(4)= 0.47961474042181382443E-01
         aa(4)(5)= 0.39083482447840698486E-02
         aa(4)(6)= -0.34600839001342442657E-03
         aa(5)(1)= 0.79729071619449992615E-01
         aa(5)(2)= 0.14419100392702230613E+00
         aa(5)(3)= 0.13628542646896576408E+00
         aa(5)(4)= 0.81956586217401900627E-01
         aa(5)(5)= 0.23736460480774324642E-01
         aa(5)(6)= 0.90625420195651151857E-03
         e(0)1)= -0.16761132335280609813E-01
         e(0)2)= 0.10201050166615899799E-01
         e(0)3)= -0.58593121685075943100E-02
         e(0)4)= -0.11907383391366998251E-03
         e(0)5)= 0.10615611118132982241E-01
         e(0)6)= -0.30692054230989138447E-01
         e(0)7)= 0.10615182045216224925E-01
         e(0)8)= 0.22586707045496892369E-01
         e(0)9)= -0.16931992776201068110E-04
         e(1)1)= 0.10671755276327262128E-01
         e(1)2)= -0.51098203653251450913E-02
         e(1)3)= 0.16062647299186369205E-03
         e(1)4)= 0.64818802653621866868E-02
         e(1)5)= -0.12132386914873895089E-01
         e(1)6)= -0.99709737725909584834E-02
         e(1)7)= -0.70287093442894942752E-02
         e(1)8)= 0.31243249755879001843E-01
         e(1)9)= 0.31763603839792897936E-04
         e(2)1)= -0.40875203230945019464E+00
         e(2)2)= 0.28214948905763253599E+00
         e(2)3)= -0.22612660499718519054E+00
         e(2)4)= 0.13640993962985420478E+00
         e(2)5)= 0.15888529591697266925E+00
         e(2)6)= -0.11667863471317749710E+01
         e(2)7)= 0.25224964119340060668E+00
         e(2)8)= 0.10440940643938620983E+01
         e(2)9)= 0.33914722176493324285E-03
         e(3)1)= -0.29437531285359759661E+01
         e(3)2)= 0.20017220470127690267E+01
         e(3)3)= -0.15383035791443948798E+01
         e(3)4)= 0.78114323215109899716E+00
         e(3)5)= 0.13930345104184182146E+01
         e(3)6)= -0.75958281612589849630E+01
         e(3)7)= 0.18220129530415584951E+01
         e(3)8)= 0.62663163493155487560E+01
         e(3)9)= 0.54279630166374655267E-02
         e(4)1)= -0.79572842006457093076E+01
         e(4)2)= 0.53527892762707449170E+01
         e(4)3)= -0.40049139768467199697E+01
         e(4)4)= 0.18326058141135591515E+01
         e(4)5)= 0.39753886181058367500E+01
         e(4)6)= -0.19423696478604790213E+02
         e(4)7)= 0.49362128400107292627E+01
         e(4)8)= 0.15601708062381928560E+02
         e(4)9)= 0.32142123424873719685E-01
         e(5)1)= -0.78463118056075171475E+01
         e(5)2)= 0.53580869574441241664E+01
         e(5)3)= -0.41476905275607763365E+01
         e(5)4)= 0.21275912797813913113E+01
         e(5)5)= 0.37642416878253538582E+01
         e(5)6)= -0.20329681631523484613E+02
         e(5)7)= 0.48515418060343387549E+01
         e(5)8)= 0.16604467346259915039E+02
         e(5)9)= 0.84559690262225766975E-01
         SM(1)= 0.00000000000000000000E+00
         SM(2)= 0.10000000000000000000E+01
         SM(3)= 0.17500000000000000000E+01
         AM(1)= 0.58080578375796358720E+05
         AM(2)= -0.33214989339522861968E+05
         AM(3)= 0.28376088288312020853E+05
         AM(4)= -0.27923430684614999462E+05
         AM(5)= 0.29743005589491042677E+05
         AM(6)= -0.15525927919158826444E+05
         AM(7)= -0.27700591278076171875E+03
         AM(8)= 0.73086943817138671875E+03
         AM(9)= 0.00000000000000000000E+00
 */
case class Parameters6Stages(hstep:Double) extends ParametersBase
{
  val ns = 6
  private val hstep2 = hstep * hstep

  val nsd = 6


  val C = Array(
    0.33765242898423986094E-01,
    0.16939530676686774317E+00,
    0.38069040695840154568E+00,
    0.61930959304159845432E+00,
    0.83060469323313225683E+00,
    0.96623475710157601391E+00
  ).map(_ * hstep)

  val B = Array(
    0.85662246189585172520E-01,
    0.18038078652406930378E+00,
    0.23395696728634552369E+00,
    0.23395696728634552369E+00,
    0.18038078652406930378E+00,
    0.85662246189585172520E-01
  ).map(_ * hstep)

  val BC = Array(
    0.82769839639769234611E-01,
    0.14982512785597570103E+00,
    0.14489179419935320895E+00,
    0.89065173086992314743E-01,
    0.30555658668093602753E-01,
    0.28924065498159379092E-02
  ).map(_ * hstep2)
  val AA = {
    val aa = Array.ofDim[Double](ns, ns)
    aa(0)(0) = 0.90625420195651151857E-03
    aa(0)(1) = -0.72859711612531400024E-03
    aa(0)(2) = 0.79102695861167691135E-03
    aa(0)(3) = -0.70675390218535384182E-03
    aa(0)(4) = 0.45647714224056921122E-03
    aa(0)(5) = -0.14836147050330408643E-03
    aa(1)(0) = 0.11272367531794365387E-01
    aa(1)(1) = 0.39083482447840698486E-02
    aa(1)(2) = -0.14724868010943911900E-02
    aa(1)(3) = 0.10992669056588431310E-02
    aa(1)(4) = -0.67689040729401428165E-03
    aa(1)(5) = 0.21677950347174141516E-03
    aa(2)(0) = 0.30008019623627547434E-01
    aa(2)(1) = 0.36978289259468146662E-01
    aa(2)(2) = 0.65490339168957822692E-02
    aa(2)(3) = -0.16615098173008262274E-02
    aa(2)(4) = 0.84753461862041607649E-03
    aa(2)(5) = -0.25877462623437421721E-03
    aa(3)(0) = 0.49900269650650898941E-01
    aa(3)(1) = 0.82003427445271620462E-01
    aa(3)(2) = 0.54165111295060067982E-01
    aa(3)(3) = 0.65490339168957822692E-02
    aa(3)(4) = -0.11352871017627472322E-02
    aa(3)(5) = 0.28963081055952389031E-03
    aa(4)(0) = 0.68475836671617248304E-01
    aa(4)(1) = 0.11859257878058808400E+00
    aa(4)(2) = 0.10635984886129551097E+00
    aa(4)(3) = 0.47961474042181382443E-01
    aa(4)(4) = 0.39083482447840698486E-02
    aa(4)(5) = -0.34600839001342442657E-03
    aa(5)(0) = 0.79729071619449992615E-01
    aa(5)(1) = 0.14419100392702230613E+00
    aa(5)(2) = 0.13628542646896576408E+00
    aa(5)(3) = 0.81956586217401900627E-01
    aa(5)(4) = 0.23736460480774324642E-01
    aa(5)(5) = 0.90625420195651151857E-03
    aa.map(_.map(_ * hstep2))

  }
  val E = {
    val e = Array.ofDim[Double](ns, ns + nm)

    e(0)(0) = -0.16761132335280609813E-01
    e(0)(1) = 0.10201050166615899799E-01
    e(0)(2) = -0.58593121685075943100E-02
    e(0)(3) = -0.11907383391366998251E-03
    e(0)(4) = 0.10615611118132982241E-01
    e(0)(5) = -0.30692054230989138447E-01
    e(0)(6) = 0.10615182045216224925E-01
    e(0)(7) = 0.22586707045496892369E-01
    e(0)(8) = -0.16931992776201068110E-04
    e(1)(0) = 0.10671755276327262128E-01
    e(1)(1) = -0.51098203653251450913E-02
    e(1)(2) = 0.16062647299186369205E-03
    e(1)(3) = 0.64818802653621866868E-02
    e(1)(4) = -0.12132386914873895089E-01
    e(1)(5) = -0.99709737725909584834E-02
    e(1)(6) = -0.70287093442894942752E-02
    e(1)(7) = 0.31243249755879001843E-01
    e(1)(8) = 0.31763603839792897936E-04
    e(2)(0) = -0.40875203230945019464E+00
    e(2)(1) = 0.28214948905763253599E+00
    e(2)(2) = -0.22612660499718519054E+00
    e(2)(3) = 0.13640993962985420478E+00
    e(2)(4) = 0.15888529591697266925E+00
    e(2)(5) = -0.11667863471317749710E+01
    e(2)(6) = 0.25224964119340060668E+00
    e(2)(7) = 0.10440940643938620983E+01
    e(2)(8) = 0.33914722176493324285E-03
    e(3)(0) = -0.29437531285359759661E+01
    e(3)(1) = 0.20017220470127690267E+01
    e(3)(2) = -0.15383035791443948798E+01
    e(3)(3) = 0.78114323215109899716E+00
    e(3)(4) = 0.13930345104184182146E+01
    e(3)(5) = -0.75958281612589849630E+01
    e(3)(6) = 0.18220129530415584951E+01
    e(3)(7) = 0.62663163493155487560E+01
    e(3)(8) = 0.54279630166374655267E-02
    e(4)(0) = -0.79572842006457093076E+01
    e(4)(1) = 0.53527892762707449170E+01
    e(4)(2) = -0.40049139768467199697E+01
    e(4)(3) = 0.18326058141135591515E+01
    e(4)(4) = 0.39753886181058367500E+01
    e(4)(5) = -0.19423696478604790213E+02
    e(4)(6) = 0.49362128400107292627E+01
    e(4)(7) = 0.15601708062381928560E+02
    e(4)(8) = 0.32142123424873719685E-01
    e(5)(0) = -0.78463118056075171475E+01
    e(5)(1) = 0.53580869574441241664E+01
    e(5)(2) = -0.41476905275607763365E+01
    e(5)(3) = 0.21275912797813913113E+01
    e(5)(4) = 0.37642416878253538582E+01
    e(5)(5) = -0.20329681631523484613E+02
    e(5)(6) = 0.48515418060343387549E+01
    e(5)(7) = 0.16604467346259915039E+02
    e(5)(8) = 0.84559690262225766975E-01
    e.map(_.map(_ * hstep2))
  }

  val SM = Array(
    0.00000000000000000000E+00,
    0.10000000000000000000E+01,
    0.17500000000000000000E+01
  )

  val AM = Array(
    0.58080578375796358720E+05,
    -0.33214989339522861968E+05,
    0.28376088288312020853E+05,
    -0.27923430684614999462E+05,
    0.29743005589491042677E+05,
    -0.15525927919158826444E+05,
    -0.27700591278076171875E+03,
    0.73086943817138671875E+03,
    0.00000000000000000000E+00).zipWithIndex.map(x => if (x._2 < 2) x._1 else x._1 * hstep)
}
