/*
 * How to publish (very short version)
 * 
 * - To the local ivy2 repository : sbt publishLocal  
 * 
 * - To SonaType (snap shot staging)
 *   + see http://www.scala-sbt.org/release/docs/Using-Sonatype.html
 *   + add gpg signing : addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")
 *   + add ossrh credentials in  ~/.sbt/0.13/sonatype.sbt 
 *   + addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")  
 *   + sbt clean/compile/publishSigned
 *    
 * 
 */
name := "OdeMeta"
//version := "0.1.0-SNAPSHOT"
version := "0.1.0"

useGpg := true
lazy val pgpPass = Option(System.getenv("pgp_pass"))

pgpPassphrase := {
  if (pgpPass.isDefined) {
    println("PGP password specified under settings : " + pgpPass.toString)
    pgpPass.map(_.toCharArray)
  } else {
    println("Could not find settings for a PGP passphrase.")
    println(s"pgpPass defined in environemnt: ${pgpPass.isDefined}")
    None
  }
}

organization      := "com.kabouterlabs"

scalaVersion := "2.11.8"

useGpg := true

publishMavenStyle := true

publishArtifact in Test := false

licenses := Seq("BSD-3-clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/fons/OdeMeta"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/fons/OdeMeta"),
    "scm:git@github.com:fons/OdeMeta.git"
  )
)

developers := List(
  Developer(
    id    = "fons",
    name  = "alfons haffmans",
    email = "fh@mohegan-skunkworks.com",
    url   = url("https://github.com/fons/")
  )
)

// https://mvnrepository.com/artifact/com.nativelibs4java/bridj
//libraryDependencies += "com.nativelibs4java" % "bridj" % "0.7.1-SNAPSHOT"
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

// https://mvnrepository.com/artifact/ch.qos.logback/logback-classic
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.9"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
// https://mvnrepository.com/artifact/org.scalanlp/breeze-viz_2.11
libraryDependencies += "org.scalanlp" % "breeze-viz_2.11" % "0.13"

// https://mvnrepository.com/artifact/com.kabouterlabs/JavaOdeInt
libraryDependencies += "com.kabouterlabs" % "JavaOdeInt" % "0.9.0"

// https://mvnrepository.com/artifact/com.kabouterlabs/JavaOdeIntLinux
libraryDependencies += "com.kabouterlabs" % "JavaOdeIntLinux" % "0.99.0"


resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
resolvers += Resolver.mavenLocal

unmanagedSourceDirectories in Compile += baseDirectory.value / "src/examples"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}








  


