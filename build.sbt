name := "OdeMeta"

version := "0.1.0-SNAPSHOT"

organization      := "com.kabouterlabs"

scalaVersion := "2.12.11"
githubOwner := "fons"
githubRepository := "OdeMeta"
githubTokenSource := TokenSource.GitConfig("github.token")



publishMavenStyle := true

// https://mvnrepository.com/artifact/com.nativelibs4java/bridj
libraryDependencies += "com.nativelibs4java" % "bridj" % "0.7.1-SNAPSHOT"

//libraryDependencies += "com.nativelibs4java" % "bridj" % "0.7.0"

libraryDependencies += "com.kabouterlabs" % "JavaOdeInt" % "DEV-SNAPSHOT"

//libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3"
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.9"

//libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"

//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

// https://mvnrepository.com/artifact/ch.qos.logback/logback-classic
//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.9"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" 


//libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

// https://mvnrepository.com/artifact/org.scalanlp/breeze-viz_2.11
//libraryDependencies += "org.scalanlp" % "breeze-viz_2.11" % "0.13"

//libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"
//libraryDependencies += "org.scalanlp" %% "breeze-natives" % "1.0"

// doesn't work with 2.13.1
libraryDependencies += "org.scalanlp" %% "breeze-viz" % "1.0"




unmanagedSourceDirectories in Compile += baseDirectory.value / "src/examples"

scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)




  


