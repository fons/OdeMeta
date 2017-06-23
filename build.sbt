name := "OdeMeta"

version := "0.1.0-SNAPSHOT"

organization      := "com.kabouterlabs"

scalaVersion := "2.11.8"


publishMavenStyle := true

// https://mvnrepository.com/artifact/com.nativelibs4java/bridj
//libraryDependencies += "com.nativelibs4java" % "bridj" % "0.7.1-SNAPSHOT"


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

//resolvers += "jitpack" at "https://jitpack.io"



//libraryDependencies += "com.github.fons" % "JavaOdeInt" % "master-SNAPSHOT"


  


