name := "OdeMeta"

version := "0.1.0-SNAPSHOT"

organization      := "com.kabouterlabs"

scalaVersion := "2.11.8"


publishMavenStyle := true

// https://mvnrepository.com/artifact/com.nativelibs4java/bridj
libraryDependencies += "com.nativelibs4java" % "bridj" % "0.7.0"

libraryDependencies += "com.kabouterlabs" % "JavaOdeInt" % "DEV-SNAPSHOT"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"


resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
resolvers += Resolver.mavenLocal



