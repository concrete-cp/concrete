import com.typesafe.sbt.SbtStartScript
import AssemblyKeys._ // put this at the top of the file

name := "cspfj"

organization := "fr.univ-valenciennes.cspfj"

version := "1.0.1-SNAPSHOT"

scalaVersion := "2.10.1"

resolvers += "CSP4J repository" at "http://cspfj.sourceforge.net/repository"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
	//"fr.univ-valenciennes.cspfj" %% "cspom" % "1.3-SNAPSHOT",
	"postgresql" % "postgresql" % "9.1-901-1.jdbc4",
	"org.sat4j" % "org.sat4j.core" % "2.3.0",
	"junit" % "junit" % "4.10" % "test",
	"com.novocode" % "junit-interface" % "0.8" % "test->default",
	"com.typesafe.slick" %% "slick" % "1.0.0",
	"org.slf4j" % "slf4j-nop" % "1.6.4"
	)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-Xlint", "-Xdisable-assertions")

org.scalastyle.sbt.ScalastylePlugin.Settings

seq(SbtStartScript.startScriptForClassesSettings: _*)

assemblySettings

mainClass in assembly := Some("concrete.XCSPConcrete")