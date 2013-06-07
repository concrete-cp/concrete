import com.typesafe.sbt.SbtStartScript
import AssemblyKeys._ // put this at the top of the file

name := "concrete"

organization := "fr.univ-valenciennes.concrete"

version := "1.0.1-SNAPSHOT"

scalaVersion := "2.10.1"

resolvers += "Concrete repository" at "http://scand1sk.github.io/concrete/repository"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
	"fr.univ-valenciennes.concrete" %% "cspom" % "1.3-SNAPSHOT",
	"postgresql" % "postgresql" % "9.1-901-1.jdbc4",
	"org.sat4j" % "org.sat4j.core" % "2.3.0",
	"junit" % "junit" % "4.11" % "test",
	"com.novocode" % "junit-interface" % "0.8" % "test->default",
	"com.typesafe.slick" %% "slick" % "1.0.0",
	"org.slf4j" % "slf4j-nop" % "1.6.4",
	"org.jcp" % "jsr331" % "1.1.1"
	)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-Xlint", "-Xdisable-assertions")

org.scalastyle.sbt.ScalastylePlugin.Settings

seq(SbtStartScript.startScriptForClassesSettings: _*)

assemblySettings

mainClass in assembly := Some("concrete.XCSPConcrete")

test in assembly := {}

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))
