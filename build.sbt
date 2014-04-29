name := "concrete"

organization := "fr.univ-valenciennes.concrete"

version := "1.0.1-SNAPSHOT"

scalaVersion := "2.11.0"

resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"

resolvers += "typesafe-relases" at "http://repo.typesafe.com/typesafe/releases"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

testOptions in Test += Tests.Argument("-oD")

libraryDependencies ++= Seq(
	"fr.univ-valenciennes.concrete" %% "cspom" % "2.0.3",
	"junit" % "junit" % "4.11" % "test",
	"org.postgresql" % "postgresql" % "9.3-1101-jdbc41",
	"org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.4",
	"com.typesafe.slick" %% "slick" % "2.1.0-M1",
	"org.jcp" % "jsr331" % "1.1.1",
	"org.scalatest" %% "scalatest" % "2.1.3" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
	)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-Xlint", "-Xdisable-assertions")

org.scalastyle.sbt.ScalastylePlugin.Settings

com.typesafe.sbt.SbtNativePackager.packageArchetype.java_application

mainClass in Compile := Some("concrete.runner.FZConcrete")

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))
		
