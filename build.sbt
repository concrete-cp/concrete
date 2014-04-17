name := "concrete"

organization := "fr.univ-valenciennes.concrete"

version := "1.0.1-SNAPSHOT"

scalaVersion := "2.11.0-RC4"

resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"

resolvers += "typesafe-relases" at "http://repo.typesafe.com/typesafe/releases"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

testOptions in Test += Tests.Argument("-oD")

libraryDependencies ++= Seq(
	"fr.univ-valenciennes.concrete" %% "cspom" % "2.0.1",
	"postgresql" % "postgresql" % "9.1-901-1.jdbc4",
	"org.sat4j" % "org.sat4j.core" % "2.3.0",
	"junit" % "junit" % "4.11" % "test",
	"com.novocode" % "junit-interface" % "0.10" % "test->default",
	"com.typesafe.slick" %% "slick" % "2.1.0-SNAPSHOT",
	"org.jcp" % "jsr331" % "1.1.1",
	"org.scalatest" %% "scalatest" % "2.1.3" % "test"
	)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-Xlint", "-Xdisable-assertions")

org.scalastyle.sbt.ScalastylePlugin.Settings

com.typesafe.sbt.SbtNativePackager.packageArchetype.java_application

mainClass in Compile := Some("concrete.runner.FZConcrete")

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))
		
