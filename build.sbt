import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

name := "concrete"

organization := "fr.univ-valenciennes.concrete"

version := "1.3-SNAPSHOT"

scalaVersion := "2.11.2"


// For JSR331, CSPOM and its dependencies
resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"



testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}


libraryDependencies ++= Seq(
	"fr.univ-valenciennes.concrete" %% "cspom" % "2.4-SNAPSHOT",
	"junit" % "junit" % "4.11" % "test",
	"org.postgresql" % "postgresql" % "9.3-1102-jdbc41",
	"org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5",
	"com.typesafe.slick" %% "slick" % "2.1.0",
	"org.jcp" % "jsr331" % "1.1.1",
	"org.scalatest" %% "scalatest" % "2.2.2" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
	"com.storm-enroute" %% "scalameter" % "0.6" % "test"
	)

scalacOptions ++= Seq("-optimise"
	      , "-Xdisable-assertions"
//	"-deprecation", 
//	"-unchecked", 
//	"-optimise", 
//	"-Xlint", 
//	
//	"-feature",
//	"-Yinline-warnings"
)



packageArchetype.java_application

mainClass in Compile := Some("concrete.runner.FZConcrete")

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))
		
sourceGenerators in Compile <+= (sourceManaged in Compile, version, name) map { (d, v, n) =>
  val file = d / "concrete" / "Info.scala"
  IO.write(file, s"""package concrete
    |object Info {
    |  val version = "$v"
    |  val name = "$n"
    |}
    |""".stripMargin)
  Seq(file)
}