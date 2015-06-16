name := "concrete"

organization := "fr.univ-valenciennes.concrete"

version := "2.0-B1-SNAPSHOT"

scalaVersion := "2.11.6"


// For JSR331, CSPOM and its dependencies
resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"



testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}


libraryDependencies ++= Seq(
	"fr.univ-valenciennes.concrete" %% "cspom" % "2.6-SNAPSHOT",
	"org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
	//"org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5",
	"org.ow2.sat4j" % "org.ow2.sat4j.pb" % "2.3.5",
	
	"com.typesafe.slick" %% "slick" % "3.0.0",
	//"org.jcp" % "jsr331" % "1.1.1",
	"org.scalatest" %% "scalatest" % "2.2.5" % "test",
	"org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
	"com.storm-enroute" %% "scalameter" % "0.6" % "test"
	)

scalacOptions ++= Seq("-optimise"
//	      , "-Xdisable-assertions"
	      , "-target:jvm-1.7"
//	"-deprecation", 
//	"-unchecked", 
//	"-optimise", 
//	"-Xlint", 
//	
//	"-feature",
//	"-Yinline-warnings"
)

//javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

enablePlugins(JavaAppPackaging)

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

excludeFilter in packageBin in unmanagedResources := "logback.xml"


EclipseKeys.eclipseOutput in Compile := Some("target/scala-2.11/classes")
EclipseKeys.eclipseOutput in Test := Some("target/scala-2.11/test-classes")