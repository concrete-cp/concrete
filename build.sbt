name := "concrete"

organization := "fr.univ-valenciennes"

maintainer := "Julien Vion <julien.vion@univ-valenciennes.fr>"

packageSummary := "Concrete is a Scala CSP Solving API"

packageDescription := "Concrete is a Scala CSP Solving API"

version := "3.2-SNAPSHOT"

scalaVersion := "2.11.8"

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
	"fr.univ-valenciennes" %% "cspom" % "2.9-SNAPSHOT",
	"org.postgresql" % "postgresql" % "9.4.1212",
	//"org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5",
	"org.ow2.sat4j" % "org.ow2.sat4j.pb" % "2.3.5",
	"com.typesafe.slick" %% "slick" % "3.1.1",
	"com.github.tminglei" %% "slick-pg" % "0.14.3",
	"com.typesafe" % "config" % "1.3.1",
	//"org.apache.commons" % "commons-math3" % "3.6.1",
	//"org.jcp" % "jsr331" % "1.1.1",
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
	"com.storm-enroute" %% "scalameter" % "0.8.1" % "test",
	"com.github.davidmoten" % "rtree" % "0.7.6"
	)

scalacOptions ++= Seq(
  "-optimise"
, "-Xdisable-assertions"
//	"-deprecation", 
//	"-unchecked", 
,	"-Xlint" 
//	"-feature",
, 	"-Ywarn-unused-import"
)

enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

mainClass in Compile := Some("concrete.runner.FZConcrete")

testOptions in Test += Tests.Argument("-oDF")

publishTo :=  {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

licenses := Seq("LGPL 3.0" -> url("https://www.gnu.org/licenses/lgpl-3.0.txt"))

homepage := Some(url("https://github.com/concrete-cp/concrete"))

publishMavenStyle := true

pomExtra in Global := {
  <scm>
    <connection>scm:git:github.com/concrete-cp/concrete.git</connection>
    <url>github.com/concrete-cp/concrete.git</url>
  </scm>

  <developers>
    <developer>
      <id>scand1sk</id>
      <name>Julien Vion</name>
      <url>http://vion.free.fr/perso</url>
    </developer>
  </developers>
}

		
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

sourceGenerators in Scapegoat <+= (sourceManaged in Compile, version, name) map { (d, v, n) =>
  val file = d / "concrete" / "Info.scala"
  IO.write(file, s"""package concrete
    |object Info {
    |  val version = "$v"
    |  val name = "$n"
    |}
    |""".stripMargin)
  Seq(file)
}


excludeFilter in packageBin in unmanagedResources := "logback.xml" || "reference.conf"

import NativePackagerHelper._

mappings in Universal ++= directory((resourceDirectory in Compile).value / "conf")
    
mappings in Universal ++= directory((resourceDirectory in Compile).value / "mzn_lib")
   

scapegoatVersion := "1.2.1"
