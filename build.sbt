name := "concrete"

organization := "fr.univ-valenciennes"

maintainer := "Julien Vion <julien.vion@univ-valenciennes.fr>"

packageSummary := "Concrete is a Scala CSP Solving API"

packageDescription := "Concrete is a Scala CSP Solving API"

version := "3.3.1"

scalaVersion := "2.12.2"

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
	"fr.univ-valenciennes" %% "cspom" % "2.14",
	"org.postgresql" % "postgresql" % "42.1.1",
	"org.ow2.sat4j" % "org.ow2.sat4j.pb" % "2.3.5",
	"com.typesafe.slick" %% "slick" % "3.2.0",
	"com.typesafe" % "config" % "1.3.1",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
	"com.storm-enroute" %% "scalameter" % "0.8.2" % "test",
	"com.github.davidmoten" % "rtree" % "0.8.0.1"
	)

scalacOptions ++= Seq(
  "-Xdisable-assertions",
	"-deprecation"
//	"-unchecked", 
,	"-Xlint" 
//	"-feature",
, 	"-Ywarn-unused-import"
)

enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

mainClass in Compile := Some("concrete.runner.XCSP3Concrete")

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

// excludeFilter in packageBin in unmanagedResources := "conf/logback.xml"

import NativePackagerHelper._

mappings in Universal ++= directory((baseDirectory in Compile).value / "conf")
    
mappings in Universal ++= directory((baseDirectory in Compile).value / "mzn_lib")
   
//mappings in Universal <+= (packageBin in Compile, baseDirectory ) map { (_, base) =>
//     val conf = base / "conf"
//     conf -> "conf"
//}


// scapegoatVersion := "1.2.1"
