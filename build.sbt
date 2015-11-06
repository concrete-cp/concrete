name := "concrete"

organization := "fr.univ-valenciennes"

version := "3.0-B3-SNAPSHOT"

scalaVersion := "2.11.7"

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
	"fr.univ-valenciennes" %% "cspom" % "2.7-SNAPSHOT",
	"org.postgresql" % "postgresql" % "9.4-1205-jdbc42",
	//"org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5",
	"org.ow2.sat4j" % "org.ow2.sat4j.pb" % "2.3.5",
	"com.typesafe.slick" %% "slick" % "3.1.0",
	"org.apache.commons" % "commons-math3" % "3.5",
	//"org.jcp" % "jsr331" % "1.1.1",
	"org.scalatest" %% "scalatest" % "2.2.5" % "test",
	"org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
	"com.storm-enroute" %% "scalameter" % "0.7" % "test"
	
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

enablePlugins(JavaAppPackaging)

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


excludeFilter in packageBin in unmanagedResources := "logback.xml"
