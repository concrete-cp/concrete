name := "concrete"

organization := "com.github.concrete-cp"

maintainer := "Julien Vion <julien.vion@uphf.fr>"

packageSummary := "Concrete is a Scala CSP Solving API"

packageDescription := "Concrete is a Scala CSP Solving API"

version := "3.12.3"

scalaVersion := "2.13.5"

javaOptions in ThisBuild += "-Xss16M"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${(target in Test).value / "test-reports"}")

resolvers += Resolver.sonatypeRepo("snapshots")
// resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "com.github.concrete-cp" %% "cspom" % "3.1.2",
  "org.postgresql" % "postgresql" % "42.2.8",
  "com.typesafe.slick" %% "slick" % "3.3.2",
  "com.typesafe" % "config" % "1.4.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "com.github.davidmoten" % "rtree" % "0.8.7",
  "org.eclipse.collections" % "eclipse-collections" % "10.0.0",
)

scalacOptions ++= Seq(
  "-Xdisable-assertions",
  "-deprecation",
  //	"-unchecked", 
  "-Xlint",
  //	"-feature",
  // "-target:jvm-1.8",
)

// javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

cancelable in Global := true

logBuffered in Test := false
// testOptions in Test += Tests.Argument("-oDF")
fork in Test := true
fork := true

publishTo := sonatypePublishToBundle.value

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

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion,
      BuildInfoKey.action("buildTime") {
        System.currentTimeMillis
      }),
    buildInfoPackage := "concrete"
  )

bashScriptExtraDefines += """addJava "-Dconfig.file=${app_home}/../conf/application.conf""""
bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml""""
