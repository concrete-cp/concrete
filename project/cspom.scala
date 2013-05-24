import sbt._
import Keys._

object cspom extends Build {

  lazy val root = Project(id = "concrete", base = file(".")) dependsOn (cspom)

  lazy val cspom = RootProject(file("../cspom"))
}