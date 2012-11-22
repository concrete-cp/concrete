import sbt._
import Keys._

object cspom extends Build {

  lazy val root = Project(id = "cspfj", base = file(".")) dependsOn (cspom)

  lazy val cspom = RootProject(file("../cspom"))
}