name := "cspfj"

organization := "fr.univ-valenciennes.cspfj"

version := "1.0.1-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers += "CSP4J repository" at "http://cspfj.sourceforge.net/repository"

libraryDependencies ++= Seq(
	"fr.univ-valenciennes.cspfj" %% "cspom" % "1.3-SNAPSHOT",
	"org.sat4j" % "org.sat4j.core" % "2.3.0",
	"junit" % "junit" % "4.10" % "test"
	)

