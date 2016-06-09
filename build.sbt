name := "monteCarloCards"

version := "1.0"

scalaVersion := "2.11.8"


val snapshots = "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.typelevel" %% "cats" % "0.6.0"

resolvers += snapshots
    