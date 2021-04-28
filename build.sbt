lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.example",
      scalaVersion := "2.12.13"
    )
  ),
  name := "scalatest-example"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test

resolvers += "stephenjudkins-bintray" at "https://dl.bintray.com/stephenjudkins/maven"

libraryDependencies += "ps.tricerato" % "pureimage_2.11" % "0.1.2"
