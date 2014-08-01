import sbt._
import Keys._

object ApplicationBuild extends Build {

  val appName         = "reblog"
  val appVersion      = "1.0-SNAPSHOT"
  
  val appDependencies = Seq(
    // Add your project dependencies here,
    // jdbc,
    // anorm,
    "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.akka23-SNAPSHOT",
    "org.planet42" %% "laika" % "0.4.0",
    "rome" % "rome" % "1.0"
  )


  val main = Project(appName, file(".")).enablePlugins(play.PlayScala).settings(
    version := appVersion,
    libraryDependencies ++= appDependencies,
    resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
    // Add your own project settings here
  )
}
