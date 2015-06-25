import sbt._
import Keys._

object EasyccgBuild extends Build {

  lazy val main = Project(id = "easyccg", base = file(".")) dependsOn(ccg)
  
  lazy val ccg = Project("ccg", file("ccg"))

}
