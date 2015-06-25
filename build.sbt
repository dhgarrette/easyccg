import com.typesafe.sbt.SbtStartScript

name := "easyccg"

version := "0.0.1"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
  "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "dhg" % "scala-util_2.11" % "0.0.2-SNAPSHOT",
  "dhg" % "condor-util_2.11" % "0.0.2-SNAPSHOT",
  "edu.mit" % "jwi" % "2.3.3",
  "com.google.guava" % "guava" % "15.0",
  "com.lexicalscope.jewelcli" % "jewelcli" % "0.7.6",
  "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.3",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test") //switch to ScalaTest at some point...

seq(SbtStartScript.startScriptForClassesSettings: _*)

SbtStartScript.stage in Compile := Unit

scalacOptions ++= Seq("-deprecation", "-feature")

initialCommands in console := "import dhg.util._"               

