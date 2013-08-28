import AssemblyKeys._

name := "optparsec"

version := "0.1"

scalaVersion := "2.10.2"

scalacOptions in Compile := Seq("-feature" /* , "-Xlog-implicits" */)

initialCommands := """
import jp.segfault.optparsec._
import N._
import HLists._
"""

assemblySettings

assembleArtifact in packageScala := false

jarName in assembly := "optparsec-0.1.jar"
