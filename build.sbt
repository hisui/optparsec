name := "optparsec"

version := "0.2"

scalaVersion := "2.11.11"

scalacOptions in Compile := Seq("-feature" /* , "-Xlog-implicits" */)

initialCommands := """
import jp.segfault.optparsec._
import N._
import HLists._
"""

assembleArtifact in assemblyPackageScala := false

assemblyJarName in assembly := "optparsec-0.2.jar"
