organization := "me.lessis"

name := "stack"

version := "0.1.0-SNAPSHOT"

description := "docker container stacks"

crossScalaVersions := Seq("2.10.4", "2.11.1")

scalaVersion := crossScalaVersions.value.last

libraryDependencies ++= Seq(
  "me.lessis" %% "tugboat" % "0.2.0",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test")
