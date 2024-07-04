ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

mainClass in Compile := Some("TodoApp")

lazy val root = (project in file("."))
  .settings(
    name := "todo-list"
  )
