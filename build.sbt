val dottyVersion = "0.19.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "guide-to-scala3-examples",
    version := "0.1.0",
    scalaVersion := dottyVersion
  )
