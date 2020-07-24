val dottyVersion = "0.25.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    testFrameworks += new TestFramework("munit.Framework"),
    scalaVersion := dottyVersion,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.9" % Test
  )
