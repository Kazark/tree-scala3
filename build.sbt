lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    testFrameworks += new TestFramework("munit.Framework"),
    scalaVersion := "3.0.0-RC1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test
  )
