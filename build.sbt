lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    testFrameworks += new TestFramework("munit.Framework"),
    scalaVersion := "3.0.0-RC2",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.23" % Test,
      "org.typelevel" %% "cats-core" % "2.5.0",
    )
  )
