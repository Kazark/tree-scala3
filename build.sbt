lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    testFrameworks += new TestFramework("munit.Framework"),
    scalaVersion := "3.0.0",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.26" % Test,
      "org.typelevel" %% "cats-core" % "2.6.1",
    )
  )
