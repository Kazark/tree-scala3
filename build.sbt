lazy val V = new {
  val cats = "2.7.0"
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    testFrameworks += new TestFramework("munit.Framework"),
    scalaVersion := "3.1.0",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % V.cats,
      "org.typelevel" %% "discipline-munit" % "1.0.9" % Test,
      "org.typelevel" %% "cats-laws" % V.cats % Test,
    )
  )
