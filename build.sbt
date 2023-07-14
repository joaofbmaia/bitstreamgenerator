val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "bitstreamgenerator",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.virtuslab" %% "scala-yaml" % "0.0.7"
    ),

    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation"
    )
  )
