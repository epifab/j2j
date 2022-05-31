ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "j2j",
    libraryDependencies ++= Seq(
      "org.typelevel"         %% "cats-core"    % "2.7.0",
      "io.circe"              %% "circe-core"   % "0.14.1",
      "io.circe"              %% "circe-parser" % "0.14.1",
      "com.github.pureconfig" %% "pureconfig"   % "0.17.1",
      "org.scalatest"         %% "scalatest"    % "3.2.12" % Test,
      "org.scalacheck"        %% "scalacheck"   % "1.16.0" % Test,
    ),
  )
