lazy val neuroniscVer = "0.0.1-M1"
lazy val scalaVer     = "3.3.0-RC3"

ThisBuild / version       := neuroniscVer
ThisBuild / scalaVersion  := scalaVer
ThisBuild / organization  := "se.bjornregnell"

console / initialCommands := """import neuronisc.*"""
Global / onChangedBuildSource := ReloadOnSourceChanges

fork := true
outputStrategy := Some(StdoutOutput)
run / javaOptions += "-Xmx8G"
run / connectInput := true

lazy val `reqT` = (project in file("."))
  .settings(
    name := "reqT",
    scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked"),
  )