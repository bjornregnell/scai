lazy val scaiVer = "0.0.1-M1"
lazy val scalaVer     = "3.3.0-RC3"

ThisBuild / version       := scaiVer
ThisBuild / scalaVersion  := scalaVer
ThisBuild / organization  := "se.bjornregnell"

console / initialCommands := """import scai.*"""
Global / onChangedBuildSource := ReloadOnSourceChanges

fork := true
outputStrategy := Some(StdoutOutput)
run / javaOptions += "-Xmx8G"
run / connectInput := true

lazy val `scai` = (project in file("."))
  .settings(
    name := "scai",
    scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked"),
  )