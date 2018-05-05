val deps = Seq("com.storm-enroute" % "scalameter_2.12" % "0.8.2")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.ucu.pfp",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Parallel MonteCarlo",
    libraryDependencies ++= deps
  )