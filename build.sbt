lazy val root = project
  .in(file("."))
  .settings(
    name := "arrow_forever_bug",
    version := "0.1.0",
    resolvers += Resolver.mavenLocal,

    scalaVersion := dottyLatestNightlyBuild.get,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.24" % (scalaVersion.value)
    )
  )
