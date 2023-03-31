ThisBuild / scalaVersion := "3.2.1"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven" ) ) )
ThisBuild / resolvers += "ai.dragonfly.code" at "https://code.dragonfly.ai/"
ThisBuild / organization := "ai.dragonfly.code"
ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation")

lazy val bitfrost = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "bitfrost",
    version := "0.0.03",
    Compile / mainClass := Some("ai.dragonfly.bitfrost.verification.ConversionFidelity"),
    libraryDependencies ++= Seq(
      "ai.dragonfly.code" %%% "mesh" % "0.03.41.5401",
      "ai.dragonfly.code" %%% "spatial" % "0.4.5401",
      "ai.dragonfly.code" %%% "cliviz" % "0.02.5401"
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings()

lazy val demo = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .dependsOn(bitfrost)
  .settings(
    name := "demo",
    Compile / mainClass := Some("Demo"),
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.12.0",
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings()
