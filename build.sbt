
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val bitfrost = crossProject(JSPlatform, JVMPlatform).settings(
  publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
  name := "bitfrost",
  version := "0.0.01",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
  libraryDependencies += "ai.dragonfly.code" %%% "matrix" % "0.331.526",
  scalacOptions ++= Seq("-feature", "-deprecation"),
).jsSettings().jvmSettings()

lazy val demo = crossProject(JSPlatform, JVMPlatform).dependsOn(bitfrost).settings(
  name := "demo",
  // Compile / mainClass := Some("Demo"),
  Compile / mainClass := Some("DyeMC"),
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.11.1",
).jsSettings(
  scalaJSUseMainModuleInitializer := true
).jvmSettings()
