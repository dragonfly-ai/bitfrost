
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val bitfrost = crossProject(JSPlatform, JVMPlatform).settings(
  publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
  name := "bitfrost",
  version := "0.0.01",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
  libraryDependencies += "ai.dragonfly.code" %%% "matrix" % "0.331.524",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  Compile / mainClass := Some("ai.dragonfly.bitfrost.visualization.TestVolumeMesh"),
  scalaJSUseMainModuleInitializer := true
).jsSettings().jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin)).jvmSettings()

lazy val demo = project.enablePlugins(ScalaJSPlugin,ScalaJSBundlerPlugin).dependsOn(bitfrost.projects(JSPlatform)).settings(
  name := "demo",
  Compile / mainClass := Some("Demo"),
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.11.1",
  scalaJSUseMainModuleInitializer := true
)
