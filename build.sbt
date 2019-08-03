lazy val root = (project in file("."))
  .settings(
    commonSettings,
    compilerPlugins,
    compilerOptions,
    dependencies,
    testSettings,
  )


lazy val commonSettings = Seq(
  name := "movie-tv-notifier",
  scalaVersion := "2.12.8",
  // due to a bug in sbt 1.0.x - should be removed when using 0.13.x
  updateOptions := updateOptions.value.withGigahorse(false),
)

val compilerPlugins = Seq(
  addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
  addCompilerPlugin("com.softwaremill.clippy" %% "plugin" % "0.6.1" classifier "bundle"),
  addCompilerPlugin( "org.typelevel" %% "kind-projector" % "0.10.3"),
  addCompilerPlugin(("org.scalameta" % "paradise" % "3.0.0-M11").cross(CrossVersion.full)),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
)

lazy val compilerOptions =
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-language:higherKinds",
    "-Ypartial-unification",
    "-P:splain:all:true",
    "-P:clippy:colors=true",
    "-language:implicitConversions"
  )

lazy val dependencies = {
  val SttpVersion = "1.6.0"
  val CirceVersion = "0.11.1"

  val cats = Seq(
    "org.typelevel" %% "cats-core" % "1.6.1",
    "org.typelevel" %% "cats-effect" % "1.3.1",
    "org.typelevel" %% "cats-mtl-core" % "0.5.0",
    "com.github.mpilquist" %% "simulacrum" % "0.14.0",
    "org.typelevel" %% "cats-tagless-macros" % "0.8"
  )

  val zio = Seq(
    "dev.zio" %% "zio" % "1.0.0-RC10-1",
    "dev.zio" %% "zio-interop-cats" % "1.3.1.0-RC3",
  )

  val config = Seq(
    "com.github.pureconfig" %% "pureconfig" % "0.9.1",
  )

  val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
    "io.chrisdavenport" %% "log4cats-slf4j"   % "0.3.0"
  )

  val http4sVersion = "0.20.1"

  val http4s = Seq(
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion
  )

  val sttp = Seq(
    "com.softwaremill.sttp" %% "core" % SttpVersion,
    "com.softwaremill.sttp" %% "cats" % SttpVersion,
    "com.softwaremill.sttp" %% "async-http-client-backend-cats" % SttpVersion,
  )

  val circe = Seq(
    "io.circe" %% "circe-generic" % CirceVersion exclude ("aopalliance", "aopalliance"),
    "io.circe" %% "circe-parser" % CirceVersion
  )
  
  val others = Seq(
    "commons-codec" % "commons-codec" % "1.12",
    "com.lihaoyi" %% "scalatags" % "0.7.0",
    "org.simplejavamail" % "simple-java-mail" % "5.2.0" exclude ("com.sun.activation", "jakarta.activation")
  )

  Seq(
    libraryDependencies ++= cats ++ config ++ logging ++ http4s ++ sttp ++ others ++ circe ++ zio
  )
}

lazy val testSettings = {
  val dependencies = {
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.4",
      "org.scalatest" %% "scalatest" % "3.0.5",
      "com.ironcorelabs" %% "cats-scalatest" % "2.3.1",
    ).map(_ % Test)
  }

  Seq(
    logBuffered in Test := false,
    dependencies
  )
}


import sbtassembly.MergeStrategy

test in assembly := {}

assemblyJarName in assembly := name.value + ".jar"

assemblyOutputPath in assembly := file("target/out/" + (assemblyJarName in assembly).value)

publishArtifact in(Compile, packageDoc) := false


assemblyMergeStrategy in assembly := {
  case PathList("javax", "jms", xs@_*) => MergeStrategy.first
  case PathList(ps@_*) if ps.last == "overview.html" => MergeStrategy.first
  case PathList(ps@_*) if ps.last == "RELEASE.txt" => MergeStrategy.first
  case PathList(ps@_*) if ps.last == "LICENSE-2.0.txt" => MergeStrategy.first
  case PathList(ps@_*) if ps.last endsWith ".properties" => MergeStrategy.concat
  case x if x.contains("pureconfig") => MergeStrategy.first
  case x if x.contains("netty") => MergeStrategy.last
  case x if x.contains("aop.xml") => MergeStrategy.last
  case x if x.contains("cinnamon-reference.conf") => MergeStrategy.concat
  case PathList("META-INF", "aop.xml") => aopMergeStrategy
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

val aopMergeStrategy: MergeStrategy = new MergeStrategy {
  val name = "aopMerge"

  import scala.xml._
  import scala.xml.dtd._

  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
    val dt = DocType("aspectj", PublicID("-//AspectJ//DTD//EN", "http://www.eclipse.org/aspectj/dtd/aspectj.dtd"), Nil)
    val file = MergeStrategy.createMergeTarget(tempDir, path)
    val xmls: Seq[Elem] = files.map(XML.loadFile)
    val aspectsChildren: Seq[Node] = xmls.flatMap(_ \\ "aspectj" \ "aspects" \ "_")
    val weaverChildren: Seq[Node] = xmls.flatMap(_ \\ "aspectj" \ "weaver" \ "_")
    val options: String = xmls.map(x => (x \\ "aspectj" \ "weaver" \ "@options").text).mkString(" ").trim
    val weaverAttr = if (options.isEmpty) Null else new UnprefixedAttribute("options", options, Null)
    val aspects = new Elem(null, "aspects", Null, TopScope, false, aspectsChildren: _*)
    val weaver = new Elem(null, "weaver", weaverAttr, TopScope, false, weaverChildren: _*)
    val aspectj = new Elem(null, "aspectj", Null, TopScope, false, aspects, weaver)
    XML.save(file.toString, aspectj, "UTF-8", xmlDecl = false, dt)
    IO.append(file, IO.Newline.getBytes(IO.defaultCharset))
    Right(Seq(file -> path))
  }
}

