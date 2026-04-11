name := "scalapandoc"

version := "0.1.0-SNAPSHOT"

scalaVersion := "3.8.3"

libraryDependencies ++= Seq(
  // CommonMark parsing - use all-in-one for simplicity
  "com.vladsch.flexmark" % "flexmark-all" % "0.64.8",

  // JSON (pandoc AST compatibility)
  "io.circe" %% "circe-core" % "0.14.10",
  "io.circe" %% "circe-generic" % "0.14.10",
  "io.circe" %% "circe-parser" % "0.14.10",

  // CLI
  "com.github.scopt" %% "scopt" % "4.1.0",

  // Testing
  "org.scalameta" %% "munit" % "1.0.2" % Test,
)

// Java options for better performance
javaOptions ++= Seq(
  "-Xmx4G",
  "-Xss4M",
)
