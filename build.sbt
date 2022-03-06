name         := "faxion"
version      := "0.1"
scalaVersion := "2.13.8"

// Dependency Versions.
val circeVersion     = "0.14.1"
val scrimmageVersion = "4.0.13"
val scalaTestVersion = "3.2.11"
val fs2Version       = "3.2.5"
val http4sVersion    = "0.23.10"

lazy val app = (project in file(".")).settings(
  libraryDependencies ++= Seq(
    "org.typelevel"         %% "cats-core"                     % "2.7.0",
    "org.typelevel"         %% "cats-effect"                   % "3.3.5",
    "org.typelevel"         %% "log4cats-slf4j"                % "2.2.0",
    "org.scalactic"         %% "scalactic"                     % scalaTestVersion,
    "org.scalatest"         %% "scalatest"                     % scalaTestVersion % Test,
    "org.typelevel"         %% "cats-effect-testing-scalatest" % "1.4.0"          % Test,
    // Json.
    "io.circe"              %% "circe-core"                    % circeVersion,
    "io.circe"              %% "circe-generic"                 % circeVersion,
    "io.circe"              %% "circe-parser"                  % circeVersion,
    // Image processing.
    "com.sksamuel.scrimage"  % "scrimage-core"                 % scrimmageVersion,
    "com.sksamuel.scrimage"  % "scrimage-filters"              % scrimmageVersion,
    "com.sksamuel.scrimage"  % "scrimage-formats-extra"        % scrimmageVersion,
    "com.sksamuel.scrimage"  % "scrimage-webp"                 % scrimmageVersion,
    "com.sksamuel.scrimage" %% "scrimage-scala"                % scrimmageVersion,
    // Streaming.
    "co.fs2"                %% "fs2-core"                      % fs2Version,
    "co.fs2"                %% "fs2-io"                        % fs2Version,
    // Http Server.
    "org.http4s"            %% "http4s-blaze-server"           % http4sVersion,
    "org.http4s"            %% "http4s-blaze-client"           % http4sVersion,
    "org.http4s"            %% "http4s-blaze-core"             % http4sVersion,
    "org.http4s"            %% "http4s-circe"                  % http4sVersion,
    "org.http4s"            %% "http4s-dsl"                    % http4sVersion
    // Image processing.
    // "ch.unibas.cs.gravis"   %% "scalismo-faces"                % "0.90.0"
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)
