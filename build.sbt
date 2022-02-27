name         := "faxion"
version      := "0.1"
scalaVersion := "2.13.8"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

// TODO: Add OpenCV + http/graphql backend server library.
// TODO: Http client?
// Dependency Versions.
val circeVersion = "0.14.1"

lazy val app = (project in file(".")).settings(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core"   % "2.7.0",
    "org.typelevel" %% "cats-effect" % "3.3.5"
  ) ++ Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_            % circeVersion)
)
