val scala213Version = "2.13.10"

ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := Seq(scala213Version, "3.3.0")
ThisBuild / organization := "io.github.casehubdk"
ThisBuild / organizationName := "CaseHubDK"

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / tlCiMimaBinaryIssueCheck := false
ThisBuild / tlMimaPreviousVersions := Set.empty
ThisBuild / mimaReportSignatureProblems := false
ThisBuild / mimaFailOnProblem := false
ThisBuild / mimaPreviousArtifacts := Set.empty

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("valdemargr", "Valdemar Grange", "randomvald0069@gmail.com", url("https://github.com/valdemargr"))
)

lazy val sharedSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.scalameta" %% "munit" % "1.0.0-M6" % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test
  )
)

lazy val proto = project
  .in(file("proto"))
  .enablePlugins(Fs2Grpc)
  .settings(
    name := "spice4s-client-proto-api",
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb",
      scalapb.validate.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-validate-core" % scalapb.validate.compiler.BuildInfo.version % "protobuf",
      "com.thesamet.scalapb.common-protos" %% "proto-google-common-protos-scalapb_0.11" % "2.9.6-0" % "protobuf",
      "com.thesamet.scalapb.common-protos" %% "proto-google-common-protos-scalapb_0.11" % "2.9.6-0"
    )
  )

lazy val client = project
  .in(file("client"))
  .settings(sharedSettings)
  .settings(
    name := "spice4s-client",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.1",
      "co.fs2" %% "fs2-core" % "3.7.0"
    )
  )
  .dependsOn(proto)

// lazy val core = project
//   .in(file("modules/core"))
//   .settings(sharedSettings)
//   .settings(name := "hxl")
// 
// lazy val natchez = project
//   .in(file("modules/natchez"))
//   .dependsOn(core)
//   .settings(sharedSettings)
//   .settings(
//     libraryDependencies ++= Seq(
//       "org.tpolecat" %% "natchez-core" % "0.3.2",
//       "org.tpolecat" %% "natchez-noop" % "0.3.2" % Test,
//       "org.tpolecat" %% "natchez-testkit" % "0.3.2" % Test
//     )
//   )
//   .settings(name := "hxl-natchez")
