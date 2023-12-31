val scala213Version = "2.13.12"

ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := Seq(scala213Version /*, "3.3.0"*/ )
ThisBuild / organization := "io.github.casehubdk"
ThisBuild / organizationName := "CaseHubDK"

ThisBuild / tlBaseVersion := "0.0"
ThisBuild / tlSonatypeUseLegacyHost := false
ThisBuild / tlUntaggedAreSnapshots := false

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
  ),
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2")) {
      Seq(
        "-Xsource:3",
      )
    } else Seq.empty // Seq("-explain")
  },
)

lazy val proto = project
  .in(file("proto"))
  .enablePlugins(Fs2Grpc)
  .settings(
    name := "spice4s-client-proto-api",
    scalapbCodeGenerators := scalapbCodeGenerators.value /* ++ Seq(
      protocbridge.Target(scalapb.validate.gen(), (Compile / sourceManaged).value / "scalapb")
    )*/,
    libraryDependencies ++= Seq(
      /* "com.thesamet.scalapb" %% "scalapb-validate-core" % scalapb.validate.compiler.BuildInfo.version % "protobuf", */
      "com.thesamet.scalapb.common-protos" %% "pgv-proto-scalapb_0.11" % "0.6.13-0",
      "com.thesamet.scalapb.common-protos" %% "pgv-proto-scalapb_0.11" % "0.6.13-0" % "protobuf",
      "com.thesamet.scalapb.common-protos" %% "proto-google-common-protos-scalapb_0.11" % "2.9.6-0" % "protobuf",
      "com.thesamet.scalapb.common-protos" %% "proto-google-common-protos-scalapb_0.11" % "2.9.6-0"
    ),
    tlFatalWarningsInCi := false,
    excludeDependencies ++= Seq(
      ExclusionRule("com.thesamet.scalapb", "scalapb-validate-core_2.13")
    )
  )

lazy val client = project
  .in(file("client"))
  .settings(sharedSettings)
  .dependsOn(proto)
  .settings(
    name := "spice4s-client",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.1",
      "co.fs2" %% "fs2-core" % "3.7.0"
    )
  )

lazy val testkit = project
  .in(file("testkit"))
  .settings(sharedSettings)
  .dependsOn(proto)
  .dependsOn(client)
  .settings(name := "spice4s-testkit")

lazy val generatorCore = project
  .in(file("generator-core"))
  .settings(sharedSettings)
  .dependsOn(client)
  .settings(
    name := "spice4s-generator-core"
  )

lazy val encoder = project
  .in(file("encoder"))
  .settings(sharedSettings)
  .dependsOn(client)
  .settings(
    name := "spice4s-encoder"
  )

lazy val parser = project
  .in(file("parser"))
  .settings(sharedSettings)
  .settings(
    name := "spice4s-parser",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9"
    )
  )

lazy val generator = project
  .in(file("generator"))
  .settings(sharedSettings)
  .dependsOn(parser)
  .dependsOn(client)
  .settings(
    name := "spice4s-generator",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.1",
      "co.fs2" %% "fs2-core" % "3.7.0",
      "co.fs2" %% "fs2-io" % "3.7.0",
      "org.scalameta" %% "scalameta" % "4.8.6",
      "org.typelevel" %% "cats-mtl" % "1.3.0"
    )
  )

lazy val generatorCli = project
  .in(file("generator-cli"))
  .dependsOn(generator)
  .settings(
    name := "spice4s-generator-cli",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline" % "2.4.1",
      "com.monovore" %% "decline-effect" % "2.4.1"
    )
  )
