addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.4.19")
addSbtPlugin("org.typelevel" % "sbt-fs2-grpc" % "2.7.4")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")

libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "compilerplugin" % "0.11.11",
  "com.thesamet.scalapb" %% "scalapb-validate-codegen" % "0.3.4"
)

addDependencyTreePlugin
