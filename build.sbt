name := "pb2"

version := "0.1"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:higherKinds",
  "-language:implicitConversions",
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0-M4",
  "org.typelevel" %% "cats-mtl-core" % "0.6.0",
)
