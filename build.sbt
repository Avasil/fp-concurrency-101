name := "fp-concurrency-101"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "io.monix" %% "monix" % "3.0.0-RC2"
)

fork in run := true