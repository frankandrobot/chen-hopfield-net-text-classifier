name := "chen"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.apache.opennlp" % "opennlp-tools" % "1.6.0"

libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.12",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.12"
)

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.11"