name := "parse-go"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-io" % "1.3.2",
  "com.lihaoyi" %% "fastparse" % "0.3.4",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
