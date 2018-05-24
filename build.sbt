name := "ca-project"

version := "0.0.1"

scalaVersion := "2.11.8"

// When execute "sbt assembly" command,
// it resolves many errors...
assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
