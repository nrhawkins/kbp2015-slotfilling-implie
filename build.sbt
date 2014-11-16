name := "ImplicitRelationExtractor"

version := "1.0"

// TODO: Make this not dependent on my system.
// TODO: Figure out how to set up javac so that it doesn't get deleted each time this reruns.
javaHome := Some(file("c:/Program Files/Java/jdk1.8.0_25"))

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "1.4.0",
  "com.typesafe" % "config" % "1.2.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0",
  "edu.stanford.nlp" % "stanford-parser" % "3.5.0",
  "edu.washington.cs.knowitall.taggers" % "taggers-core_2.10" % "0.4",
  "org.scala-lang" % "scala-compiler" % "2.10.2",
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "org.scala-lang" % "scala-library" % "2.10.2"
)
