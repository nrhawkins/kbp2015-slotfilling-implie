name := "ImplicitRelationExtractor"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.4",
  "org.scala-lang" % "scala-reflect" % "2.11.4",
  "org.scala-lang" % "scala-library" % "2.11.4",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0",
  "edu.stanford.nlp" % "stanford-parser" % "3.5.0",
  "edu.washington.cs.knowitall.taggers" % "taggers-core_2.10" % "0.4",
  "com.github.nscala-time" %% "nscala-time" % "1.4.0"
)