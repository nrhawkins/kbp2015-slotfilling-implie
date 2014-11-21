package testers

import com.typesafe.config.ConfigFactory
import extractor.{NounToNounRelation, NounToNounRelationExtractor}
import org.joda.time.DateTime

import scala.collection.mutable
import scala.io.Source

/**
 * TODO: complete comment.
 *
 * Uses the tagger specified in ModularTaggerTester.
 */
object ExtractorTester {
  type Tags = Set[(String, Int)]
  // TODO: change the solution struct
  type ExtractorSolution = (mutable.Map[String, mutable.Set[Tags]], String)

  val tagger = TaggerTester.tagger
  val extractor = new NounToNounRelationExtractor(tagger)

  // Configuratons.
  // Set it here so we don't have methods that are passing entire configurations.
  val config = ConfigFactory.load("extractor-tester.conf")
  val datetime = DateTime.now
  val INPUT_FILE = config.getString("input-file")
  val SOLUTION_FILE = config.getString("solution-file")
  val TAGGER_TYPE = config.getString("tagger-type")
  val OUTPUT_FILE = config.getString("output-dir") +
    datetime.toString.replace(":", ";") + config.getString("output-file-tail")

  def main(args: Array[String]) {
    val inputs = trimSplit(Source.fromFile(INPUT_FILE).mkString, "\n")
    val solutions = trimSplit(Source.fromFile(SOLUTION_FILE).mkString, "\n")

    //val testInfo = new TestInfo[String, NounToNounRelation, ExtractorSolution](
    //  extractor.extractRelations, inputs, solutions)
  }

  // TODO: write oomparator

  // Splits a string by a given regular expression and trims the results.
  private def trimSplit(str: String, regex: String): List[String] = str.split(regex).map(s => s.trim()).filter(s => s != "").toList


}
