package experimenting

import java.io.PrintWriter

import extractor.{ExtractionFormatUtils, ImplicitRelationExtractor, TaggerLoader}

import scala.collection.mutable
import scala.io.Source

/**
 * Created by Gene on 12/24/2014.
 */
object VerboseExtractor {
  val sentenceFile = "tac_development/small-subset/evaluation/details/sentences.txt"
  val outputFile = "tac_development/small-subset/evaluation/details/verbose-extractions2.txt"

  def main(args: Array[String]) {
    val tagger = TaggerLoader.defaultTagger
    val extractor = new ImplicitRelationExtractor(tagger)
    val out = new PrintWriter(outputFile)

    for (sentence <- Source.fromFile(sentenceFile).getLines()) {
      val extractions = extractor.extractRelations(sentence)
      out.println(sentence)

      for (extraction <- extractions) {
        out.println(s"extraction:$extraction")
        out.println(s"\ttrace:${extraction.relationTrace}")
      }

      // tag hops
      out.println(ExtractionFormatUtils.extractionInfo(extractor)(sentence))

      // parse tree + dependency relations
      out.println(ExtractionFormatUtils.verboseOutput(extractor)(sentence))
      out.println()
    }
    out.close()
  }
}
