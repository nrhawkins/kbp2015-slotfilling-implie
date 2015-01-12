package scripts

import java.io.PrintWriter

import com.typesafe.config.ConfigFactory
import extractor.{TaggerLoader, NERFilteredIRE}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import utils.ExtractionFormatUtils

import scala.io.Source

/**
 * Created by Gene on 1/8/2015.
 */
object VerboseNERFilteredExtractor {
  def main(args: Array[String]) {
    val config = ConfigFactory.load("verbose-ner-extractor.conf")
    val inputFile = config.getString("input-file")
    val outname = config.getString("output-dir") + outputFilename

    println("Loading Sentences.")
    val sentences = Source.fromFile(inputFile).getLines()
                    .filter(l => l.trim != "").map(l => l.trim).toList

    println("Loading Extractor.")
    val extractor = new NERFilteredIRE(TaggerLoader.basicTestTagger)

    println("Extracting...")
    val out = new PrintWriter(outname)
    val results = sentences.map(s => (s, extractor.extractRelations(s)))
    for ((sentence, extractions) <- results) {
      out.println(sentence)
      println(sentence)

      for (extraction <- extractions) {
        out.println(extraction)
        out.println(extraction.relationTrace)
        println(extraction)
      }
      out.println(ExtractionFormatUtils.extractionInfo(extractor)(sentence))
      out.println(ExtractionFormatUtils.verboseOutput(extractor)(sentence))
      out.println()

      println(ExtractionFormatUtils.extractionInfo(extractor)(sentence))
      println(ExtractionFormatUtils.verboseOutput(extractor)(sentence))
      println()
    }
    out.close()
    println("Done.")
  }

  def outputFilename: String = {
    val dateStr = DateTimeFormat.forPattern("MM-dd-yyyy_HHmm").print(DateTime.now)
    s"$dateStr"
  }
}
