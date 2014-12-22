package tac

import java.io.{File, PrintWriter, StringReader}
import java.nio.file.{Paths, Path, Files}

import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.{PTBTokenizer, WordToSentenceProcessor, CoreLabelTokenFactory, LexedTokenFactory}

import scala.io.Source

import scala.collection.JavaConversions._

/**
 * Created by Gene on 12/21/2014.
 */
object TACDevelopmentSentenceExtractor {
  val config = ConfigFactory.load("tac-extractor.conf")
  val sources = config.getString("source-dir")
  val sentences = config.getString("sentence-dir")

  val outfile = s"${sentences}http-split.text"

  def main(args: Array[String]) {
    val filestrs = inputs
    val out = output
    val sentenceExtractor = new DocumentSentenceExtractor()

    var index = 0
    for ((filename, filestr) <- filestrs) {
      for (sentence <- sentenceExtractor.extract(filestr)) {
        out.println(s"$index\t$filename\t$sentence")
        index += 1
      }
    }
    out.close()
  }

  def inputs = {
    new File(sources)
      .listFiles()
      .filter(f => f.isFile)
      .map(file => (file.getName, Source.fromFile(file).mkString))
      .toList
  }

  def output = {
    // Get current sequence num and increment.
    val seqFilename = config.getString("sequence-file")
    val seqNum = Source.fromFile(seqFilename)
      .getLines().next().trim.toInt
    new PrintWriter(seqFilename).append(s"${seqNum + 1}").close()

    val filename = sentences + seqNum + "-sentence-file"

    // Check if the file exists.
    if (Files.exists(Paths.get(filename))) {
      System.out.println(s"Sentence file already exists!  " +
        s"Please set the number in $seqFilename to a non conflicting " +
        s"sequence number.\nExiting...")
      sys.exit(1)
    }

    // If not, create new file with the given sequence num.
    new PrintWriter(filename)
  }
}
