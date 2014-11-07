
import java.util.Collection
import java.util.List
import java.io.{PrintWriter, StringReader}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.stanford.nlp.process.Tokenizer
import edu.stanford.nlp.process.TokenizerFactory
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.process.DocumentPreprocessor
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.HasWord
import edu.stanford.nlp.ling.Sentence
import edu.stanford.nlp.trees._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser

import scala.io.Source

object ParserDemo {

  /**
   * demoAPI demonstrates other ways of calling the parser with
   * already tokenized text, or in some cases, raw text that needs to
   * be tokenized as a single sentence.  Output is handled with a
   * TreePrint object.  Note that the options used when creating the
   * TreePrint can determine what results to print out.  Once again,
   * one can capture the output by passing a PrintWriter to
   * TreePrint.printTree.
   */
  def main(args: Array[String]) {
    val sentences = "testSet/tagger/sentences.txt"
    val tokenFile = "testSet/tagger/tokenized.txt"
    val lines = Source.fromFile(sentences).getLines()
    val out = new PrintWriter(tokenFile)
    val sent2: String = "Oscar Emmanuel Peterson was born in the poor St. Antoine district of Montreal on Aug. 15, 1925, one of five children of Daniel Peterson, a West Indian immigrant, and the former Olivia John, whom Daniel had met in Montreal."
    /*val tokenizerFactory: TokenizerFactory[CoreLabel] = PTBTokenizer.factory(new CoreLabelTokenFactory, "")
    val tok: Tokenizer[CoreLabel] = tokenizerFactory.getTokenizer(new StringReader(sent2))
    val rawWords2: List[CoreLabel] = tok.tokenize
    for (i <- 0 until rawWords2.size()) {
      val word = rawWords2.get(i)
      println(word.toString)
    }*/
    val chunker = new OpenNlpChunker()
    for (line <- lines) {
      val tokens = chunker.chunk(line)
      for (i <- 0 until tokens.size) {
        out.print(s"${tokens(i).string}-${i+1} ")
      }
      out.println()
    }
    out.close()
    println(Source.fromFile(tokenFile).mkString)
  }
}


