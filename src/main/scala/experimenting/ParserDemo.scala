package experimenting


import java.io.StringReader
import java.util

import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer, Tokenizer, TokenizerFactory}

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
    //val sentences = "testSet/tagger/sentences.txt"
    //val tokenFile = "testSet/tagger/tokenized.txt"
    //val lines = Source.fromFile(sentences).getLines()
    //val out = new PrintWriter(tokenFile)
    val sent2: String = "Oscar Emmanuel Peterson was born in the poor St. Antoine district of Montreal on Aug. 15, 1925, one of five children of Daniel Peterson, a West Indian immigrant, and the former Olivia John, whom Daniel had met in Montreal."
    val tokenizerFactory: TokenizerFactory[CoreLabel] = PTBTokenizer.factory(new CoreLabelTokenFactory, "")
    val tok: Tokenizer[CoreLabel] = tokenizerFactory.getTokenizer(new StringReader(sent2))
    val rawWords2: util.List[CoreLabel] = tok.tokenize
    println("Stanford tokenizer")
    for (i <- 0 until rawWords2.size()) {
      val word = rawWords2.get(i)
      print(s"${word.toString}-${i+1} ")
    }
    println()
    println("OpenNLP chunker")
    val chunker = new OpenNlpChunker()
    val tokens = chunker.apply(sent2)
    for (i <- 0 until tokens.size) {
      print(s"${tokens(i).string}-${i+1} ")
    }
    println()

    /*val tokenizerFactory: TokenizerFactory[CoreLabel] = PTBTokenizer.factory(new CoreLabelTokenFactory, "")
    val tok: Tokenizer[CoreLabel] = tokenizerFactory.getTokenizer(new StringReader(sent2))
    val rawWords2: List[CoreLabel] = tok.tokenize
    for (i <- 0 until rawWords2.size()) {
      val word = rawWords2.get(i)
      println(word.toString)
    }*/
    /*
    val chunker = new OpenNlpChunker()
    for (line <- lines) {
      val tokens = chunker.chunk(line)
      for (i <- 0 until tokens.size) {
        out.print(s"${tokens(i).string}-${i+1} ")
      }
      out.println()
    }
    out.close()
    println(Source.fromFile(tokenFile).mkString)*/
  }
}


