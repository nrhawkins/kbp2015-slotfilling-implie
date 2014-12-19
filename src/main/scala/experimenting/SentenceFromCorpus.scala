package experimenting

import java.io.{StringReader, PrintStream}

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.{WordToSentenceProcessor, LexedTokenFactory, CoreLabelTokenFactory, PTBTokenizer}
import tac.HtmlUtils

import scala.io.Source

import scala.collection.JavaConversions._

/**
 * Created by Gene on 12/18/2014.
 */
object SentenceFromCorpus {

  // TODO: put stuff in config
  val indir = "random-files/"
//  val infile = s"${indir}AFP_ENG_20080920.0463.LDC2009T13.sgm"
  val infile= s"${indir}NYT_ENG_20070305.0050.LDC2009T13.sgm"

  val outdir = "random-files-out/data/10wb/"
  val outfile = s"${outdir}http-split.text"

  val ltf: LexedTokenFactory[CoreLabel] = new CoreLabelTokenFactory(true)
  val options: String = "invertible=true,ptb3Escaping=true"

  def main(args: Array[String]) {
    val sen: WordToSentenceProcessor[CoreLabel] = new WordToSentenceProcessor[CoreLabel]()


    val filestr = Source.fromFile(infile).mkString

    // removes html with each tag, usually just <p>, in its own line.
    val textonly = HtmlUtils.removeHtml(filestr)
    println(filestr)
    println()
    println(textonly)
    println()

    // NOTE: Mostly copied from KBPOpenIE2013, with adjustments for Scala.
    // TODO: put this into its own class and method (DocumentSentenceExtractor)
    val paragraphs = textonly.split("\n").filter(s => s.trim != "")
    for (paragraph <- paragraphs) {
      // Clean up for htmls, urls, emails, etc (Xiao), [edited by Gene]
      val par = paragraph.replaceAll("https?://\\S+?(\\s|$)", "U_R_L$1")
        .replaceAll("[A-Za-z0-9\\.\\-]+?@([A-Za-z0-9\\-]+?\\.){1,}+(com|net)", "E_M_A_I_L")
        .replaceAll("<[A-Za-z0-9\\.\\-]+? [\\.]{3} @([A-Za-z0-9\\-]+?\\.){1,}+(com|net)>", "E_M_A_I_L")
        .replaceAll("[\\-_=]{3,}+", "---")
        .replaceAll(whitespace_charclass, " ")
        .replaceAll("</\\p{Alnum}", "")
        .replace("[ \t\\u000B\f\r]+", " ")
      // END clean up

      val tok: PTBTokenizer[CoreLabel] = new PTBTokenizer[CoreLabel](new StringReader(par), ltf, options)
      val l: java.util.List[CoreLabel] = tok.tokenize()

      val snts = sen.process(l).toList.map(snt => par.substring(
          snt(0).beginPosition(), snt.get(snt.size() - 1).endPosition()))

      snts.foreach(println)
/*
      for (snt <- sen.process(l).toList) {
        val sntBegin = snt(0).beginPosition()

        // This is left from when I copied.  Maybe useful for later.
        // Even if kept, it should probably be in a separate method though.
        val sb2: StringBuilder = new StringBuilder()
        val sb3: StringBuilder = new StringBuilder()

        for (cl <- snt) {
          if (sb2.length > 0)
            sb2.append(" ")
          // To avoid stanford corenlp group words
          // into a huge word between angle brackets,
          // which will confuse the cj parser (Xiao)
          if (cl.word().startsWith("<") && cl.word().endsWith(">")) {
            sb2.append("LONG_WORD")
          } else {
            sb2.append(cl.word())
          }

          if (sb3.length > 0)
            sb3.append(" ")
          val from = cl.beginPosition() - sntBegin
          val to = cl.endPosition() - sntBegin
          sb3.append(from + ":" + to)
        }

        val rawSnt = par.substring(sntBegin,
          snt.get(snt.size() - 1).endPosition())
      }
*/
    }

//    val out = new PrintStream(outfile)
//    out.println(textonly)
//    out.close()
  }

  val whitespace_chars: String = "" +
    "\\u000A" + // LINE FEED (LF)
    "\\u000B" + // LINE TABULATION
    "\\u000C" + // FORM FEED (FF)
    "\\u000D" + // CARRIAGE RETURN (CR)
    "\\u0020" + // SPACE
    "\\u0085" + // NEXT LINE (NEL)
    "\\u00A0" + // NO-BREAK SPACE
    "\\u1680" + // OGHAM SPACE MARK
    "\\u180E" + // MONGOLIAN VOWEL SEPARATOR
    "\\u2000" + // EN QUAD
    "\\u2001" + // EM QUAD
    "\\u2002" + // EN SPACE
    "\\u2003" + // EM SPACE
    "\\u2004" + // THREE-PER-EM SPACE
    "\\u2005" + // FOUR-PER-EM SPACE
    "\\u2006" + // SIX-PER-EM SPACE
    "\\u2007" + // FIGURE SPACE
    "\\u2008" + // PUNCTUATION SPACE
    "\\u2009" + // THIN SPACE
    "\\u200A" + // HAIR SPACE
    "\\u2028" + // LINE SEPARATOR
    "\\u2029" + // PARAGRAPH SEPARATOR
    "\\u202F" + // NARROW NO-BREAK SPACE
    "\\u205F" + // MEDIUM MATHEMATICAL SPACE
    "\\u3000"   // IDEOGRAPHIC SPACE
  val whitespace_charclass: String = "[" + whitespace_chars + "]"
}
