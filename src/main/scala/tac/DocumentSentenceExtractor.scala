package tac

import java.io.StringReader

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.{WordToSentenceProcessor, CoreLabelTokenFactory, LexedTokenFactory, PTBTokenizer}

import scala.collection.JavaConversions._

/**
 * Created by Gene on 12/21/2014.
 */
class DocumentSentenceExtractor(options: String = "invertible=true,ptb3Escaping=true") {
  val ltf = new CoreLabelTokenFactory(true)
  val sentenceProcessor = new WordToSentenceProcessor[CoreLabel]()

  def extract(document: String): List[String] = {
    // removes html with each tag, usually just <p>, in its own line.
    val textonly = HtmlUtils.removeHtml(document)

    // NOTE: Mostly copied from KBPOpenIE2013, with adjustments for Scala.
    // TODO: put this into its own class and method (DocumentSentenceExtractor)
    val paragraphs = textonly.split("\n").filter(s => s.trim != "")
    paragraphs.foldLeft(Nil: List[String])((acc, paragraph) => {
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

      val snts = sentenceProcessor.process(l).toList.map(snt => par.substring(
        snt(0).beginPosition(), snt.get(snt.size() - 1).endPosition()))
      snts ::: acc
    })
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
