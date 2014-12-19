package tac

import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.util.regex.Pattern

import scala.collection.JavaConversions._

/**
 * to be used in ExtractSEntencesAndTokenize to remove html tags
 * NOTE: copied from reverb, transferred to Scala by Gene Kim
 * @author Xiao Ling
 */
object HtmlUtils {

  private var removePatterns: java.util.HashSet[Pattern] = null
  private var breakPatterns: java.util.HashSet[Pattern] = null
  private var initialized: Boolean = false

  // TODO: split apart header stuff and the rest, use them in separate methods
  private val breakTags: Array[String] = Array("blockquote", "br", "center",
    "dd", "div", "dt", "fieldset", "h\\d", "hr", "img", "input", "isindex",
    "li", "noframes", "noscript", "p", "pre", "q", "table", "td", "textarea",
    "th", "xmp",
    // Added for simplicity, may change to include metadata.
    "headline")

  private val removeTags: Array[String] = Array("applet", "form", "head",
    "iframe", "legend", "map", "object", "script", "select", "style", "title",
    // Added for this specifically, since we want to keep it simple.
    // Just remove the metadata.
    "docid", "doctype", "datetime")

/*
  private val tag: Pattern = Pattern.compile("<[^<]*?>")
  private val whiteSpace: Pattern = Pattern.compile("\\s+")
  private val multiSpace: Pattern = Pattern.compile("  +")
  private val multiBreaks: Pattern = Pattern.compile("(\n|\r)(\n|\r)+")
*/

  def removeHtml(content: String): String = {
    val tag: Pattern = Pattern.compile("<[^<]*?>")
    val whiteSpace: Pattern = Pattern.compile("\\s+")
    val multiSpace: Pattern = Pattern.compile("  +")
    val multiBreaks: Pattern = Pattern.compile("\\s?\n(\n|\\s)+")


    if (!initialized) initPatterns()
    var contentVar = content

    // Normalize whitespace
    contentVar = whiteSpace.matcher(contentVar).replaceAll(" ")

    // Remove and break text
    contentVar = applyPatterns(removePatterns, contentVar)
    contentVar = applyPatterns(breakPatterns, contentVar)

    // Remove tags
    contentVar = tag.matcher(contentVar).replaceAll("")

    // Escape HTML codes
    // content = StringEscapeUtils.unescapeCsv(content);

    // Fix more whitespace
    contentVar = multiSpace.matcher(contentVar).replaceAll(" ")
    contentVar = multiBreaks.matcher(contentVar).replaceAll("\n")
    contentVar = contentVar.replace(';', '\n')

    contentVar
  }

  @throws(classOf[Exception])
  def main(args: Array[String]) {
    var in: BufferedReader = null
    if (args.length == 1) {
      in = new BufferedReader(new FileReader(args(0)))
    }
    else {
      in = new BufferedReader(new InputStreamReader(System.in))
    }
    {
      val sb: StringBuffer = new StringBuffer()
      var line: String = in.readLine
      while (line != null) {
        {
          sb.append(line)
        }
        line = in.readLine
      }
      System.out.println(removeHtml(sb.toString))
    }
  }

  private def applyPatterns(patterns: java.util.HashSet[Pattern], s: String): String = {
    var str = s
    for (pat <- patterns) {
      str = pat.matcher(str).replaceAll("\n")
    }
    str
  }

  private def initPatterns() {
    removePatterns = new java.util.HashSet[Pattern]
    breakPatterns = new java.util.HashSet[Pattern]
    for (rtag <- removeTags) {
      val p1 = Pattern.compile("(?is)<" + rtag + "[^<]*?>.*?</" + rtag + ">")
      removePatterns.add(p1)
      val p2 = Pattern.compile("(?i)</?" + rtag + "[^<]*?>")
      breakPatterns.add(p2)
    }
    for (btag <- breakTags) {
      val p = Pattern.compile("(?i)</?" + btag + "[^<]*?>")
      breakPatterns.add(p)
    }
    initialized = true
  }
}
