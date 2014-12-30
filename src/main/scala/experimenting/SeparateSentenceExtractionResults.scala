package experimenting

import java.io.PrintWriter

import scala.io.Source

/**
 * Created by Gene on 12/30/2014.
 */
object SeparateSentenceExtractionResults {
  def main(args: Array[String]): Unit = {
    val filename = "tac_development/random-1000/results/0-result-file"
    val nullout = new PrintWriter("tac_development/random-1000/results/0-null")
    val normout = new PrintWriter("tac_development/random-1000/results/0-extractions")

    val lines = Source.fromFile(filename).getLines().toList
    nullout.println(lines.head)
    normout.println(lines.head)

    for (line <- lines.tail) {
      val tokens = line.trim.split("\t")
      val out = if (tokens.contains("NULL")) nullout else normout
      out.println(line)
    }
  }
}
