package experimenting

import java.io.PrintWriter

import scala.io.Source

/**
 * Created by Gene on 12/30/2014.
 */
object SentenceExtractionFix {
  def main(args: Array[String]): Unit = {
    val filename = "tac_development/random-1000/results/0-result-file"
    val out = new PrintWriter("tac_development/random-1000/results/tmp")

    val str = StringBuilder.newBuilder
    val lines = Source.fromFile(filename).getLines().toList
    str.append(lines.head + "\n")

    var i = 0
    for (line <- lines.tail) {
      str.append(line.trim.split("\t").tail.foldLeft(s"$i")((acc, cur) =>
        acc + "\t" + cur) + "\n")
      i += 1
    }
    out.print(str.mkString)
    out.close()
  }
}
