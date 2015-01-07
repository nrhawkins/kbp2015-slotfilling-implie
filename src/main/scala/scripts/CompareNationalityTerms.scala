package scripts

import java.io.PrintWriter

import scala.io.Source

/**
 * Created by Gene on 1/7/2015.
 */
object CompareNationalityTerms {
  def main(args: Array[String]) {
    val outboth = "results/compare_nationality_terms/lowercase/intersection.txt"
    val outnell = "results/compare_nationality_terms/lowercase/nell.txt"
    val outwiki = "results/compare_nationality_terms/lowercase/wiki.txt"

    val nellDir = "classTermLists/"
    val wikiDir = "classTermLists/wikipedia/"
    val nellFiles = List("origin.txt")
    val wikiFiles = List(
      "country_names.txt",
      "country_adjectivals.txt",
      "country_demonyms.txt")

    val nellSet = getTermSet(Set[String](), nellDir, nellFiles)
    val wikiSet = getTermSet(Set[String](), wikiDir, wikiFiles)

    val intersect = nellSet.intersect(wikiSet)
    val onlyNell = nellSet -- intersect
    val onlyWiki = wikiSet -- intersect

    printSetToFile(intersect, outboth)
    printSetToFile(onlyNell, outnell)
    printSetToFile(onlyWiki, outwiki)
  }

  private def getTermSet(acc: Set[String], dirPrefix: String, files: List[String]): Set[String] = {
    files match {
      case Nil => acc
      case head::tail =>
        getTermSet(acc ++ Source.fromFile(dirPrefix + head).getLines().map(l => l.trim.toLowerCase), dirPrefix, tail)
    }
  }

  private def printSetToFile(set: Set[String], file: String) {
    val out = new PrintWriter(file)
    set.foreach(s => out.println(s))
    out.close()
  }
}
