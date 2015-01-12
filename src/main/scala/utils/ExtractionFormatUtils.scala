package utils

import edu.stanford.nlp.trees.TypedDependency
import extractor.{ImplicitRelationExtractor, TagInfo}

import scala.collection.mutable

/**
 * Created by Gene on 12/24/2014.
 */
object ExtractionFormatUtils {
  def verboseOutput(extractor: ImplicitRelationExtractor)(src: String) = {
    // Parse tree + dependency list
    val parse = extractor.getParse(src)
    val builder = new mutable.StringBuilder
    builder.append(parse._1.pennString())
    for (td <- parse._2) {
      builder.append(td + "\n")
    }
    builder.mkString
  }

  def extractionInfo(extractor: ImplicitRelationExtractor)(src: String): String = {
    def printHops(map: Map[TagInfo, List[List[TypedDependency]]], builder: StringBuilder) {
      for ((k, v) <- map) {
        builder.append(s"Tag: ${k.text} has tag ${k.tag}\tDependency Hops:\t")
        for (td <- v) {
          builder.append(s"$td\t")
        }
        builder.append("\n")
      }
    }
    val tags = extractor.getTags(src)
    val (parse, tdl) = extractor.getParse(src)
    val singleHops = extractor.dependencyHopsByTag(tags, tdl)
    val singleGeneralized = singleHops map { case (k, v) => (k, v.map(td => td::Nil))}
    val doubleHops = extractor.expandAllByOneHop(singleGeneralized, tdl)

    val builder = mutable.StringBuilder.newBuilder
    builder.append("Extraction Info\n")

    for ((k, v) <- singleHops) {
      builder.append(s"Tag: ${k.text} has tag ${k.tag}\tDependency Hops:\t")
      builder.append("Single Hops ")
      for (td <- v) {
        builder.append(s"$td ")
      }
      builder.append("\t")
      builder.append("Double Hops ")
      for (td <- doubleHops.getOrElse(k, Nil)) {
        builder.append(s"$td ")
      }
      builder.append("\n")
    }

    builder.mkString
  }
}
