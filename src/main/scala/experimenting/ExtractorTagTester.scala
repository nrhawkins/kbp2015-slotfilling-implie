package experimenting

import extractor.NounToNounRelationExtractor
import testers.TaggerTester

/**
 * Created by Gene on 11/25/2014.
 */
object ExtractorTagTester {
  def main(args: Array[String]) {
    val tagger = TaggerTester.tagger
    val extractor = new NounToNounRelationExtractor(tagger)
    val tags = extractor.getTags("John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud.")
    for (typ <- tags) {
      println(s"Tag: ${typ.name}\tText: ${typ.text}")
    }

    println()

    println("TagMap")
    val tagMap = extractor.createTagMap(tags)
    for ((k, v) <- tagMap) {
      println(s"key: $k\ttag: ${v.tag}\ttext: ${v.text}")
    }
  }
}
