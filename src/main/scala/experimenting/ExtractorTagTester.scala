package experimenting

import extractor.{TaggerLoader, ImplicitRelationExtractor}
import testers.TaggerTester

/**
 * Created by Gene on 11/25/2014.
 */
object ExtractorTagTester {
  def main(args: Array[String]) {

    val tagger = TaggerLoader.basicTestTagger
    val extractor = new ImplicitRelationExtractor(tagger)
//    val sentence = "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
    val sentence = "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley."
    val result = extractor.extractRelations(sentence)

    println(result)
/*
    for (typ <- tags) {
      println(s"Tag: ${typ.name}\tText: ${typ.text}")
    }

    println()

    println("TagMap")
    val tagMap = extractor.createTagMap(tags)
    for ((k, v) <- tagMap) {
      println(s"key: $k\ttag: ${v.tag}\ttext: ${v.text}")
    }
*/
  }
}
