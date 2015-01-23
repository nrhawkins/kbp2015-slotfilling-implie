package experimenting

import extractor.{WordNetFilteredIRE, ImplicitRelationExtractor, TaggerLoader}
import testers.TaggerTester

/**
 * Created by Gene on 11/25/2014.
 */
object ExtractorTagTester {
  def main(args: Array[String]) {

    val tagger = TaggerLoader.defaultTagger
    val extractor = new ImplicitRelationExtractor(tagger)
    val wordnetextractor = new WordNetFilteredIRE(tagger)
//    val sentence = "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
//    val sentence = "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley."

    val sentence = "AMSTERDAM, Netherlands 2008-01-26 21:41:45 UTC"
    val wordnetresult = wordnetextractor.extractRelations(sentence)
    val result = extractor.extractRelations(sentence)

    println(wordnetresult)
    println(result)

    val sentence2 = "The explosion happened just after midnight (2200 GMT " +
      "Monday) on a crowded seafront close to the Aloha Puerto hotel, a " +
      "police spokesman told AFP."
    val wordnetresult2 = wordnetextractor.extractRelations(sentence2)
    val result2 = extractor.extractRelations(sentence2)

    println(wordnetresult2)
    println(result2)

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
