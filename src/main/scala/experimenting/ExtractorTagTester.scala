package experimenting

import extractor._
import testers.TaggerTester

/**
 * Created by Gene on 11/25/2014.
 */
object ExtractorTagTester {
  def main(args: Array[String]) {

    val tagger = TaggerLoader.defaultTagger
//    val extractor = new ImplicitRelationExtractor(tagger)
    val filtered = new ImplIEWithBasicFilters(tagger)
/*
    val wordnetextractor = new WordNetFilteredIRE(tagger)
    val constrained = new FormalConstrainedImplIE(tagger)
*/

      val sentences = List(
          "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud.",
          "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley.",
          "AMSTERDAM, Netherlands 2008-01-26 21:41:45 UTC",
          "In a recent speech to the Jewish Coalition, he went further, accusing the Democrats of putting too much stock in diplomacy.",
          "Taipei, Sept. 11 (CNA) 09/11/07 20:10:07 (By T.C. Jiang)",
          "SEOUL, South Korea 2007-02-16 03:13:47 UTC",
          "Kyrgyzstan vs. Jordan",
          "The client division sales \"are surprisingly ahead of where we thought they would come in,\" said Sid Parakh, an analyst at McAdams Wright Ragen.",
          " According to the CWB forecast, Typhoon Hagupit is expected to move swiftly across the Bashi Channel and reach the waters southeast of Taiwan Monday, and that Taiwan's southeast region and the Kaohsiung-Pingtung area in southern Taiwan will begin to see rain from Monday afternoon, with the storm intensifying until Tuesday."
      )

      for (s <- sentences) {
          println(s)
          val results = filtered.extractRelations(s)
          println(results)
          println()
      }
//    val sentence = "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
//    val sentence = "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley."

//    val sentence = "AMSTERDAM, Netherlands 2008-01-26 21:41:45 UTC"
//    val sentence = "In a recent speech to the Jewish Coalition, he went further, accusing the Democrats of putting too much stock in diplomacy."
//    val sentence = "Taipei, Sept. 11 (CNA) 09/11/07 20:10:07 (By T.C. Jiang); SEOUL, South Korea 2007-02-16 03:13:47 UTC"
//    val sentence = "Kyrgyzstan vs. Jordan"
//    val tokens = extractor.getTokens(sentence)

//    println(tokens)
//    println(tokens(0).chunkSymbol.name)
//    println(tokens(0).postagSymbol.name)
/*
    val constrainedresult = constrained.extractRelations(sentence)
    val wordnetresult = wordnetextractor.extractRelations(sentence)
    val result = extractor.extractRelations(sentence)

    println(constrainedresult)
    println(wordnetresult)
    println(result)

    val sentence2 = "In a recent speech to the Jewish Coalition, he went further, accusing the Democrats of putting too much stock in diplomacy."
    val constrainedresult2 = constrained.extractRelations(sentence2)
    val wordnetresult2 = wordnetextractor.extractRelations(sentence2)
    val result2 = extractor.extractRelations(sentence2)

    println(constrainedresult2)
    println(wordnetresult2)
    println(result2)

    val sentence3 = "The client division sales \"are surprisingly ahead of where we thought they would come in,\" said Sid Parakh, an analyst at McAdams Wright Ragen."
    val constrainedresult3 = constrained.extractRelations(sentence3)
    val wordnetresult3 = wordnetextractor.extractRelations(sentence3)
    val result3 = extractor.extractRelations(sentence3)

    println(constrainedresult3)
    println(wordnetresult3)
    println(result3)
*/
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
