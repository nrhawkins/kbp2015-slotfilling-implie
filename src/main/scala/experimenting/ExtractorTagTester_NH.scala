package experimenting

import extractor.{FormalConstrainedImplIE, WordNetFilteredIRE, ImplicitRelationExtractor, TaggerLoader}
import testers.TaggerTester

/**
 * Created by Gene on 11/25/2014.
 */
object ExtractorTagTester_NH {
  def main(args: Array[String]) {

    val tagger = TaggerLoader.defaultTagger
    val extractor = new ImplicitRelationExtractor(tagger)
    val wordnetextractor = new WordNetFilteredIRE(tagger)
    val constrained = new FormalConstrainedImplIE(tagger)
    
    val sentence = "The old signatories of the 1985 Schengen Agreement included Austria, Belgium, Denmark, Finland, France, Germany, Greece, Iceland, Italy, Luxembourg, the Netherlands, Norway, Portugal, Spain and Sweden."
    val result = extractor.extractRelations(sentence)    

    println("Result Size: " + result.size)
    result.foreach(r => {
      println(r.np)
      println(r.relation)
      println(r.tag)
      println(r.head)
      println(r.ners.size)
      println(r.relationTrace.size)
      println(r.relationTrace.foreach(rt => println(rt.toString)))
    })
    
    //val sentence = "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
    //val sentence = "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley."

    //val sentence = "AMSTERDAM, Netherlands 2008-01-26 21:41:45 UTC"
    //val constrainedresult = constrained.extractRelations(sentence)
    //val wordnetresult = wordnetextractor.extractRelations(sentence)
    //val result = extractor.extractRelations(sentence)

    //println(constrainedresult)
    //println(wordnetresult)
    //println(result)

    //val sentence2 = "In a recent speech to the Jewish Coalition, he went further, accusing the Democrats of putting too much stock in diplomacy."
    //val constrainedresult2 = constrained.extractRelations(sentence2)
    //val wordnetresult2 = wordnetextractor.extractRelations(sentence2)
    //val result2 = extractor.extractRelations(sentence2)

    //println(constrainedresult2)
    //println(wordnetresult2)
    //println(result2)

    //val sentence3 = "The client division sales \"are surprisingly ahead of where we thought they would come in,\" said Sid Parakh, an analyst at McAdams Wright Ragen."
    //val constrainedresult3 = constrained.extractRelations(sentence3)
    //val wordnetresult3 = wordnetextractor.extractRelations(sentence3)
    //val result3 = extractor.extractRelations(sentence3)

    //println(constrainedresult3)
    //println(wordnetresult3)
    //println(result3)
    
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
