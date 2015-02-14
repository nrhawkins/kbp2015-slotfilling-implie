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
    
    val sentence = "Most want nothing to do with religious feuding and blame politicians for the ungodly messes going on in Iraq, Palestine, Afghanistan and elsewhere."
    //val sentence = "HANOI -- Vietnam's stock market index, VN-Index, closed at 1, 059.79 points on Thursday, up 18.8 points, or 1.81 percent against Wednesday."
    //Wilson, Phil Mickelson, two-time Singapore winner Adam Scott, Ernie Els, Darren Clarke, Ian Poulter, K.J. Choi-43
    //Wilson, Phil Mickelson, two-time Singapore winner Adam Scott, Ernie Els, Darren Clarke, Ian Poulter, K.J. Choi and Thai star Thongchai Jaidee-48
    //val sentence = "Notes: Ireland's Padraig Harrington, the British Open and U.S. PGA Championship winner, tops the field along with Wilson, Phil Mickelson, two-time Singapore winner Adam Scott, Ernie Els, Darren Clarke, Ian Poulter, K.J. Choi and Thai star Thongchai Jaidee."
    //val sentence = "Mercosur was founded in 1991 by Brazil, Argentina, Uruguay and Paraguay."
    //head index = 6, tag index = 1, both for CAIRO
    //val sentence = "CAIRO, Egypt 2007-09-07 01:21:47 UTC"
    //val sentence = "CAIRO, Nov. 13 (Xinhua)"  
    //val sentence =   
    //val sentence = "Kentucky Fried Chicken announced a new CEO, Jake Sanders."
    //val sentence = "Chad claims rebel infiltration from Sudan."
    //val sentence = "The old signatories of the 1985 Schengen Agreement included Austria, Belgium, Denmark, Finland, France, Germany, Greece, Iceland, Italy, Luxembourg, the Netherlands, Norway, Portugal, Spain and Sweden."
    val result = extractor.extractRelations(sentence)    

    println("Result Size: " + result.size)
    result.foreach(r => {
      println(r.np)
      println(r.relation)
      println("tag: " + r.tag)
      println("tag index: " + r.tag.index)
      println("head: " + r.head)
      println("head index: " + r.head.index)      
      println("ner size: " + r.ners.size)
      println("rt size: " + r.relationTrace.size)
      println("ert size: " + r.explicitRelationTraces.size)
      //println(r.relationTrace.foreach(rt => println(rt.toString)))
      println(r.explicitRelationTraces.foreach(rt => { 
        println("RT")
        rt.foreach(rt => {
          val x = rt.toString
          println(rt.toString) 
          println(x.contains("conj_and"))        
        })
      }))
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
