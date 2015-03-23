package experimenting

import extractor.{ConstrainedImplIE, WordNetFilteredIRE, ImplicitRelationExtractor, TaggerLoader}

/**
 * Created by Gene on 2/12/2015.
 */
object ExplicitListDev {
  def main(args: Array[String]) {

    val tagger = TaggerLoader.defaultTagger
    val extractor = new ImplicitRelationExtractor(tagger)

    val sentences = List(
      "NEW YORK 2008-11-14 04:43:44 UTC",
      "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud.",
      "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley.",
      "AMSTERDAM, Netherlands 2008-01-26 21:41:45 UTC",
      "In a recent speech to the Jewish Coalition, he went further, accusing the Democrats of putting too much stock in diplomacy.",
      "The client division sales \"are surprisingly ahead of where we thought they would come in,\" said Sid Parakh, an analyst at McAdams Wright Ragen."
    )

    for (s <- sentences) {
      val result = extractor.extractRelations(s)
      println(result)
      for (r <- result) {
        println(s"ners ${r.getNERs}}")
        println(s"explicit traces ${r.explicitRelationTraces}")
      }
    }
  }
}
