package tac

import extractor.{ImplicitRelation, TaggerLoader, NERFilteredIRE}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import tac.RandomSentenceSetExtractor.SentenceEntry

import scala.io.Source

/**
 * Created by Gene on 1/9/2015.
 */
object RerunSentenceExtraction {
  def main(args: Array[String]) {
    assert(args.size > 0)
    val infile = args(0)
    val outfile = infile + "-rerun-on-" +
      DateTimeFormat.forPattern("MM-dd-yyyy").print(DateTime.now)

    val entries = Source.fromFile(infile).getLines().toList.tail
                  .filter(l => l.trim != "")
                  .map(l => {
      val tokens = l.split("\t").filter(t => t.trim != "")
      SentenceEntry(tokens(1).toInt, tokens(2), tokens(6))
    })

    val split = RandomSentenceSetExtractor.splitList(Nil, entries, 20)

    println("Sentences Selected.")

    println("Loading Extractor.")
    val extractor = new NERFilteredIRE(TaggerLoader.basicTestTagger)

    println("Extracting Sentences.")
    var extractions: List[(List[ImplicitRelation], SentenceEntry)] = Nil
    var percent = 0
    for (sublist <- split) {
      extractions = sublist.map(se => (extractor.extractRelations(se.sentence), se)):::extractions
      percent += 5
      println(s"$percent%")
    }

    println("Printing Results.")
    RandomSentenceSetExtractor.outputResults(outfile, extractions)
  }
}
