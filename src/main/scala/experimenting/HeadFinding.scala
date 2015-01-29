package experimenting

import edu.stanford.nlp.trees.SemanticHeadFinder
import extractor.{TaggerLoader, ImplicitRelationExtractor}

/**
 * Created by Gene Kim on 1/22/2015.
 */
object HeadFinding {
  def main(args: Array[String]) {
    val phrases = Array(
      "the Hindu festival of Holi",
      "the Hindu festival",
      "one who would be the first Mormon president (Mitt Romney)",
      "President Bush's decision to increase US forces in Baghdad",
      "Persian Emperor Xerxes (Brazil's Rodrigo Santoro, from TV's \"Lost\")",
      "Chinese President Hu",
      "John Arterberry, executive deputy chief of the fraud section in the Justice Department,",
      "the Jewish Coalition of Stars",
      "the Jewish Coalition",
      "In a recent speech to the Jewish Coalition"
    )

    val extractor = new ImplicitRelationExtractor(TaggerLoader.defaultTagger)

    val trees = phrases.map(line => extractor.getParse(line)._1)

    val headFinder = new SemanticHeadFinder()

    val heads = trees.map(tree => headFinder.determineHead(tree))
    val terminals = trees.map(tree => tree.headTerminal(headFinder))

    heads.foreach(tree => tree.pennPrint())
    terminals.map(t => t.value())
    terminals.foreach(println)

  }
}
