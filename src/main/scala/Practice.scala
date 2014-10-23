import java.io.StringReader

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.{Tokenizer, TokenizerFactory, CoreLabelTokenFactory, PTBTokenizer}

/**
 * Created by Gene on 10/23/2014.
 */
object Practice {
  def main(args: Array[String]) {
    // This option shows loading and using an explicit tokenizer
    val sent2: String = " \"Black holes are infamous for wreaking havoc in their immediate neighborhoods,\" said Neil deGrasse Tyson, an astrophysicist with the American Museum of Natural History who was not involved in the 3C321 research, but is an expert on black holes."
    val tokenizerFactory: TokenizerFactory[CoreLabel] = PTBTokenizer.factory(new CoreLabelTokenFactory, "")
    val tok: Tokenizer[CoreLabel] = tokenizerFactory.getTokenizer(new StringReader(sent2))
    val rawWords2 = tok.tokenize()
    /*for (word <- rawWords2.iterator()) {
      println(word)
    }*/
    val iter = rawWords2.iterator()
    var i = 0
    while (iter.hasNext) {
      i += 1
      println(iter.next().toString + "-" + i)
    }
  }
}
