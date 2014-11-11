import java.util

import edu.knowitall.repr.sentence
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.TaggerCollection
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.stanford.nlp.ling
import edu.stanford.nlp.ling.{CoreLabel, Word}
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees._

import scala.collection.JavaConversions._

/**
 * Created by Gene on 11/10/2014.
 */
class NounToNounRelationExtractor(tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized]) {
  private val PARSER_MODEL = "models/englishPCFG.ser.gz"

  private val parser = LexicalizedParser.loadModel(PARSER_MODEL)
  private val lemmatizer = MorphaStemmer

  def process(text: String): sentence.Sentence with Chunked with Lemmatized = {
    new sentence.Sentence(text) with Chunker with Lemmatizer {
      val chunker = tagger.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  def extractRelations(line: String): List[Relation] = {
    // Process uses the same chunker.
    val a = process(line)
    val types = tagger.tag(a).toList
    val tokens = tagger.chunker.chunk(line)

    val sent: scala.collection.immutable.List[Word] = tokens.toList.map(a => new Word(a.string))
    val rawWords: util.List[CoreLabel] = ling.Sentence.toCoreLabelList(sent)
    val parse: Tree = parser.apply(rawWords)

    val tlp: TreebankLanguagePack = new PennTreebankLanguagePack
    val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory
    val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parse)
    val tdl: List[TypedDependency] = gs.typedDependenciesCCprocessed.toList
//    tdl.map(td => td.reln())



    Nil
  }
}
