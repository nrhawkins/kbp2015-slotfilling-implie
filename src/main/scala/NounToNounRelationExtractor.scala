import java.util

import edu.knowitall.repr.sentence
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.TaggerCollection
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type
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

  type Phrase = String
  type TagName = String
  case class TagInfo(tag: String, tagClass: String, text: String)
  case class NounToNounTD(td: TypedDependency, tag: NounToNounRelation)

  def process(text: String): sentence.Sentence with Chunked with Lemmatized = {
    new sentence.Sentence(text) with Chunker with Lemmatizer {
      val chunker = tagger.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  def extractRelations(line: String): List[NounToNounRelation] = {
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

    /*
    .gov()  [govenor: modified word]
    .dep()  [dependent: class term word]
    .reln() [relation]
     */

    /*
    Steps:
      1. filter dependencies that have tagged govenors
      2. filter dependencies that are noun-to-noun relations
      3. Obtain the noun phrases that contain the dependency
      4. Construct the relation out of the
     */

//    tdl.map(td => td.reln())



    Nil
  }
/*
  private def getTaggedDependencies(
      tags: List[Type], tdl: List[TypedDependency]): List[TypedDependency] = {
    // 1. turn tags into a map of index to phrase and tag
    // 2. Filter out dependencies with tagged dependents
    // 3. Update the td to include the tag
    val tagMap = createTagMap(tags)
    val rawTaggedDeps = filterTags(tagMap, tdl)

  }

  private def tagTypedDeps(tagMap: Map[Int, (Phrase, TagName)], tdl: List[TypedDependency]): List[NounToNounTD] = {
    tdl.map(td => {
        tagMap.get(td.dep().index) match {
          case Some((index, tag)) => NounToNounTD (td, new NounToNounRelation(tag.))
          case _ => new NounToNounRelation("", "", "")
        }
      }
    )
  }

*/
  private def filterTags(
      tagMap: Map[Int, (Phrase, TagName)], tdl: List[TypedDependency]): List[TypedDependency] = {
    tdl.filter(td => tagMap.contains(td.dep().index()))
  }
  // pre: each index can have at most 1 tag
  private def createTagMap(tags: List[Type]): Map[Int, TagInfo] = {
    def tagMapHelper (acc: Map[Int, TagInfo], tags: List[Type]): Map[Int, TagInfo] = {
      tags match {
        case Nil => acc
        case (tag :: tail) =>
          val newacc = acc + ((tag.tokenInterval.end, TagInfo(tag.name, tag.source, tag.text)))
          tagMapHelper(newacc, tail)
      }
    }
    tagMapHelper(Map(), tags)
  }
}
