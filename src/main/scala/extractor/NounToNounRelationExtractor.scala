package extractor

import java.util

import edu.knowitall.repr.sentence
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.TaggerCollection
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type
import edu.stanford.nlp.ling
import edu.stanford.nlp.ling.{CoreLabel, IndexedWord, Word}
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees._

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * TODO: add comment
 * Created by Gene on 11/10/2014.
 */
class NounToNounRelationExtractor(tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized]) {
  private val PARSER_MODEL = "models/englishPCFG.ser.gz"

  private val parser = LexicalizedParser.loadModel(PARSER_MODEL)
  private val lemmatizer = MorphaStemmer

  private val nounRelations = Set("nn")
  private val tagCache = mutable.Map[String, List[Type]]()
  private val parseCache = mutable.Map[String, (Tree, List[TypedDependency])]()

  type Phrase = String
  type TagName = String
  case class TagInfo(tag: String, text: String)
  case class NounToNounTD(td: TypedDependency, tag: NounToNounRelation)

  def process(text: String): sentence.Sentence with Chunked with Lemmatized = {
    new sentence.Sentence(text) with Chunker with Lemmatizer {
      val chunker = tagger.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  def extractRelations(line: String): List[NounToNounRelation] = {
    // Process uses the same chunker.
    val tags = getTags(line)
    val tokens = tagger.chunker.chunk(line)
    val (parse, tdl) = getParse(line)

//    println("Tags")
//    for (tag <- tags) {
//      println(s"${tag.name}, ${tag.text}, ${tag.tokenInterval}, ${tag.source}")
//    }
//    println()

    /*
    .gov()  [govenor: modified word]
    .dep()  [dependent: class term word]
    .reln() [relation]
     */

      /*
      Steps:
        1. filter dependencies that have tagged depedency
        2. filter dependencies that are noun-to-noun relations
        3. Obtain the noun phrases that contain the dependency
        4. Construct the relation out of the parser.
          - identify noun-to-noun relations in the parse
          - find the noun-phrase we want
       */

    val processedTdl = processDependencies(tags, tdl)

      //    printTdl(processedTdl, "Processed Tdls")

    parse.indexLeaves()

    val results = getNounToNounRelations(parse, processedTdl, tokens, line)
    results.foreach(nnr => nnr.sentence = line)

      //    printNounToNounRelations(results)
    results
  }

  def getTags(line: String): List[Type] = {
    tagCache.get(line) match {
      case None =>
        val newtags = tagger.tag(process(line)).toList
        tagCache.put(line, newtags)
        newtags
      case Some(tags) => tags
    }
  }

  def getParse(line: String): (Tree, List[TypedDependency]) = {
    parseCache.get(line) match {
      case None =>
        val tokens = tagger.chunker.chunk(line)
        val tokenizedSentence: scala.collection.immutable.List[Word] = tokens.toList.map(a => new Word(a.string))
        val rawWords: util.List[CoreLabel] = ling.Sentence.toCoreLabelList(tokenizedSentence)
        val parse: Tree = parser.apply(rawWords)

        val tlp: TreebankLanguagePack = new PennTreebankLanguagePack
        val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory
        val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parse)
        val list = gs.typedDependenciesCCprocessed.toList
        parseCache.put(line, (parse, list))
        (parse, list)
      case Some(parse) => parse
    }
  }

  /*
  To get hops from tag:
    Get word that is modified by tag: the govenor word
    go through every dependency, where the tagged word is included.
    Get all dependencies where the dep of the previous
  */
  // Pre: tdl has all the tagged dependencies for a specific tag
  // tagged and tdl must come from the same extraction.  Otherwise the result is undefined.
  // Pull out any hop that includes the tag's govenor word.
  def getSingleHops(tagged: List[TypedDependency], tdl: List[TypedDependency]): List[TypedDependency] = {
    tagged.map(tagDep => tdl.filter(td => td.gov().endPosition() == tagDep.gov().endPosition() || td.dep().endPosition() == tagDep.gov().endPosition())).flatten
  }

  private def printNounToNounRelations(relns: Iterable[NounToNounRelation]) {
    println("NounToNounRelations")
    for (reln <- relns) {
      println(reln)
    }
    println()
  }

  private def getNounToNounRelations(parseTree: Tree, tdl: List[TypedDependency], tokens: Seq[ChunkedToken], sentence: String): List[NounToNounRelation] = {
    tdl.map(td =>
      new NounToNounRelation(
        new IndexedString(td.dep().value(), td.dep().index()), td.dep.tag(),
        getNounPhrase(parseTree, td, tokens, sentence)))
  }

  private def getNounPhrase(tree: Tree, td: TypedDependency, tokenizedSentence: Seq[ChunkedToken], sentence: String): IndexedString = {
    // Assume that the full tag noun phrase is a child of the noun phrase
    // containing the dep and gov.
    // If incorrect - TO-DO: expand dep to the full phrase.
    def getNPRoot(tree: Tree, dep: IndexedWord, gov: IndexedWord): (Tree, Boolean, Boolean) = {
      if (tree.isLeaf) {
        val label = tree.label()toString()
        if (label.equalsIgnoreCase(s"${dep.value()}-${dep.index()}")) {
          (tree, true, false)
        } else if (label.equalsIgnoreCase(s"${gov.value()}-${gov.index()}")) {
          (tree, false, true)
        } else {
          (tree, false, false)
        }
      } else {
        val childResults = tree.children()
          .map(t => getNPRoot(t, dep, gov))
        val correctChildren = childResults.filter(t => t._1.label().toString == "NP" && t._2 && t._3)
        if (correctChildren.size > 0) {
          return correctChildren(0)
        }
        val curResult = childResults.foldLeft((false, false))((acc, cur) => (acc._1 || cur._2, acc._2 || cur._3))
        (tree, curResult._1, curResult._2)
      }
    }

    def phraseTokensFromTree(tree: Tree): List[IndexedString] = {
      val labels = tree.`yield`().map(l => l.toString).toList
      // Only labels of leaf nodes will have a dash.
      // All others will be a name for a phrase type.
      labels.filter(l => l.contains("-"))
            .map(l => {
              val (string, negIndex) = l.splitAt(l.lastIndexOf('-'))
              new IndexedString(string, 0 - negIndex.toInt)
            })
    }

    // Get root of noun phrase that contains both the dependent and govenor words.
    val npRoot = getNPRoot(tree, td.dep, td.gov)._1

    // Cut out the appropriate noun phrase from the sentence.
    // Phrase tokens don't have character offsets so get them from the chunked sentence.
    val phraseTokens = phraseTokensFromTree(npRoot)
    val firstChunk = tokenizedSentence(phraseTokens(0).index - 1)
    val lastChunk = tokenizedSentence(phraseTokens(phraseTokens.size - 1).index - 1)

    new IndexedString(
      sentence.substring(firstChunk.offset, lastChunk.offset + lastChunk.string.length),
      phraseTokens(phraseTokens.size - 1).index)
  }

  private def printLabels(tree: Tree) {
    println(s"${tree.label()}, ${tree.isLeaf}")
    for (child <- tree.children()) {
      printLabels(child)
    }
  }

  private def printTdl(tdl: List[TypedDependency], message: String) {
    println(message)
    for (i <- 0 until tdl.size){
      val td = tdl(i)
      println(s"${td.dep()}, ${td.gov()}, ${td.reln().getShortName}, ${td.dep().tag()}")
    }
    println()
  }

  private def processDependencies(tags: List[Type], tdl: List[TypedDependency]): List[TypedDependency] = {
    filterDependencyByRelations(getTaggedDependencies(tags, tdl))
  }

  private def filterDependencyByRelations(tdl: List[TypedDependency]): List[TypedDependency] = {
    tdl.filter(td =>
      nounRelations.contains(td.reln().getShortName)
    )
  }

  def expandByOneHop(curtdl: List[List[TypedDependency]], tdl: List[TypedDependency]): List[List[TypedDependency]] = {
    curtdl.foldLeft(List[List[TypedDependency]]())((acc, cur: List[TypedDependency]) =>
      tdl.filter(td =>
        (cur.head.dep().index() == td.dep().index() ||
        cur.head.dep().index() == td.gov().index() ||
        cur.head.gov().index() == td.dep().index() ||
        cur.head.gov().index() == td.gov().index()) &&
        !cur.contains(td)
      ).map(td => td::cur)
    )
  }

  def expandAllByOneHop(tdlMaps: Map[TagInfo, List[List[TypedDependency]]],
       tdl: List[TypedDependency]): Map[TagInfo, List[List[TypedDependency]]] = {
    val newMap = mutable.Map[TagInfo, List[List[TypedDependency]]]()
    for ((k, v) <- tdlMaps) {
      newMap.put(k, expandByOneHop(v, tdl))
    }
    newMap.toMap
  }

  def dependencyHopsByTag(tags: List[Type], tdl: List[TypedDependency])
      : Map[TagInfo, List[TypedDependency]] = {
    val tagMap = createTagMap(tags)

    val results = mutable.Map[TagInfo, List[TypedDependency]]()

    for (td <- tdl) {
      tagMap.get(td.dep().index()) match {
        case None => null
        case Some(tagInfo: TagInfo) =>
          results.get(tagInfo) match {
            case None => results.put(tagInfo, td::Nil)
            case Some(current) => results.put(tagInfo, td::current)
          }
      }
      tagMap.get(td.gov().index()) match {
        case None => null
        case Some(tagInfo: TagInfo) =>
          results.get(tagInfo) match {
            case None => results.put(tagInfo, td::Nil)
            case Some(current) => results.put(tagInfo, td::current)
          }
      }
    }
    results.toMap
  }

  def getTaggedDependencies(
      tags: List[Type], tdl: List[TypedDependency]): List[TypedDependency] = {
    // 1. turn tags into a map of index to phrase and tag
    val tagMap = createTagMap(tags)

    // 2. Update the td to include the tag, make any without tag null
    // 3. And filter out nulls.
    tdl.map(td =>
      tagMap.get(td.dep().index()) match {
        case None => null
        case Some(tagInfo: TagInfo) =>
          td.dep().setTag(tagInfo.tag)
          td
      }).filter(td => td != null)
  }

  private def tagTypedDeps(tagMap: Map[Int, TagInfo], tdl: List[TypedDependency]): List[NounToNounTD] = {
    tdl.map(td => {
        tagMap.get(td.dep().index) match {
          case Some(tag: TagInfo) => NounToNounTD(td,
            new NounToNounRelation(
              new IndexedString(tag.text, -1), tag.tag,
              new IndexedString("", -1)))
          case _ => NounToNounTD(null, null)
        }
      }
    )
  }

  // pre: each index can have at most 1 tag
  private def createTagMap(tags: List[Type]): Map[Int, TagInfo] = {
    def tagMapHelper (acc: Map[Int, TagInfo], tags: List[Type]): Map[Int, TagInfo] = {
      tags match {
        case Nil => acc
        case (tag :: tail) =>
          val newacc = acc + ((tag.tokenInterval.end, TagInfo(tag.name, tag.text)))
          tagMapHelper(newacc, tail)
      }
    }
    tagMapHelper(Map(), tags)
  }
}
