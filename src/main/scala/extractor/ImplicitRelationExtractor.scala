package extractor

import java.util

import com.typesafe.config.{Config, ConfigFactory}
import edu.knowitall.repr.sentence
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.TaggerCollection
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type
import edu.stanford.nlp.ling.{Sentence, _}
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees._
import utils.SerializationUtils

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * TODO: add comment
 * TODO: consider replacing IndexedString with stanford's IndexedWord
 *
 * Created by Gene on 11/10/2014.
 */

// TODO: make a file with all the case classes.

class ImplicitRelationExtractor(tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized]) {
  val config = ConfigFactory.load("extractor.conf")

  private val PARSER_MODEL = config.getString("parser-model-file")
  // TODO: make the parser and classifier arguments as well.
  private val parser = LexicalizedParser.loadModel(PARSER_MODEL)

  private val relationPatterns =
    constructRelationPatterns(config.getConfigList("relation-patterns").toList)
  private val tagId = config.getString("tag-id")
  private val enclosingPunctuation = constructEnclosingPunctuation(
    config.getConfigList("enclosing-punctuation").toList)

  private val expansionFunctions = new ExpansionFunctions

  // memo function caches
  private val tagCache = mutable.Map[String, List[Type]]()
  private val parseCache = mutable.Map[String, (Tree, List[TypedDependency])]()
  private val tokenCache = mutable.Map[String, Seq[ChunkedToken]]()

  // serialized caches
  private val serializedTokenFile = config.getString("tokenization-cache")
  private val serializedParseFile = config.getString("parse-cache")
  private val serializedTokenCache =
    SerializationUtils.loadSerializedTokenizedSentences(serializedTokenFile)
  private val serializedParseCache =
    SerializationUtils.loadSerializedParses(serializedParseFile)

  // Used for tokenizer.
  def process(text: String): sentence.Sentence with Chunked with Lemmatized = {
    new sentence.Sentence(text) with Chunker with Lemmatizer {
      val chunker = tagger.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  // Extracts implicit relations from a string.
  def extractRelations(line: String): List[ImplicitRelation] = {
    // Process uses the same chunker.
    val tags = getTags(line)
    val tokens = getTokens(line)
    val (parse, tdl) = getParse(line)

    // Add indices to the tree for the relation identifying phase.
    parse.indexLeaves()

    // Raw extractions in terms of typed dependency lists
    val processedTdl = rawExtractionTDLs(tags, tdl, tokens)

//    val npFn: NounPhraseFunction = NounPhraseFunctions.firstNounPhraseAncestor
    val eeFn: EntityExtractionFunction = EntityExtractionFunctions.ignoreNP

    // Refined results as noun to noun relations
    val relations =
      implicitRelationsFromRawExtractions(parse, processedTdl, tokens, line, eeFn)

    // Add the full sentence to the results.
    relations.foreach(nnr => nnr.sentence = line)

    relations
  }

  // Memoized tagger.
  def getTags(line: String): List[Type] = {
    tagCache.get(line) match {
      case None =>
        val newtags = tagger.tag(process(line)).toList
        tagCache.put(line, newtags)
        newtags
      case Some(tags) => tags
    }
  }

  // Memoized & serially cached tokenizer.
  def getTokens(line: String): Seq[ChunkedToken] = {
    serializedTokenCache.get(line) match {
      case Some(tokens) => tokens
      case None =>
        tokenCache.get (line) match {
          case Some(tokens) => tokens
          case None =>
            val newtokens = tagger.chunker.chunk(line)
            tokenCache.put(line, newtokens)
            SerializationUtils.addSerializedChunkedSentence(
              serializedTokenFile, line, newtokens)
            newtokens
        }
    }
  }

  // Memoized & serially cached parser.
  def getParse(line: String): (Tree, List[TypedDependency]) = {
    serializedParseCache.get(line) match {
      case Some(parse) => parse
      case None =>
        parseCache.get (line) match {
          case Some(parse) => parse
          case None =>
            val tokens = getTokens (line)
            val tokenizedSentence = tokens.toList.map (a => new Word (a.string) )
            val rawWords = Sentence.toCoreLabelList (tokenizedSentence)
            val parse = parser.apply (rawWords)

            val tlp = new PennTreebankLanguagePack
            val list = tlp.grammaticalStructureFactory
                       .newGrammaticalStructure(parse)
                       .typedDependenciesCCprocessed.toList
            parseCache.put(line, (parse, list))
            SerializationUtils.addSerializedObject(serializedParseFile,
              new ParseEntry(line, parse, new util.ArrayList(list)))
            (parse, list)
        }
    }
  }

  def getEnclosingPunctuation = enclosingPunctuation

  // pre: each index can have at most 1 tag
  // Creates a mapping from the token index to the TagInfo.
  def createTagMap(tags: List[Type]): Map[Int, TagInfo] = {
    def tagMapHelper (acc: Map[Int, TagInfo], tags: List[Type]): Map[Int, TagInfo] = {
      tags match {
        case Nil => acc
        case (tag :: tail) =>
          val newacc = acc + ((tag.tokenInterval.end, new TagInfo(tag)))
          tagMapHelper(newacc, tail)
      }
    }
    tagMapHelper(Map(), tags)
  }

  /**
   * Methods for testing.
   */

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

  def expandByOneHop(curtdl: List[List[TypedDependency]], tdl: List[TypedDependency]): List[List[TypedDependency]] = {
    curtdl.foldLeft(List[List[TypedDependency]]())((acc, cur: List[TypedDependency]) =>
      tdl.filter(td =>
        (cur.head.dep().index() == td.dep().index() ||
          cur.head.dep().index() == td.gov().index() ||
          cur.head.gov().index() == td.dep().index() ||
          cur.head.gov().index() == td.gov().index()) &&
          !cur.contains(td)
      ).map(td => td::cur):::acc
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

  /**
   * Private functions.
   */

  private def implicitRelationsFromRawExtractions(
    parseTree: Tree,
    nntdls: List[NounToNounTDL],
    tokens: Seq[ChunkedToken],
    sentence: String,
    entityExtractionFn: EntityExtractionFunction): List[ImplicitRelation] = {

    nntdls.map(nntdl =>
      new ImplicitRelation(nntdl.tag.tag, nntdl.tag.relation,
        entityExtractionFn(parseTree, nntdl.tdl, nntdl.tag.tag, tokens, sentence, this),
        nntdl.tag.sentence, nntdl.tag.relationTrace))
          .filter(nnr => nnr.np != null)
  }

  def substringFromWordIndicies(string: String, beginIndex: Int, endIndex: Int): String = {
    val tokens = getTokens(string)
    string.substring(tokens(beginIndex).offset,
      tokens(endIndex).offset + tokens(endIndex).string.length)
  }

  def genRelationFilter(relations: Set[String])(td: TypedDependency) = {
     relations.contains(td.reln().getShortName)
  }


  // Givens an id, id value, a list of rules returns a function that checks that a typed dependency
  // satisfies the rules and identifier constraints.
  // If satisfied, returns the next step id and idValue.
  // NOTE: NOT THREADSAFE!
  def expandIdByRules
    (id: String, idValue: IndexedString, rules: List[Rule],
     tokens: Seq[ChunkedToken], tdl: List[TypedDependency])
    (td: TypedDependency): (TypedDependency, String, IndexedString) = {

    expansionFunctions.prepareFunctions(id, idValue, rules, tokens, tdl)

    rules.foldLeft(null: (TypedDependency, String, IndexedString))((acc, cur) => {
      if (acc != null) {
        return acc
      }
      expansionFunctions.getFunctionForRelation(cur.rel)(td, cur)
    })
  }

  // Find relations that match the given id/idValue and satisfy the relation
  // rules.  Expand each of those relations by the corresponding pattern
  // and concatenate all the results.
  def expandByPattern(tdl: List[TypedDependency],
                      id: String,
                      idValue: IndexedString,
                      patterns: RelationPattern,
                      tokens: Seq[ChunkedToken]): List[TypedDependency] = {
    // filter by relation
    // map relations to the next hop id and idval
    // map to expand pattern on the filtered results
    // fold to merge
    val rules = patterns.getOrElse(id, Nil)
    if (rules == Nil) {
      return Nil
    }
    tdl.map(expandIdByRules(id, idValue, rules, tokens, tdl))
       .filter(x => x != null)
       .map(triple => triple._1::expandByPattern(tdl, triple._2, triple._3, patterns, tokens))
       .fold(Nil: List[TypedDependency])((acc, cur) => cur:::acc)
  }

  private def rawExtractionTDLs(tags: List[Type], tdl: List[TypedDependency],
                                  tokens: Seq[ChunkedToken]): List[NounToNounTDL] = {
    val tagMap = createTagMap(tags)

    val tagWords = tdl
      .map(td => tagMap.getOrElse(td.dep.index, tagMap.getOrElse(td.gov.index, null)))
      .filter(w => w != null).toSet.toList

    val expansions = tagWords.map(tag =>
      (tag, expandByPattern(tdl, tagId, tag.asIndexedString, relationPatterns, tokens)))

    expansions.map(pair => {
        val nnTag = new ImplicitRelation(pair._1, pair._1.tag,
          IndexedSubstring.emptyInstance, "", pair._2)
        NounToNounTDL(pair._2, nnTag)
      }
    )
  }

  // Constructs a mapping of relation patterns given the config object for those patterns.
  private def constructRelationPatterns(relConfs: List[Config]): RelationPattern = {
    def getRuleList(acc: List[Rule], rulesConf: List[Config]): List[Rule] = {
      rulesConf match {
        case Nil => acc
        case head::tail =>
          val ruleVals = head.getStringList("rule")
          val rule = Rule(ruleVals.get(0), ruleVals.get(1), ruleVals.get(2))
          getRuleList(rule::acc, tail)
      }
    }

    relConfs match {
      case Nil => Map[String, List[Rule]]()
      case head::tail =>
        val rulesConf = head.getConfigList("rules").toList
        val expansionId = head.getString("expansion-id")
        val patterns = getRuleList(Nil, rulesConf)
        constructRelationPatterns(tail) + ((expansionId, patterns))
    }
  }

  // Constructs a list of enclosing punctuation from the given config.
  private def constructEnclosingPunctuation(punctConfs: List[Config]): List[EnclosingPunctuation] = {
    punctConfs.map(pc => EnclosingPunctuation(pc.getString("open"), pc.getString("close")))
  }
}
