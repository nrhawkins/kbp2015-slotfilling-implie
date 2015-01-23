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
import utils.{ExtractionUtils, SerializationUtils}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Basic implicit relation extractor.
 *
 * Identifies relation terms from the tagger passed into the constructor.
 * Then expands on specific dependency relations from the Stanford Parser to
 * extract the entity that the relation term modifies.
 *
 * The rules for the extraction are in resources/extractor.conf.
 */
class ImplicitRelationExtractor(
    tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized],
    serializedTokenCacheFile: String = null,
    serializedParseCacheFile: String = null) {

  val config = ConfigFactory.load("extractor.conf")

  // Parser.
  private val PARSER_MODEL = config.getString("parser-model-file")
  private val parser = LexicalizedParser.loadModel(PARSER_MODEL)

  // Patterns.
  private val relationPatterns =
    constructRelationPatterns(config.getConfigList("relation-patterns").toList)
  private val tagId = config.getString("tag-id")
  private val enclosingPunctuation = constructEnclosingPunctuation(
    config.getConfigList("enclosing-punctuation").toList)

  // Expansion functions.
  private val expansionFunctions = new ExpansionFunctions

  // Memo function caches.
  private val tagCache = mutable.Map[String, List[Type]]()
  private val parseCache = mutable.Map[String, (Tree, List[TypedDependency])]()
  private val tokenCache = mutable.Map[String, Seq[ChunkedToken]]()

  // Serialized caches.
  private val serializedTokenCache = if (serializedTokenCacheFile != null) {
    SerializationUtils.loadSerializedTokenizedSentences(serializedTokenCacheFile)
  } else { null }
  private val serializedParseCache = if (serializedParseCacheFile != null) {
    SerializationUtils.loadSerializedParses(serializedParseCacheFile)
  } else { null }

  // Used for tokenizer.
  def process(text: String): sentence.Sentence with Chunked with Lemmatized = {
    new sentence.Sentence(text) with Chunker with Lemmatizer {
      val chunker = tagger.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  /**
   * Extracts implicit relations from a string.
   * @param line String, line of text to extract.
   * @return List[ImplicitRelation], list of relations extracted from the string.
   */
  def extractRelations(line: String): List[ImplicitRelation] = {
    // Process uses the same chunker.
    val tags = getTags(line)
    val tokens = getTokens(line)
    val (parse, tdl) = getParse(line)

    // Add indices to the tree for the relation identifying phase.
    parse.indexLeaves()

    // Raw extractions in terms of typed dependency lists
    val processedTdl = rawExtractionTDLs(tags, tdl, tokens)

    val eeFn: EntityExtractionFunction =
      EntityExtractionFunctions.firstNounPhraseAncestor

    // Refined results as noun to noun relations
    val relations = implicitRelationsFromRawExtractions(
      parse, processedTdl, tokens, line, eeFn)

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
    if (serializedTokenCacheFile == null) {
      tokenCache.get(line) match {
        case Some(tokens) => tokens
        case None =>
          val newtokens = tagger.chunker.chunk(line)
          tokenCache.put(line, newtokens)
          newtokens
      }
    } else {
      serializedTokenCache.get(line) match {
        case Some(tokens) => tokens
        case None =>
          tokenCache.get(line) match {
            case Some(tokens) => tokens
            case None =>
              val newtokens = tagger.chunker.chunk(line)
              tokenCache.put(line, newtokens)
              SerializationUtils.addSerializedChunkedSentence(
                serializedTokenCacheFile, line, newtokens)
              newtokens
          }
      }
    }
  }

  // Memoized & serially cached parser.
  def getParse(line: String): (Tree, List[TypedDependency]) = {
    def calculateParse = {
      val tokens = getTokens(line)
      val tokenizedSentence = tokens.toList.map(a => new Word(a.string))
      val rawWords = Sentence.toCoreLabelList(tokenizedSentence)
      val parse = parser.apply(rawWords)

      val tlp = new PennTreebankLanguagePack
      val list = tlp.grammaticalStructureFactory
                 .newGrammaticalStructure(parse)
                 .typedDependenciesCCprocessed.toList
      (parse, list)
    }

    if (serializedParseCacheFile == null) {
      parseCache.get(line) match {
        case Some(parse) => parse
        case None =>
          val (parse, list) = calculateParse
          parseCache.put(line, (parse, list))
          (parse, list)
      }
    } else {
      serializedParseCache.get(line) match {
        case Some(parse) => parse
        case None =>
          parseCache.get(line) match {
            case Some(parse) => parse
            case None =>
              val (parse, list) = calculateParse
              parseCache.put(line, (parse, list))
              SerializationUtils.addSerializedObject(serializedParseCacheFile,
                new ParseEntry(line, parse, new util.ArrayList(list)))
              (parse, list)
          }
      }
    }
  }

  def getEnclosingPunctuation = enclosingPunctuation


  /*

  Private functions.

   */

  // Extracts the entity and created a list of ImplicitRelation classes from
  // the raw dependency list relation data.
  private def implicitRelationsFromRawExtractions(
    parseTree: Tree,
    nntdls: List[RawTDLRelation],
    tokens: Seq[ChunkedToken],
    sentence: String,
    entityExtractionFn: EntityExtractionFunction): List[ImplicitRelation] = {

    nntdls.map(nntdl =>
      new ImplicitRelation(nntdl.tag.tag, nntdl.tag.relation,
        entityExtractionFn(parseTree, nntdl.tdl, nntdl.tag.tag, tokens, sentence, this),
        nntdl.tag.sentence, nntdl.tag.relationTrace))
          .filter(nnr => nnr.np != null)
  }


  // Relation filter generator.
  private def genRelationFilter(relations: Set[String])(td: TypedDependency) = {
     relations.contains(td.reln().getShortName)
  }


  // Givens an id, id value, a list of rules returns a function that checks that a typed dependency
  // satisfies the rules and identifier constraints.
  // If satisfied, returns the next step id and idValue.
  // NOTE: NOT THREADSAFE!
  private def expandIdByRules(id: String, idValue: IndexedString, rules: List[Rule],
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
  private def expandByPattern(tdl: List[TypedDependency],
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

  // Returns a raw extraction in terms of the tag and the expanded dependencies.
  private def rawExtractionTDLs(tags: List[Type], tdl: List[TypedDependency],
    tokens: Seq[ChunkedToken]): List[RawTDLRelation] = {

    val tagMap = ExtractionUtils.createTagMap(tags)

    val tagWords = tdl
      .map(td => tagMap.getOrElse(td.dep.index, tagMap.getOrElse(td.gov.index, null)))
      .filter(w => w != null).toSet.toList

    val expansions = tagWords.map(tag =>
      (tag, expandByPattern(tdl, tagId, tag.asIndexedString, relationPatterns, tokens)))

    expansions.map(pair => {
        val nnTag = new ImplicitRelation(pair._1, pair._1.tag,
          IndexedSubstring.emptyInstance, "", pair._2)
        RawTDLRelation(pair._2, nnTag)
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
