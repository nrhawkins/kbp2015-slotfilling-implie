package extractor

import java.net.URL

import com.typesafe.config.ConfigFactory
import edu.knowitall.repr.sentence.{Lemmatized, Chunked, Sentence}
import edu.knowitall.taggers.TaggerCollection
import edu.mit.jwi.Dictionary
import edu.mit.jwi.morph.WordnetStemmer
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.CoreLabel

import scala.collection.JavaConversions._

/**
 * Created by Gene on 1/29/2015.
 */
class FormalConstrainedImplIE
  (tagger: TaggerCollection[Sentence with Chunked with Lemmatized],
   serializedTokenCacheFile: String = null,
   serializedParseCacheFile: String = null)
  extends ImplicitRelationExtractor(
    tagger, serializedTokenCacheFile, serializedParseCacheFile)
  // filterWordNet function.
  with WordNetHypernymFilter
  // filterNERs function.
  with NERFilterByHeadWord {

  protected val wordnetConfig = ConfigFactory.load("wordnet-filtered-ire.conf")
  protected val wordnetFilterParams =
    getWordnetFilters(wordnetConfig.getConfigList("wordnet-filters").toList)
  protected val wordnetDictionary = new Dictionary(new URL(
    "file", null, wordnetConfig.getString("wordnet-dictionary")))
  val stemmer = new WordnetStemmer(wordnetDictionary)

  val nerConfig = ConfigFactory.load("ner-filtered-ire.conf")
  protected val expectedEntities =
    expectedTagEntities(nerConfig.getConfigList("tag-entities").toList)
  protected val NER_MODEL = nerConfig.getString("ner-model-file")
  protected val classifier: CRFClassifier[CoreLabel] = CRFClassifier.getClassifier(NER_MODEL)
  protected val NER_TAGS_TO_IGNORE = nerConfig.getStringList("ner-tag-ignore").toList

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val unfiltered = super.extractRelations(line)
    addHeadsToExtractions(unfiltered)
    // Return everything that satisfies WordNet or NER.
    val wordnetFiltered = filterWordNet(line, unfiltered)
    val nerFiltered = filterNERs(line, unfiltered)
    println("extract relations.")
    println(unfiltered)
    println(wordnetFiltered)
    println(nerFiltered)
    println(wordnetFiltered.toSet union nerFiltered.toSet)
    (wordnetFiltered.toSet union nerFiltered.toSet).toList
  }
}
