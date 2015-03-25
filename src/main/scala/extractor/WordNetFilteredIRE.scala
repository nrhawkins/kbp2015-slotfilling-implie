package extractor

import java.net.URL

import com.typesafe.config.ConfigFactory
import edu.knowitall.repr.sentence.{Lemmatized, Chunked, Sentence}
import edu.knowitall.taggers.TaggerCollection
import edu.mit.jwi.Dictionary
import edu.mit.jwi.morph.WordnetStemmer

import scala.collection.JavaConversions._

/**
 * An ImplicitRelationExtractor that also filters by WordNet types.
 * The filtering finds the head word of the extraction, then looks at its
 * WordNet type for filtering.
 *
 * Since WordNet only
 *
 * The filters for each tag are specified in resources/wordnet-filtered-ire.conf.
 */
class WordNetFilteredIRE
  (tagger: TaggerCollection[Sentence with Chunked with Lemmatized],
   serializedTokenCacheFile: String = null,
   serializedParseCacheFile: String = null)
  extends ImplicitRelationExtractor(
    tagger, serializedTokenCacheFile, serializedParseCacheFile)
  with WordNetHypernymFilter {

  protected val wordnetConfig = ConfigFactory.load("wordnet-filtered-ire.conf")
  protected val wordnetFilterParams =
    getWordnetFilters(wordnetConfig.getConfigList("wordnet-filters").toList)
  protected val wordnetDictionary = new Dictionary(new URL(
    "file", null, wordnetConfig.getString("wordnet-dictionary")))
  val stemmer = new WordnetStemmer(wordnetDictionary)
  val wordSenseLimit = wordnetConfig.getInt("word-sense-limit")

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val unfiltered = super.extractRelations(line)
    filterWordNet(line, unfiltered)
  }
}
