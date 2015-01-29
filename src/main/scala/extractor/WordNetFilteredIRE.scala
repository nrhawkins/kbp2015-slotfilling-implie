package extractor

import java.net.URL

import com.typesafe.config.{Config, ConfigFactory}
import edu.knowitall.repr.sentence.{Lemmatized, Chunked, Sentence}
import edu.knowitall.taggers.TaggerCollection
import edu.mit.jwi.Dictionary

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
  with WordNetFilterable {

  protected val wordnetConfig = ConfigFactory.load("wordnet-filtered-ire.conf")
  protected val wordnetFilters =
    getWordnetFilters(wordnetConfig.getConfigList("wordnet-filters").toList)
  protected val wordnetDictionary = new Dictionary(new URL(
    "file", null, wordnetConfig.getString("wordnet-dictionary")))

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val unfiltered = super.extractRelations(line)
    addHeadsToExtractions(unfiltered)
    filterWordNet(line, unfiltered)
  }

  /**
   * Get the wordnet filter definitions from the config file.
   * @param confs Config that contains the wordnet filter information.
   * @return Mapping of tags to the corresponding wordnet filter definition.
   */
  private def getWordnetFilters(confs: List[Config]): Map[String, WordNetFilter] = {
    def getOrEmpty(conf: Config, field: String): List[String] = {
      if (conf.hasPath(field)) {
        conf.getStringList(field).toList
      } else {
        Nil
      }
    }

    def filterLists(conf: Config) = {
      FilterLists(getOrEmpty(conf, "hypernyms"), getOrEmpty(conf, "hypernym-instances"))
    }

    val map = scala.collection.mutable.HashMap[String, WordNetFilter]()
    confs.foreach(conf => {
      val name = conf.getString("tag")
      map.put(name, WordNetFilter(name, filterLists(conf.getConfig("accept")),
        filterLists(conf.getConfig("reject"))))
    })
    map.toMap
  }
}
