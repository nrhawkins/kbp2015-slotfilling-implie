package extractor

import com.typesafe.config.{ConfigFactory, Config}
import edu.knowitall.repr.sentence.{Sentence, Chunked, Lemmatized}
import edu.knowitall.taggers.TaggerCollection
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.CoreLabel

import scala.collection.mutable

import scala.collection.JavaConversions._

/**
 * An ImplicitRelationExtractor that also filters by NER types.
 *
 * The filters for each tag are specified in resources/ner-filtered-ire.conf.
 */
class NERFilteredIRE
  (tagger: TaggerCollection[Sentence with Chunked with Lemmatized],
   serializedTokenCacheFile: String = null,
   serializedParseCacheFile: String = null)
  extends ImplicitRelationExtractor(
    tagger, serializedTokenCacheFile, serializedParseCacheFile)
  // filterNERs function.
  with NERFilterByAnyInEntity {

  val nerConfig = ConfigFactory.load("ner-filtered-ire.conf")
  protected val expectedEntities =
    expectedTagEntities(nerConfig.getConfigList("tag-entities").toList)
  protected val NER_MODEL = nerConfig.getString("ner-model-file")
  protected val classifier: CRFClassifier[CoreLabel] = CRFClassifier.getClassifier(NER_MODEL)
  protected val NER_TAGS_TO_IGNORE = nerConfig.getStringList("ner-tag-ignore").toList

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val unfiltered = super.extractRelations(line)
    filterNERs(line, unfiltered, NER_TAGS_TO_IGNORE, expectedEntities)
  }
}
