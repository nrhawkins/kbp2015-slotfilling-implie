package extractor

import edu.knowitall.repr.sentence
import edu.knowitall.repr.sentence.{Lemmatized, Chunked}
import edu.knowitall.taggers.TaggerCollection

/**
 * Extractor with no filters and generalized expansion rules.
 * For most recall, the tagger should use case insensitive taggers with no
 * ignored tags.
 */
class HighRecallImplIE(
  tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized],
  serializedTokenCacheFile: String = null,
  serializedParseCacheFile: String = null)
  extends ImplicitRelationExtractor(tagger, serializedTokenCacheFile,
    serializedParseCacheFile, "high-recall-extractor.conf") {

  override def extractRelations(line: String): List[ImplicitRelation] = {
    // Avoid the head filter.
    super.unfilteredExtractions(line)
  }
}