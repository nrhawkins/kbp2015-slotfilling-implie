package extractor

import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence
import edu.knowitall.taggers.TaggerCollection

/**
 * ImplIE with all basic filters.
 *
 * List of filters:
 *  NN of ...
 *  ... vs. ...
 *  Days of week at beginning or end or entity
 *  strange date formats (... UTC)
 */
class ImplIEWithBasicFilters(
  tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized],
  serializedTokenCacheFile: String = null,
  serializedParseCacheFile: String = null)
  extends ImplicitRelationExtractorNoLists(
    tagger, serializedTokenCacheFile, serializedParseCacheFile)
  with BasicFilterFunctions {

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val relations = super.extractRelations(line)

//    println(s"Before filtering $relations")

    var filtered = relations
    filtered = filterNNOfEntities(line, filtered)
//    println(s"After NN of filter $filtered")
    filtered = filterDaysOfWeek(line, filtered)
//    println(s"After days of week filter $filtered")
    filtered = filterStrangeDateFormats(line, filtered)
//    println(s"After strange date filter $filtered")
    filtered = filterVsEntities(line, filtered)
//    println(s"After vs entity filter $filtered")
    filtered
  }

}
