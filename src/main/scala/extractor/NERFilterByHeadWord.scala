package extractor

/**
 * Trait that provides a method for filtering by NER tags on the head word of
 * each extracted relation.
 *
 * The provided only returns extracted relations where the head word is tagged
 * as one of the expected NER types for the given relation class of the
 * extraction.
 */
trait NERFilterByHeadWord extends NERFilterable {
  def addHeadsToExtractions(extractions: List[ImplicitRelation])

  // Checking that the head word has the expected NER tag.
  def filterNERsByHeadWord(src: String, relations: List[ImplicitRelation],
                 nerTagsToIgnore: List[String],
                 expectedEntities: Map[String, List[String]]): List[ImplicitRelation] = {
    addHeadsToExtractions(relations)

    // Add NER tags for each extraction.
    val taggedNERs = tagNERs(relations, src, nerTagsToIgnore)

    // Filter out NERs that don't match the keyword tag's expected entity type.
    taggedNERs.foreach(extraction => {
      val ners = extraction.getNERs
      val tag = extraction.relation
      extraction.setNERs(
        ners.filter(ner => expectedEntities.getOrElse(tag, Nil).contains(ner.ner)))
    })

    // Filter out words where the head word isn't one of the NER tags.
    taggedNERs.filter(extraction =>
      extraction.ners.foldLeft(false)((acc, cur) =>
        cur.entityString.contains(extraction.head.string) || acc))
  }
}
