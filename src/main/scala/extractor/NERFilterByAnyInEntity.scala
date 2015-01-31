package extractor

/**
 * Original NER filter.
 *
 * It assumes nothing about the head word of the entity.
 * It only filters out entities that have NO words that don't match an
 * expected NER class of the relation tag.
 */
trait NERFilterByAnyInEntity extends NERFilterable {
  def filterNERs(src: String, relations: List[ImplicitRelation]): List[ImplicitRelation] = {
    // Add NER tags for each extraction.
    val taggedNERs = tagNERs(relations, src)

    // Filter out NERs that don't match the keyword tag's expected entity type.
    taggedNERs.foreach(extraction => {
      val ners = extraction.getNERs
      val tag = extraction.relation
      extraction.setNERs(
        ners.filter(ner => expectedEntities.getOrElse(tag, Nil).contains(ner.ner)))
    })

    // Filter out extractions where there are no remaining NER tags.
    val results = taggedNERs.filter(extraction => extraction.getNERs.size > 0)

    results
  }
}
