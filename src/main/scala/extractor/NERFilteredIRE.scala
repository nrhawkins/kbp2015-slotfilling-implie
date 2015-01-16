package extractor

import com.typesafe.config.{ConfigFactory, Config}
import edu.knowitall.repr.sentence.{Sentence, Chunked, Lemmatized}
import edu.knowitall.taggers.TaggerCollection
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel, Word}
import utils.ExtractionUtils

import scala.collection.mutable

import scala.collection.JavaConversions._

/**
 * An ImplicitRelationExtractor that also filters by NER types.
 *
 * The filters for each tag are specified in resources/ner-filtered-ire.conf.
 */
class NERFilteredIRE(tagger: TaggerCollection[Sentence with Chunked with Lemmatized])
  extends ImplicitRelationExtractor(tagger) {

  val nerConfig = ConfigFactory.load("ner-filtered-ire.conf")
  private val expectedEntities =
    expectedTagEntities(nerConfig.getConfigList("tag-entities").toList)
  private val NER_MODEL = nerConfig.getString("ner-model-file")
  private val classifier = CRFClassifier.getClassifier(NER_MODEL)

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val unfiltered = super.extractRelations(line)
    filterNERs(line, unfiltered)
  }

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


  def tagNERs(extractions: List[ImplicitRelation],
    line: String): List[ImplicitRelation] = {
    // Run NER tagger and pair in the entity portion (check the indicies).
    // TODO: make this a util function.
    val wordTokens = getTokens(line).map(token => new Word(token.string))
    val rawNERTags = classifier.classifySentence(wordTokens)

    // Filter the default NER tag and group NER tags together by answer annotations.
    val nerTags = rawNERTags
                  // Add token indicies to ner tags.
                  .foldLeft(Nil: List[(CoreLabel, Int)], 0)((acc, cur) =>
      ((cur, acc._2)::acc._1, acc._2 + 1))._1.reverse
                  // TODO: generalize this filter so that the string isn't hardcoded.
                  .filter(tag => tag._1.get(classOf[CoreAnnotations.AnswerAnnotation]) != "O")
                  .foldLeft(Nil: List[NERTag])((acc, cur) => {
      val curNER = cur._1.get(classOf[CoreAnnotations.AnswerAnnotation])
      val curIndex = cur._2
      acc match {
        case Nil => new NERTag(cur._1.word, curNER, curIndex, curIndex, cur::Nil)::Nil
        case head::tail =>
          if (curNER == head.ner && curIndex == head.endIndex + 1) {
            new NERTag(
              ExtractionUtils.substringFromWordIndicies(line, getTokens(line), head.beginIndex, curIndex),
              curNER, head.beginIndex, curIndex, cur::head.tokens)::tail
          } else {
            new NERTag(cur._1.word, curNER, curIndex, curIndex, cur::Nil)::
              head::tail
          }
      }
    }).map(tag => new NERTag(tag.entityString, tag.ner, tag.beginIndex,
      tag.endIndex, tag.tokens.reverse))

    // For each extraction find the NER tags that are within the extraction bounds.
    extractions.foldLeft(Nil: List[ImplicitRelation])(
      (acc, cur) => {
        val (beginIndex, endIndex) = (cur.np.beginWordIndex, cur.np.endWordIndex)
        val nersWithinExtraction =
          nerTags.foldLeft(Nil: List[NERTag])((acc, cur) => {
            val tokensWithin = cur.tokens.filter(
              pair => pair._2 >= beginIndex && pair._2 <= endIndex)
            if (tokensWithin.size == 0) {
              acc
            } else {
              val (curBegin, curEnd) = (tokensWithin(0)._2,
                tokensWithin(tokensWithin.size - 1)._2)
              new NERTag(
                ExtractionUtils.substringFromWordIndicies(line, getTokens(line), curBegin, curEnd),
                cur.ner, curBegin, curEnd, tokensWithin) :: acc
            }
          })
        cur.setNERs(nersWithinExtraction)
        cur::acc
      })
  }

  private def expectedTagEntities(confs: List[Config]): Map[String, List[String]] = {
    val map = mutable.Map[String, List[String]]()
    for (conf <- confs) {
      val tag = conf.getString("tag")
      val entityType = conf.getString("entity-type")
      map.put(tag, entityType::Nil)
    }
    map.toMap
  }
}
