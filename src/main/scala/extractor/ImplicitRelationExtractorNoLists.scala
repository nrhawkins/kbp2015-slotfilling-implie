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
import edu.stanford.nlp.ie.crf.CRFClassifier
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
class ImplicitRelationExtractorNoLists(
    tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized],
    serializedTokenCacheFile: String = null,
    serializedParseCacheFile: String = null,
    extractorConfig: String = "extractor.conf")
    extends ImplicitRelationExtractor(
    tagger, serializedTokenCacheFile, serializedParseCacheFile, extractorConfig)
    with NERFilterByTagNER {

  val nerConfig = ConfigFactory.load("filter-by-tag-ner-types.conf")
  protected val expectedEntities =
    expectedTagEntities(nerConfig.getConfigList("not-expected-tag-types").toList)
  protected val NER_MODEL = nerConfig.getString("ner-model-file")
  protected val classifier: CRFClassifier[CoreLabel] = CRFClassifier.getClassifier(NER_MODEL)
  protected val NER_TAGS_TO_IGNORE = nerConfig.getStringList("ner-tag-ignore").toList

  /**
   * Extracts implicit relations from a string.
   * @param line String, line of text to extract.
   * @return List[ImplicitRelation], list of relations extracted from the string.
   */
  override def extractRelations(line: String): List[ImplicitRelation] = {
    val implicitRelations = super.extractRelations(line)
    //val taggedNERs = tagNERs(implicitRelations, line)
    
    /*taggedNERs.foreach(extraction => {
      val ners = extraction.getNERs
      val tag = extraction.relation
      println("irenl: ner.size " + ners.size)
      ners.foreach(ner => println(ner.ner))
      val nersRegion = ners.filter(ner => ner.beginIndex <= extraction.tag.index-1 && 
          ner.endIndex >= extraction.tag.index-1)
      println("irenl: nersRegion size " + nersRegion.size)
    }) */
    
    //Filter the implicitRelations, exclude ones which have lists in the entity
    //println(s"initial extractions $implicitRelations")

    //filterNoLists(implicitRelations)
    val relationsNoLists = filterNoLists(implicitRelations)

    //println(s"after filtering out lists $relationsNoLists")

    //filterNERs(implicitRelations)
    
    filterNERsByTag(line, relationsNoLists, NER_TAGS_TO_IGNORE, expectedEntities)
    
  }

  /**
   * Extracts implicit relations from a string without filtering by head.
   * @param line String, line of text to extract.
   * @return List[ImplicitRelation], list of relations extracted from the string.
   */
  def headUnfilteredExtractions(line: String): List[ImplicitRelation] = {
    val relations = super.unfilteredExtractions(line)
    val relationsNoLists = filterNoLists(relations)
    filterNERsByTag(line, relationsNoLists, NER_TAGS_TO_IGNORE, expectedEntities)
  }

  // -----------------------------------------------------------------------
  // Filter out implicitRelations which have a list in the entity 
  //
  // Filter out relation if any one of these is true: 
  // 1) 1 or more conj_and's 
  // 2) 2 or more appos's
  // 3) 1 or more conj_and's and 1 or more appos's
  // -----------------------------------------------------------------------
  def filterNoLists(relations: List[ImplicitRelation]): List[ImplicitRelation] = {

    val relationsFiltered = for(rel <- relations) yield{      

      rel.explicitRelationTraces.foreach(ert => {
        
         var countConjAnd = 0
         var countAppos = 0  
        ert.foreach(t => {  
          if(t.toString.contains("conj_and")) countConjAnd += 1 
          if(t.toString.contains("appos")) countAppos += 1          
        })
       if(countConjAnd >= 1 || countAppos >= 2 || countConjAnd + countAppos >=2 ){
         rel.relation = "dropThisRelation"
       }
         
      }) 

      rel
    
    }
    
    relationsFiltered.filter(rel => rel.relation != "dropThisRelation")   
    
  }
    
  
}


