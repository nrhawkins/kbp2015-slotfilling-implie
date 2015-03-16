package tac



//import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.Candidate
//import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.StanfordAnnotatorHelperMethods
//import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.Slot
import edu.stanford.nlp.dcoref.CorefChain.CorefMention
import edu.knowitall.collection.immutable.Interval
//import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.SolrHelper
import scala.collection.JavaConverters._
import scala.util.matching.Regex
//import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
//import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.Extraction
//import edu.knowitall.tac2013.openie.KbpExtractionUtils
import java.io.File


object DocUtils {
  
  lazy val stanfordHelper = new StanfordAnnotatorHelperMethods()
  
  val yearPattern = new Regex("\\d\\d\\d\\d")
  
/*  def putInTimexFormat(slotCandidates: Map[Slot,Seq[Candidate]], oldOrNew: String): Unit = {
    
    for(slot <- slotCandidates.keys){
      if(slot.isDate){
        //get timex format from SUTime
        val candidates = slotCandidates(slot)
        for(candidate <- candidates){
          val timexFormattedFill = stanfordHelper.getNormalizedDate(candidate.fillOffsetInterval, candidate.extr.sentence.docId ,candidate.trimmedFill.string)
          candidate.trimmedFill.setString(timexFormattedFill)
          
          //timexFormattedFill is in TimexFormat and the year specified does not appear in the
          //candidate slot fill then we can attribute it to Stanford's SUTime relative temporal
          //reasoning and we should get the byte offsets of the date entry in the document
          
          val properTimexPattern = new Regex("\\w\\w\\w\\w\\-\\w\\w\\-\\w\\w")
          if(properTimexPattern.findPrefixOf(timexFormattedFill).isDefined){
            val year = timexFormattedFill.slice(0,4)
            //if the candidate trimmed Fill does not contain the year then it was most likely
            //reasoned by SUTime
            if(!candidate.fillField.originalText.contains(year)){
                val supportingByteOffsets = getDateByteOffsets(year,candidate.extr.sentence.docId,oldOrNew)
                if(supportingByteOffsets.isDefined){
                  //increment start by 3 because of preposition on..
                  try{
                   candidate.trimmedFill.setSupportingByteOffsets(Interval.closed(supportingByteOffsets.get.start+3,supportingByteOffsets.get.end))
                  }
                  catch{
                    case e : Exception => {}
                  }
                }
              }
            }
        }
      }
    }
  }
 */
  
  /*def getCorefMentions(docId: String, interval: Interval): Option[List[CorefMention]] = {
    val rawDoc = SolrHelper.getRawDoc(docId)
    val mentions = stanfordHelper.getCorefMentions(rawDoc, interval)
    if(mentions.isEmpty()){
      return None
    }
    else {
     return Some(mentions.asScala.toList)
    }
  }
  */
  
  def main(args: Array[String]){
    
    SolrHelper.setConfigurations("old",false)
    
    if(args.length != 3){
      throw new Exception("There must be three args, arg 1 is docId from the old corpus, arg2 is beginning offset of string and arg3 is ending offset of string")
    }
    
    /*val mentions = getCorefMentions(args(0),Interval.closed(args(1).toInt,args(2).toInt))
    if(mentions.isDefined){
      for(m <- mentions.get){
        println(m.mentionSpan)
      }
    }*/
    
  }
  
  def findBestFillMention(slotCandidates: Map[Slot,Seq[Candidate]]){
    
    for(slot <-slotCandidates.keys){
      for(candidate <- slotCandidates(slot)){
        
      }
    }
    
    
  }
  
  def getByteOffSetsOfFirstOccurenceOfString(docId:String, str: String): Option[Interval] = {
    var xmlDoc = SolrHelper.getRawDoc(docId)
    val indexOfSlice = xmlDoc.indexOfSlice(str)
    if(indexOfSlice == -1){
      None
    }
    else{
      return Some(Interval.closed(indexOfSlice,indexOfSlice + (str.length()-1)))
    }
  }
  
  /*def getDateByteOffsets(year: String, docId: String, oldOrNew: String): Option[Interval] = {
    val solrClient = SolrQueryExecutor.getInstance(oldOrNew).solrClient
    val results = solrClient.query("+docId:" + "\"" + docId + "\"" + " +relText:" + "\"written\"" + " +arg2Text:" +"\"on\""
        +" +arg1Text:\"post\"").getResultAsMap()
    val kbpExtrs = results.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap)
    }
    
    
    val sent0Results = kbpExtrs.filter(p => (p.sentence.sentNum < 2))
    for( r <- sent0Results){
      if(yearPattern.findFirstIn(r.arg2.originalText).isDefined){
        return Some(KbpExtractionUtils.getOffset(r.arg2,r.sentence))
      }
    }
    return None
    
  }
  */
  
  def docLength(docId: String): Int = {
    val rawDoc = SolrHelper.getRawDoc(docId)
    rawDoc.size
  }
  
  /**
   * If file is not a directory, return it
   * else, return all files (that are not directories) found recursively in this directory.
   */
  def getFilesRecursive(file: File): Iterator[File] = {
    if (!file.isDirectory()) Iterator(file)
    else (file.listFiles.iterator.flatMap { f => getFilesRecursive(f) })
  }
  

}