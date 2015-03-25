package tac

//import testers.{ExtractionScoring}
import com.typesafe.config.ConfigFactory

import java.io._
import java.util.Properties
import collection.JavaConverters._

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;

import extractor.{ImplicitRelationExtractor, TaggerLoader}
//import extractor.{ImplicitRelationExtractorNoLists, TaggerLoader}
//import tac.KBPExtraction

/**
 * Run the Implie Extractor on sentences relevant to KBPQueries 
 */

object RunKBPImplie {
  
  val config = ConfigFactory.load("tac-runkbp-implie.conf")
  val resultDir = config.getString("result-dir")
  val relDocsFileName = config.getString("reldocs-file")
  val queriesFileName = config.getString("queries-file")
  val corpusName = config.getString("corpus")
  val slotfillFileName = config.getString("slotfill-file")
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()
  
  val queryCounter = new java.util.concurrent.atomic.AtomicInteger
  
  def main(args: Array[String]) {
  
    val runID = "UWashington3"
    val detailed = false 
    
    println("Loading Tagger.")
    val tagger = TaggerLoader.defaultTagger
    println("Loading Extractor.")
    val relationExtractor = new ImplicitRelationExtractor(tagger)
    println("Done Loading Extractor.")
    
    //System.exit(0)
    
    val queries = KBPQuery.parseKBPQueries(queriesFileName)	  

    println("Number of Queries: " + queries.size)
    println("Query 1: " + queries(0).name)
    
    val corpusOldNew = corpusName
    val relevantDocsFileName = relDocsFileName
    val relevantDocsFile = new File(relevantDocsFileName)  
    
    val outputStream = new PrintStream(slotfillFileName)

    val outFmt = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStream,runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStream,runID)
    }
    
    println("Ready to assign rel docs")
    println("SolrHelper: " + corpusOldNew)
    
    SolrHelper.setConfigurations(corpusOldNew, false)
    
    //System.exit(0)
      
    val entityRelevantDocSerialization = {
		    
	        if(relevantDocsFile.exists()){
	          println("reldocs file exists!")
		      QuerySetSerialization.getRevelantDocIdMap(relevantDocsFileName)
	        }	        

	        else{
		      // make this map and write it out
		      val qm = SolrHelper.getRelevantDocuments(queries)
		      val qidMap = qm.toList.map(f => (f._1.id,f._2)).toMap
		      QuerySetSerialization.writeRelevantDocIdMap(qidMap, relevantDocsFileName)
		      qidMap
		    }
      }

    println("relDocs size: " + entityRelevantDocSerialization.size)
    
    
      // ----------- Process Queries --------------- //

	  //println("Running " + queries.size + " queries.")
      //println("QID_Map: " + entityRelevantDocSerialization.keySet)
      //System.exit(0) 
      
      //test 1 query
      val testQueries = queries.dropRight(99)

      //select 5 random queries
      //import scala.util.Random
      //Seq.fill(n)(Random.nextInt)
      // 2014 PER: Seq.fill(5)(Random.nextInt(50))
      //res7: Seq[Int] = List(38, 45, 11, 0, 37)
      // 2014 ORG: val y = for( r <- Seq.fill(5)(Random.nextInt(50)) ) yield { r + 50}
      // y: Seq[Int] = List(87, 68, 91, 78, 81)
      // 2013 PER: res12: Seq[Int] = List(33, 26, 49, 10, 3)
      // 2013 ORG: y: Seq[Int] = List(82, 86, 90, 79, 94)

      println("Running " + testQueries.size + " queries.")
      
      for(query <- testQueries){
      //for(query <- queries){
	    
	      var allRelevantExtractions: Seq[KBPExtraction] = Nil		
	      //var allRelevantCandidates: Seq[Candidate] = Nil		          	      
		  val relevantDocs = entityRelevantDocSerialization(query.id).toSet		       
          val nwngDocuments = relevantDocs.filter(doc => !doc.startsWith("bolt") ) 
          val nwDocuments = nwngDocuments.filter(doc => !doc.startsWith("eng-"))		    
		  
          println("Query: " + query.id)
		  println("Size All Documents: " + relevantDocs.size)         
		  println("Size no forum Documents: " + nwngDocuments.size)  
          println("Size nw Documents: " + nwDocuments.size)  
		  
		  var allRelevantCandidates: Seq[Candidate] = Nil
  		      
  		  val queryFullName = query.name
  		  val queryLastName = queryFullName.split(" ").last
  		      
  		  println("QueryFullName: " + queryFullName)
  		  println("QueryLastName: " + queryLastName)
		  
  		  val slots = query.slotsToFill 		      
		  println("Slots: " + slots.size)
  		  
		  try{
		    
              // ---------------------------------------
  		      // Get the sentences for Implie
              // ---------------------------------------  		      

  		      val documents :List[Option[Annotation]] = { 
  		          //processDocuments(relevantDocs)  		  
  		          processDocuments(nwDocuments) 
		      }  		      
  		      
  		      println("Number of Annotated Documents: " + documents.size)  		        		      
  		      
  		      for(doc <- documents){  
  		        
  		        //lookAtCoref(doc)
  		        //System.exit(0) 
  		        
                //val sentences = doc.get(classOf[SentencesAnnotation]).asScala.toList
  		        
  		        val (sentences, corefMap) = getSentencesAndCorefMap(doc)  		        
  		        val matchingCorefMentions = getMatchingCorefMentions(corefMap, queryFullName)  		        
  		       	//val relevantSentences = getSentencesMatchingCoref(sentences, matchingCorefMentions)          		        
  		        //val relevantSentences = getRelevantSentences(doc, queryLastName)                

  		        val relevantSentences = getRelevantSentencesIncludingCoref(doc, queryLastName, matchingCorefMentions)    

  		        //println("sentences size: " + sentences.size)
  		       	//println("matchingCorefMentions size: " + matchingCorefMentions.size)
  		        println("relevantSentences size: " + relevantSentences.size)
  		        
  		        if(relevantSentences.size > 0){

  		          relevantSentences.foreach(s => println("rel sentence: " + s.get(classOf[TextAnnotation])))
  		          
  		          println("Getting Extractions")
  		          
                  val extractions = getExtractions(relationExtractor, relevantSentences)

                  println("extractions size: " + extractions.size)
                  
                  val filteredExtractions = filterExtractions(extractions, queryFullName)
                  
                  //val filteredExtractions = filterExtractionsIncludingCoref(extractions, matchingCorefMentions, queryFullName)
                 
                  println("filteredExtractions size: " + filteredExtractions.size)
                  
                  val relevantCandidates = wrapWithCandidate(filteredExtractions.toSeq)

                  if(relevantCandidates.size > 0)
  		            allRelevantCandidates = allRelevantCandidates ++ relevantCandidates  
                  
  		        }	        
  		      }
		  }
	      catch {case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION: " + query.id + " " + query.name) 
	         outFmt.printEmpty(query)
	        }	  
	      }		
	      
	     //val relevantCandidates = FilterExtractionResults.filterResults(FilterExtractionResults.wrapWithCandidate(extractions), query, document)
                
	  	 println("SubstituteKBPRelations")    	
		 val kbpAllRelevantCandidates = substituteKBPRelations(allRelevantCandidates, query)
              
	     println("Make Slot Map - Best Answers")		      
		 val bestAnswers = slots map { slot => ( slot, SelectBestAnswers.reduceToMaxResults(slot, kbpAllRelevantCandidates.filter(_.extr.getRel() == slot.name)) ) } toMap

		 println("bestAnswers size: " + bestAnswers.size)
		        
		 outFmt.printAnswers(bestAnswers, query) 
		  
	     println("Finished, going to next query")	    
	  }   	  
    
    
    println("Finished with Queries")
	  
    outputStream.close()
	  
    println("Closed outputStreams")
    
    
  }

  def substituteKBPRelations(candidates: Seq[Candidate], query: KBPQuery): Seq[Candidate] = {

    val queryEntityType = query.entityType.toString
    
    for(c <- candidates){
      
       if(queryEntityType == "PER"){
         c.extr.getRel() match {
              //relations to substitute
              case s if (s.contains("nationality")) => c.extr.setRel("per:origin")
              case s if (s.contains("city")) => c.extr.setRel("per:cities_of_residence") 
              case s if (s.contains("province")) => c.extr.setRel("per:stateorprovinces_of_residence")
              case s if (s.contains("jobTitle")) => c.extr.setRel("per:title")
              case s if (s.contains("religion")) => c.extr.setRel("per:religion")
              case s if (s.contains("school")) => c.extr.setRel("per:schools_attended")
              //The above list should cover all relations identified by Implie
              case _ => 
         }
       }
       else if(queryEntityType == "ORG"){
         
         c.extr.getRel() match {
              //relations to substitute
              case s if (s.contains("nationality")) => 
              case s if (s.contains("city")) => c.extr.setRel("org:city_of_headquarters") 
              case s if (s.contains("province")) => c.extr.setRel("org:stateorprovince_of_headquarters")
              case s if (s.contains("jobTitle")) => 
              case s if (s.contains("religion")) => c.extr.setRel("org:political_religious_affiliation")
              case s if (s.contains("school")) => 
              //The above list should cover all relations identified by Implie
              case _ => 
         }         
       }  
    }
    
    candidates
    
  }
  
  def getSentenceNumbersMatchingCoref(matchingCorefMentions: List[CorefMention]): scala.collection.mutable.Set[Int] = {

    var sentenceNumsMatchingCoref = scala.collection.mutable.Set[Int]()
    
    matchingCorefMentions.foreach(m => sentenceNumsMatchingCoref += (m.sentNum -1) )
    
    sentenceNumsMatchingCoref
  }
  
  def getSentencesMatchingCoref(sentences: List[CoreMap], matchingCorefMentions: List[CorefMention]): List[CoreMap] = {
    
    val matchingSentences = for(mcm <- matchingCorefMentions) yield { 
      sentences(mcm.sentNum - 1)      
    }
    
    matchingSentences
  }

  def getMatchingCorefMentions(corefMap: scala.collection.mutable.Map[Integer, edu.stanford.nlp.dcoref.CorefChain], 
      queryFullName: String): List[CorefMention] = {
    
     var matchingCorefMentions: List[edu.stanford.nlp.dcoref.CorefChain.CorefMention] = Nil
     
     //println("corefMap size: " + corefMap.size)
     println("queryFullName: " + queryFullName)
     
     for(k <- corefMap.keySet){

       //if(k == 104 || k == 109)
       //{ println("key: " + k)
       //  println("mentions: " + corefMap(k).toString())}

       val x = corefMap(k).getMentionsInTextualOrder().asScala.toList.filter(m => m.mentionType.name() == "PROPER")
       
       //println("size PROPER: " + x.size)
       //x.foreach(m => println(m.mentionSpan + " " + m.mentionType))
       
       //val y = x.filter(m => m.mentionSpan.contains(queryFullName))
       //println("size query full name: " + y.size)
       
       if(x.filter(m => m.mentionSpan.contains(queryFullName)).size > 0) {matchingCorefMentions = matchingCorefMentions ::: x}
                  
     }  
     
     matchingCorefMentions
  }
  
  def getSentencesAndCorefMap(document: Option[Annotation]): (List[CoreMap], scala.collection.mutable.Map[Integer, edu.stanford.nlp.dcoref.CorefChain]) = {
    
    val (sentences, corefMap) = document match {
                  case Some(x) =>{
                    val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList
                    val corefMap = x.get(classOf[CorefChainAnnotation]).asScala  
                    (sentences,corefMap)
                  }                   
                  case None => (Nil, scala.collection.mutable.Map[Integer, edu.stanford.nlp.dcoref.CorefChain]())
                  //case None => (Nil, Nil)
                }   
    
   (sentences, corefMap)  
   
  }
  
  def wrapWithCandidate(extrs: Seq[KBPExtraction]): Seq[Candidate] = {
    extrs.map { extr =>
      new Candidate(queryCounter.getAndIncrement, extr)
    }
  }
 
  def filterExtractions(extractions: List[KBPExtraction], queryName: String): List[KBPExtraction] = {
  
    val filteredExtractions = extractions.filter(e => e.getArg1().argName == queryName)
    
    filteredExtractions    
  }
  
  def filterExtractionsIncludingCoref(extractions: List[KBPExtraction], matchingCorefMentions: List[CorefMention], queryName: String): List[KBPExtraction] = {
  
    val filteredExtractions = extractions.filter(e => e.getArg1().argName == queryName || checkForCorefMatch(e, matchingCorefMentions))
    
    filteredExtractions    
  }
  
  def checkForCorefMatch(extraction: KBPExtraction, matchingCorefMentions: List[CorefMention]): Boolean = {
    
    var corefMatch = false
    val i = 0
    
    while(!corefMatch && i < matchingCorefMentions.size){
      if(extraction.getArg1().argName.contains(matchingCorefMentions(i).mentionSpan)) corefMatch = true         
    }       
    
    corefMatch
  }
  
  def getExtractions(relationExtractor: ImplicitRelationExtractor, sentences: List[CoreMap]): List[KBPExtraction] = {
    
    var extractions: List[KBPExtraction] = List()
    
    for(sentence <- sentences){
         
      val sentenceText = sentence.get(classOf[TextAnnotation])
      //val sentenceOffset = sentence.get(classOf[SentStartOffset])
      //val sentenceOffset = sentence.get(classOf[SentencePositionAnnotation])
      
      println("sentenceText: " + sentenceText)
    
      //println("getting tokens")
      val tokens = sentence.get(classOf[TokensAnnotation])        
      //println("tokens size: " + tokens.size())  

      val sentenceOffset = tokens.get(0).beginPosition()
        
      //println("sentenceOffset: " + sentenceOffset)      

      //relationExtractor.clearAllCaches()
      val implicitRelations = relationExtractor.extractRelations(sentenceText)      
      println("implicitRelations size: " + implicitRelations.size)
      
      if(implicitRelations.size > 0){      
        val ir = implicitRelations(0)
      
        println("ir np: " + ir.np.string)
        println("ir np start: " + ir.np.beginOffset)
        println("ir np end: " + ir.np.endOffset)
      
        println("ir tag: " + ir.tag.text)
        println("ir tag start: " + ir.tag.intervalStart)
        println("ir tag end: " + ir.tag.intervalEnd)

        println("ir rel: " + ir.relation)
      } 
        
      val sentenceExtractions: List[KBPExtraction] = for(ir <- implicitRelations) yield {
      
          //val e = new Extraction(Argument arg1, Argument arg2, String rel, double score,
			//String arg1Link, String arg2Link, String arg1BestMention,
			//String arg2BestMention, String docName, Integer sentNum,
			//Integer arg1BestMentionSentNum, Integer arg2BestMentionSentNum)

        val arg1Name = ir.np.string
        //val arg1Name = ir.np.string.split("-").dropRight(1).toString
        val arg1StartOffset = ir.np.beginOffset + sentenceOffset
        val arg1EndOffset = ir.np.endOffset + sentenceOffset        
        val arg1 = new Argument(arg1Name, arg1StartOffset, arg1EndOffset)

        //println("arg1StartOffset: " + arg1StartOffset)
        //println("arg1EndOffset: " + arg1EndOffset)
        
        //try{
          val arg2StartToken = tokens.get(ir.tag.intervalStart)
          val arg2EndToken = tokens.get(ir.tag.intervalEnd)
         
          //println("arg2StartToken: " + arg2StartToken.word())
          //println("arg2EndToken: " + arg2EndToken.word())
          
          val arg2StartOffset = arg2StartToken.beginPosition()
          val arg2EndOffset = arg2EndToken.endPosition()
          
          //println("arg2StartOffset: " + arg2StartOffset)
          //println("arg2EndOffset: " + arg2EndOffset)
          
          val arg2Name = ir.tag.text
        
          val arg2 = new Argument(arg2Name, arg2StartOffset, arg2EndOffset)
        
          val rel = ir.relation
          val score = .8
          val arg1Link = "";
          val arg2Link = "";
          val arg1BestMention = "";
          val arg2BestMention = "";
          val docName = sentence.get(classOf[DocIDAnnotation])
          
          //println("docName: " + docName)
          
          //val sentNum = sentence.get(classOf[SentenceIDAnnotation])
          val sentNum = sentence.get(classOf[SentenceIndexAnnotation])
          
          //println("sentNum: " + sentNum)
   
          //System.exit(0)
                    
          val arg1BestMentionSentNum = 0
          val arg2BestMentionSentNum = 0
        
          val e = new KBPExtraction(arg1, arg2, rel, score,
			arg1Link, arg2Link, arg1BestMention,
			arg2BestMention, docName, sentNum,
			arg1BestMentionSentNum, arg2BestMentionSentNum)
          e
        //}
        //catch{case e: Exception => {      
        //}}
        }        
        if(sentenceExtractions.size > 0) extractions = extractions ::: sentenceExtractions      
    }             
    extractions
  }
  
  def lookAtCoref(document: Option[Annotation]): Unit = {
    
    val relevantSentences = document match {
      case Some(x) =>{
        
        val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList
        
        //val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList.filter(s => s.get(classOf[TextAnnotation]).contains(nameFilter))
        //Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
        val corefMap = x.get(classOf[CorefChainAnnotation]).asScala        
        
        println("num sentences: " + sentences.size)
        val sent0 = sentences(0)
        
        val s1 = sent0.get(classOf[SentenceIndexAnnotation])
        val s2 = sent0.get(classOf[SentenceIDAnnotation])
        val s3 = sent0.get(classOf[SentencePositionAnnotation])
        
        //sentence.get(SentGlobalID.class)
        println("sent0 sentnum index integer: " + sent0.get(classOf[SentenceIndexAnnotation]))
        println("sent0 sentnum ID string: " + sent0.get(classOf[SentenceIDAnnotation]))
        println("sent0 sentnum position string: " + sent0.get(classOf[SentencePositionAnnotation]))

        
        for(k <- corefMap.keySet){

          println("coref key: " + k)
          val corefMentions = corefMap(k).getMentionsInTextualOrder().asScala

          for(m <- corefMentions){            
            //println(m.toString())
            println("mention id: " + m.mentionID)
            println("mention span: " + m.mentionSpan)
            println("mention span length: " + m.mentionSpan.length())
            println("mention type: " + m.mentionType)
            println("mention type name(): " + m.mentionType.name())
            println("mention sentnum: " + m.sentNum)
            println("mention start index: " + m.startIndex)            
          }
          
        }         
                
        //for (Integer key: annotation.get(CorefChainAnnotation.class) .keySet()) {
	    //  for (CorefMention mention: annotation.get(CorefChainAnnotation.class).get(key).getMentionsInTextualOrder()) {
		//		System.out.println(mention.mentionSpan);
		//	}
		//}
        
      }                   
      case None => List()
    }      
  }
   
  
  def getRelevantSentencesIncludingCoref(document: Option[Annotation], nameFilter: String, matchingCorefMentions: List[CorefMention]): List[CoreMap] = {
    
    val relevantSentences = document match {

    case Some(x) =>{
        val sentencesAll = x.get(classOf[SentencesAnnotation]).asScala.toList
        // Index is 0 and 1 for this group
        //println("sentence0 All: " + sentencesAll(0).get(classOf[SentenceIndexAnnotation]))
        //println("sentence1 All: " + sentencesAll(1).get(classOf[SentenceIndexAnnotation]))        
        //val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList.filter(s => s.get(classOf[TextAnnotation]).contains(nameFilter))
        
        var sentenceNums = scala.collection.mutable.Set[Int]()
        for (s <- sentencesAll) {          
          if(s.get(classOf[TextAnnotation]).contains(nameFilter)) sentenceNums += s.get(classOf[SentenceIndexAnnotation])
        }
        
        val sentenceNumsCoref = getSentenceNumbersMatchingCoref(matchingCorefMentions)       

        sentenceNums ++= sentenceNumsCoref
                
        // id's: 15 and 18, for 0 and 1
        //println("sentence0: " + sentences(0).get(classOf[SentenceIndexAnnotation]))
        //println("sentence1: " + sentences(1).get(classOf[SentenceIndexAnnotation]))
        
        sentencesAll.filter(s => sentenceNums.contains(s.get(classOf[SentenceIndexAnnotation])))
        
      }                   
      case None => List()
    }        
    
    relevantSentences
  }
  
  def getRelevantSentences(document: Option[Annotation], nameFilter: String): List[CoreMap] = {
    
    val relevantSentences = document match {
      case Some(x) =>{
        val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList.filter(s => s.get(classOf[TextAnnotation]).contains(nameFilter))  
        sentences        
      }                   
      case None => List()
    }        
    
    relevantSentences
  }
  
  /*def getRelevantSentences(document: Option[Annotation], nameFilter: String): List[String] = {
    
    val relevantSentences = document match {
      case Some(x) =>{
        val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList
        val sentencesText = for(sentence <- sentences) yield {
          sentence.get(classOf[TextAnnotation])
        } 
        val relevantSentences = sentencesText.filter(_.contains(nameFilter))    
        relevantSentences
      }                   
      case None => List()
    }    
    
    relevantSentences
  }*/
  
  def processDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var docs = documents.toList
    // ---------------------------------------------------------------------
    // Temporary: setting max number of documents to 100
    val maxSize = 2
    if(documents.size > maxSize){docs = docs.dropRight(docs.size-maxSize)}
    var docCount = 0
    for(doc <- docs) yield{
    // ---------------------------------------------------------------------  
    //for(doc <- documents.toList) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount)
        var a :Option[Annotation] = None
        a = stanfordProcessDocument(doc)
        a       
    }  
  }
  
  /*def processDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var startTime :Long = 0
	var endTime: Long = 0    	 
	var docCount = 0
    for(doc <- documents.toList) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount)
      var a :Option[Annotation] = None
      val t = new Thread {
        override def run() {    
          startTime = System.currentTimeMillis()
          //a =processDocument(doc)
          a = stanfordProcessDocument(doc)
          endTime = System.currentTimeMillis()
          println("Thread: Document took " + (endTime-startTime) + " milliseconds")      
        }
      }                                              
      t.start()
      //t.join(10000)
      t.join(180000)                                        
      a
    }
  } */
  
 
  def stanfordProcessDocument(docName: String) : Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      val processedDoc = new Annotation(rawDoc)
      //annotatorHelper.getBasicPipeline().annotate(processedDoc)
      annotatorHelper.getCorefPipeline().annotate(processedDoc)
      println("Document was Stanford Annotated")
      Some(processedDoc)
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }

}




