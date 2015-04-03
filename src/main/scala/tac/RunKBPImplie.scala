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
    
    println("total memory: " + Runtime.getRuntime().totalMemory())
    //the Xmx value
    println("max memory: " + Runtime.getRuntime().maxMemory())
    println("free memory: " + Runtime.getRuntime().freeMemory())  		        
    println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
    println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
    
    println("Loading Tagger.")
    val tagger = TaggerLoader.defaultTagger

    println("total memory: " + Runtime.getRuntime().totalMemory())
    //the Xmx value
    println("max memory: " + Runtime.getRuntime().maxMemory())
    println("free memory: " + Runtime.getRuntime().freeMemory())  		        
    println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
    println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
        
    println("Loading Extractor.")
    val relationExtractor = new ImplicitRelationExtractor(tagger)
    println("Done Loading Extractor.")
    
    println("total memory: " + Runtime.getRuntime().totalMemory())
    //the Xmx value
  	println("max memory: " + Runtime.getRuntime().maxMemory())
  	println("free memory: " + Runtime.getRuntime().freeMemory())  		        
  	println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
    println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
    
    //System.exit(0)
    
    val queries = KBPQuery.parseKBPQueries(queriesFileName)	  

    println("Number of Queries: " + queries.size)
    //println("Query 1: " + queries(0).name)
    
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
      //2014: PER: Ahmed Rashid
      //val testQueries = queries.dropRight(99)
      //2014: PER: Alan Gross
      //val testQueries = List(queries(11))
      //val testQueries = List(queries(3))
      //2014: PER: Andrew Lange
      //val testQueries = List(queries(37))
      //2014: PER: Frank Baldino Jr
      //val testQueries = List(queries(38))
      //2014: PER: Eliza Samudio
      //val testQueries = List(queries(45))
      //2014: ORG: China Charity Federation 
      //val testQueries = List(queries(67))
      //2014: ORG: Alessi
      //val testQueries = List(queries(78))
      //2014: ORG: Pluribus Capital Mgmt
      //val testQueries = List(queries(81))
      //2014: ORG: New Fabris
      //val testQueries = List(queries(87))
      //2014: ORG: Pacific Asia Travel Association
      val testQueries = List(queries(91))
    
      //select 5 random queries
      //import scala.util.Random
      //Seq.fill(n)(Random.nextInt)
      // 2014 PER: Seq.fill(5)(Random.nextInt(50))
      //res7: Seq[Int] = List(38, 45, 11, 0, 37)
      // 2014 ORG: val y = for( r <- Seq.fill(5)(Random.nextInt(50)) ) yield { r + 50}
      // y: Seq[Int] = List(87, 67, 91, 78, 81)
      // 2013 PER: res12: Seq[Int] = List(33, 26, 49, 10, 3)
      // 2013 ORG: y: Seq[Int] = List(82, 86, 90, 79, 94)

      //val sampleDocName = "APW_ENG_20101202.0845"
      //outputStream.println("Document: " + sampleDocName)
      //val rawDoc = SolrHelper.getRawDoc(sampleDocName)      
      //outputStream.println(rawDoc)
      
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
          var allExtractions: Seq[KBPExtraction] = Nil
          var totalSentences = 0
          
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

  		      val docNames = nwDocuments.toList
              var docCount = -1
  		      
  		      println("Number of Annotated Documents: " + documents.size)  		        		      
  		      
  		      //print all extractions
		      outputStream.println
		      outputStream.println("All Extractions ----------------------------------------------------------")
              outputStream.println
		      //allExtractions.foreach(e => outputStream.println(e.getArg1().argName + "\t" + e.getArg2().argName + "\t" + e.getRel()))
  		      
  		      for(doc <- documents){  

  		        docCount += 1
                println("docName: " + docNames(docCount))
  		        outputStream.println
  		        outputStream.println("docName: " + docNames(docCount))
                outputStream.println  		        
  		        //println("doc ID: " + doc.get(classOf[DocIDAnnotation]))
  		        
  		        println("total memory: " + Runtime.getRuntime().totalMemory())
  		        //the Xmx value
  		        println("max memory: " + Runtime.getRuntime().maxMemory())
  		        println("free memory: " + Runtime.getRuntime().freeMemory())  		
  		        println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
                println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
  		        
  		        //lookAtCoref(doc)
  		        //System.exit(0) 
  		        
                //val sentences = doc.get(classOf[SentencesAnnotation]).asScala.toList
  		        
  		        val (sentences, corefMap) = getSentencesAndCorefMap(doc)  		        
  		        val matchingCorefMentions = getMatchingCorefMentions(corefMap, queryFullName)  		        
  		       	//val relevantSentences = getSentencesMatchingCoref(sentences, matchingCorefMentions)          		        
  		        //val relevantSentences = getRelevantSentences(doc, queryLastName)                

  		        val relevantSentences = getRelevantSentencesIncludingCoref(doc, queryLastName, matchingCorefMentions)    
                totalSentences += relevantSentences.size
  		        
  		        //println("sentences size: " + sentences.size)
  		       	//println("matchingCorefMentions size: " + matchingCorefMentions.size)
  		        println("relevantSentences size: " + relevantSentences.size)
  		        
  		        if(relevantSentences.size > 0){

  		          //printing null
                  //println("sentences docid: " + sentences(0).get(classOf[DocIDAnnotation]))
  		          //println("docid: " + relevantSentences(0).get(classOf[DocIDAnnotation]))
  		          
  		          //relevantSentences.foreach(s => println("rel sentence: " + s.get(classOf[TextAnnotation])))
  		          
  		          println("Getting Extractions")
  		          
                  //val extractions = getExtractions(relationExtractor, relevantSentences, docNames(docCount), outputStream)
  		          val extractions = getExtractions(relationExtractor, relevantSentences, outputStream)
                  
                  println("extractions size: " + extractions.size)

                  if(extractions.size > 0)
                    allExtractions = allExtractions ++ extractions.toSeq
                                    
                  //val filteredExtractions = filterExtractions(extractions, queryFullName)

                  println("Filtering Extractions")  
                    
                  val filteredExtractions = filterExtractionsIncludingCoref(extractions, matchingCorefMentions, queryFullName)
                  
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

		 //print all extractions
		 //outputStream.println
		 //outputStream.println("All Extractions ----------------------------------------------------------")
         //outputStream.println
		 //allExtractions.foreach(e => outputStream.println(e.getArg1().argName + "\t" + e.getArg2().argName + "\t" + e.getRel()))
		 outputStream.println
		 //print filtered extractions
		 outputStream.println("Filtered Extractions ----------------------------------------------------------")
         outputStream.println
		 allRelevantCandidates.foreach(c => outputStream.println(c.extr.getArg1().argName + "\t" + c.extr.getArg2().argName + "\t" + c.extr.getRel()))
		 outputStream.println
		 outputStream.println
		 
		 //print KBP report
		 outputStream.println("KBP Report ----------------------------------------------------------")
         outputStream.println
		 outFmt.printAnswers(bestAnswers, query) 
		  
		 println("Total Sentences: " + totalSentences)
		 
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
              case s if (s.contains("province")) => c.extr.setRel("per:statesorprovinces_of_residence")
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
     val pattern = s"$queryFullName\\W".r
     
     for(k <- corefMap.keySet){

       //if(k == 104 || k == 109)
       //{ println("key: " + k)
       //  println("mentions: " + corefMap(k).toString())}

       // get the coref mentions of type PROPER noun 
       val corefMentions = corefMap(k).getMentionsInTextualOrder().asScala.toList.filter(m => m.mentionType.name() == "PROPER")
       
       //println("size PROPER: " + x.size)
       //x.foreach(m => println(m.mentionSpan + " " + m.mentionType))
       
       //val y = x.filter(m => m.mentionSpan.contains(queryFullName))
       //println("size query full name: " + y.size)
       
       for (m <- corefMentions){
         
         pattern.findFirstIn(m.mentionSpan) match {
            case Some(x) => matchingCorefMentions = matchingCorefMentions ::: corefMentions
            case None =>             
         }
         
       }
       
       //if(corefMentions.filter(m => m.mentionSpan.contains(queryFullName)).size > 0) {matchingCorefMentions = matchingCorefMentions ::: corefMentions}
                  
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
  
    val filteredExtractions = extractions.filter(e => e.getArg1().argName.contains(queryName) || checkForCorefMatch(e, matchingCorefMentions))
    
    filteredExtractions    
  }
  
  def checkForCorefMatch(extraction: KBPExtraction, matchingCorefMentions: List[CorefMention]): Boolean = {
    
    var corefMatch = false
    var i = 0
    
    while(!corefMatch && i < matchingCorefMentions.size){
      if(extraction.getArg1().argName.contains(matchingCorefMentions(i).mentionSpan)) corefMatch = true 
      i += 1
    }           
    corefMatch
  }
  
  def getExtractions(relationExtractor: ImplicitRelationExtractor, sentences: List[CoreMap], outputStream: PrintStream): List[KBPExtraction] = {
  //def getExtractions(relationExtractor: ImplicitRelationExtractor, sentences: List[CoreMap], docName: String, outputStream: PrintStream): List[KBPExtraction] = {
    
    var extractions: List[KBPExtraction] = List()
    
    for(sentence <- sentences){
         
      //val sentenceText = sentence.get(classOf[TextAnnotation])
      val sentenceText = sentence.get(classOf[TextAnnotation]).replace("\n", " ").replace("\r", " ")
      //val sentenceOffset = sentence.get(classOf[SentStartOffset])
      //val sentenceOffset = sentence.get(classOf[SentencePositionAnnotation])
      
      //outputStream.println
      //outputStream.println(sentenceText)
      //outputStream.println
      
      //println("getting tokens")
      val tokens = sentence.get(classOf[TokensAnnotation])        
      //println("sentenceText: " + sentenceText)
      //println("tokens size: " + tokens.size())  

      val sentenceOffset = tokens.get(0).beginPosition()
      outputStream.println
      outputStream.println("sentenceOffset/Text " + sentenceOffset + ": " + sentenceText)
      outputStream.println
      
      //println("sentenceOffset: " + sentenceOffset)      

      relationExtractor.clearAllCaches()
      val implicitRelations = relationExtractor.extractRelations(sentenceText)      
      println("implicitRelations size: " + implicitRelations.size)
      
      /*if(implicitRelations.size > 0){      
        val ir = implicitRelations(0)
      
        println("ir np: " + ir.np.string)
        println("ir np start: " + ir.np.beginOffset)
        println("ir np end: " + ir.np.endOffset)
      
        println("ir tag: " + ir.tag.text)
        println("ir tag start: " + ir.tag.intervalStart)
        println("ir tag end: " + ir.tag.intervalEnd)

        println("ir rel: " + ir.relation)
      } */
        
      val sentenceExtractions: List[KBPExtraction] = for(ir <- implicitRelations) yield {
      
          //val e = new Extraction(Argument arg1, Argument arg2, String rel, double score,
			//String arg1Link, String arg2Link, String arg1BestMention,
			//String arg2BestMention, String docName, Integer sentNum,
			//Integer arg1BestMentionSentNum, Integer arg2BestMentionSentNum)

        val arg1Name = ir.np.string
        //val arg1Name = ir.np.string.split("-").dropRight(1).toString
        //val arg1StartOffset = ir.np.beginOffset
        //val arg1EndOffset = ir.np.endOffset       
        val arg1StartOffset = ir.np.beginOffset + sentenceOffset
        val arg1EndOffset = ir.np.endOffset + sentenceOffset        
        val arg1 = new Argument(arg1Name, arg1StartOffset, arg1EndOffset)

        //println("arg1StartOffset: " + arg1StartOffset)
        //println("arg1EndOffset: " + arg1EndOffset)
        
        //try{

          //includes token-id
          //tokens.asScala.toList.foreach(t => println(t.toString()))
          //no token id
          //tokens.asScala.toList.foreach(t => println("token: " + t.get(classOf[TextAnnotation])))
          //val tokenText = token.get(classOf[TextAnnotation])        
          
          //println("arg2: " + ir.tag.text)
          //println("arg2 start: " + ir.tag.intervalStart)
          //println("arg2 end: " + ir.tag.intervalEnd)
          
          var arg2StartOffset = arg1StartOffset 
          var arg2EndOffset = arg1EndOffset
          //println("docname: " + docName)
          if(ir.tag.intervalStart < tokens.size & ir.tag.intervalEnd < tokens.size){ 
            arg2StartOffset = tokens.get(ir.tag.intervalStart).beginPosition()
            arg2EndOffset = tokens.get(ir.tag.intervalEnd).beginPosition()
            //println("tokens start begpos: " + arg2StartOffset)
            //println("tokens end begpos: " + arg2EndOffset)
          }
          
          //println("arg2StartToken: " + arg2StartToken.word())
          //println("arg2EndToken: " + arg2EndToken.word())
          
          //val arg2StartOffset = arg2StartToken.beginPosition()
          //val arg2EndOffset = arg2EndToken.endPosition()
          
          //println("arg2StartOffset: " + arg2StartOffset)
          //println("arg2EndOffset: " + arg2EndOffset)
          
          val arg2Name = ir.tag.text
        
          val arg2 = new Argument(arg2Name, arg2StartOffset, arg2EndOffset)
          //val arg2 = new Argument(arg2Name, ir.tag.intervalStart, ir.tag.intervalEnd)
          
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
        if(sentenceExtractions.size > 0) {
          extractions = extractions ::: sentenceExtractions
          sentenceExtractions.foreach(e => outputStream.println(e.getArg1().argName + "\t" + e.getArg2().argName + "\t" + e.getRel()))
        }      
          
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

    /*scala> var pattern = "Alan Gross\\W".r
      pattern: scala.util.matching.Regex = Alan Gross\W

      scala> val y = "The American Alan Gross wrote the article."
      y: String = The American Alan Gross wrote the article.

      scala> pattern.findFirstIn(x)
      res8: Option[String] = None

      scala> pattern.findFirstIn(y)
      res9: Option[String] = Some(Alan Gross )

      scala> x
      res10: String = Alan Grossberg is president of Vista.
    */
    
    
    val relevantSentences = document match {

    case Some(x) =>{
        //docid null
        //println("docid: " + x.get(classOf[DocIDAnnotation]))
        val sentencesAll = x.get(classOf[SentencesAnnotation]).asScala.toList
        // Index is 0 and 1 for this group
        //println("sentence0 All: " + sentencesAll(0).get(classOf[SentenceIndexAnnotation]))
        //println("sentence1 All: " + sentencesAll(1).get(classOf[SentenceIndexAnnotation]))        
        //val sentences = x.get(classOf[SentencesAnnotation]).asScala.toList.filter(s => s.get(classOf[TextAnnotation]).contains(nameFilter))
    
        var sentenceNums = scala.collection.mutable.Set[Int]()
 
        val pattern = s"$nameFilter\\W".r
        for (s <- sentencesAll) {          
 
          pattern.findFirstIn(s.get(classOf[TextAnnotation])) match {
            case Some(x) => sentenceNums += s.get(classOf[SentenceIndexAnnotation])
            case None =>             
          }
          //if(s.get(classOf[TextAnnotation]).contains(nameFilter)) sentenceNums += s.get(classOf[SentenceIndexAnnotation])
          
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

    //docid null
    //if(relevantSentences.size > 0 )println("relsent docid: " + relevantSentences(0).get(classOf[DocIDAnnotation]))
    
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
    val maxSize = 100
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
      //docid null, null pointer exception
      val docID = processedDoc.get(classOf[DocIDAnnotation])
      println("Document was Stanford Annotated: " + docID)
      println("Document was Stanford Annotated: " + docName)
      Some(processedDoc)
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }

}



