package tac

import com.typesafe.config.ConfigFactory

import java.io._
import java.util.logging.Level
import java.util.Properties
import collection.JavaConverters._
import java.nio.file.{Paths, Files}
import scala.io.Source

import java.util.logging.LogManager

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;

import KBPQueryEntityType._

import extractor.{ImplicitRelationExtractor,ImplicitRelationExtractorNoLists,ModHighRecallImplIE,TaggerLoader}

/**
 * Run the Implie Extractor on cold start corpus for KBP2015
 */

object RunKBP2015ImplieGroupQueries {
  
  val config = ConfigFactory.load("tac-runkbp2015-implie-groupQueries.conf")
  val reportDir = config.getString("report-dir")
  val relDocsFileName = config.getString("reldocs-file")
  val queriesFileName = config.getString("queries-file")
  val topJobTitlesFileName = config.getString("top-job-titles-file")
  val corpusName = config.getString("corpus")
  val slotfillFileName = config.getString("slotfill-file")
  val extractionsFileName = config.getString("extractions-file")
  val numRelDocsFileName = config.getString("num-reldocs-file")
  val roundID = config.getString("roundID")
  val round1QueriesFile = config.getString("round1-queries-file")
  val round2QueriesFile = config.getString("round2-queries-file")
  val batchDrop = config.getString("batch-drop").toInt
  val batchDropRight = config.getString("batch-drop-right").toInt
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()
  val queryCounter = new java.util.concurrent.atomic.AtomicInteger

  
  def main(args: Array[String]) {
  
    val runID = "UWashington3"  
    //val detailed = true
    val detailed = false
    
    val corpusOldNew = corpusName
    val relevantDocsFileName = relDocsFileName
    val relevantDocsFile = new File(relevantDocsFileName)  
    
    // ---------------------------------------------------------------------
    // Initialize Output Streams
    // --------------------------------------------------------------------- 
    
    val outputStream = new PrintStream(slotfillFileName)
    val outputStreamExtractions = new PrintStream(extractionsFileName)
    val outputStreamRelDocs = new PrintStream(numRelDocsFileName)
    
    val outFmt = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStream,runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStream,runID)
    }

    val outFmtExtractions = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStreamExtractions,runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStreamExtractions,runID)
    }
    
    // ---------------------------------------------------------------------
    // Load Implie Tagger and RelationExtractor
    // --------------------------------------------------------------------- 
    
    println("Loading Tagger.")
    val tagger = TaggerLoader.extendedKeywordHighRecallTagger     
    println("Loading Extractor.")
    val relationExtractor = new ModHighRecallImplIE(tagger)
    println("Done Loading Extractor.")
    
    // ---------------------------------------------------------------------
    // Load Implie TopJobs File and CountryMapper
    // --------------------------------------------------------------------- 

    val topJobs = {
     
      val inputFilename = topJobTitlesFileName
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"TopJobs file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      val lines = Source.fromFile(inputFilename).getLines().toSet
      val topJobs = for(l <- lines) yield { l.toLowerCase }      
      topJobs.filter(j => j.size > 0)      
    }       
    
    val mapper = new SingularCountryMapper()
        
    // ---------------------------------------------------------------------
    // Select the Solr Index
    // --------------------------------------------------------------------- 
    
    SolrHelper.setConfigurations(corpusOldNew, false)
   
    // ---------------------------------------------------------------------
    // Parse queries
    // ---------------------------------------------------------------------
    
    val queriesNoAliases = KBPQuery.parseKBPQueries(queriesFileName, roundID)	  
    val queries = KBPQuery.getAliases(queriesNoAliases)
    
    println("Number of Queries: " + queries.size)
        
    // ----------------------------------------------------------------------------
    // Single Name Resolver
    // ----------------------------------------------------------------------------
   
    try{
	  
      var queryNameSetRound2 = Set[String]()
      var queryNameSetRound1 = Set[String]()

      queryNameSetRound1 = KBPQuery.parseKBPQueriesToGetNames(round1QueriesFile)
      queryNameSetRound2 = KBPQuery.parseKBPQueriesToGetNames(round2QueriesFile)           
        
      //println("SNR: r1 size: " + queryNameSetRound1.size)
      //println("SNR: r2 size: " + queryNameSetRound2.size)
      
      // For PER queries which have a single name, replace that name with a full name,
      // if one can be determined
      for(query <- queries){	    

        var singleQueryNamePER = false
          
	    singleQueryNamePER = query.entityType match{
          case PER if(query.name.split(" ").size == 1) => {
            val (single, qname) = SingleNameResolver.singleQueryName(query, queryNameSetRound2, queryNameSetRound1)
            if(!single) query.name = qname 
            single
          }
	      case _ => false
	    }          
      }
	    
    }catch {case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION: SingleNameResolver") 
	        }	  
    }  

    // ----------------------------------------------------------------------------	
    // Get Relevant Docs
    // ----------------------------------------------------------------------------
    
    val entityRelevantDocSerialization = {
		    
	        if(relevantDocsFile.exists()){
	          println("reldocs file exists!")
		      QuerySetSerialization.getRevelantDocIdMap(relevantDocsFileName)
	        }	        

	        else{
	          println("creating reldocs file!")
		      // make this map and write it out
	          val qm = SolrHelper.getRelevantDocuments(queries)
	          //use fcn below if working with a list of docs which is a subset of a solr index
		      //val qm = SolrHelper.getRelevantDocumentsColdStart(queries)
              //val qm = SolrHelper.getRelevantDocuments(testQueries)
	          val qidMap = qm.toList.map(f => (f._1.id,f._2)).toMap
		      QuerySetSerialization.writeRelevantDocIdMap(qidMap, relevantDocsFileName)
		      qidMap
		    }
    } 
    
    // --------------------------------------------------------------------------------------	
    // Group the Queries - create a list of List[KBPQuery], 
	// each element of the list is a list of KBPQueries which have the same query name,
	// so that the doc processing can be shared
	// --------------------------------------------------------------------------------------
	  
    var querySet: Set[String] = Set()
    var sameQueries : List[List[KBPQuery]] = List()
    
    for(query <- queries){
      val qn = query.name
      
      if(!querySet.contains(qn)){
        //add query name to the set, which indicates it has been processed
        querySet += qn   
        //get all matching queries for this query name
        val matchingQueries = queries.filter(q => qn.equals(q.name))
        sameQueries = sameQueries ++ List(matchingQueries)        
      } 
    }	  
	
    println("sameQueries size: " + sameQueries.size)	  

    //--------------------------------------------------------------------------
    // If we want to run a subset of the collapsed queries, this is that subset
    //--------------------------------------------------------------------------
    val sameQueriesBatch = sameQueries.drop(batchDrop).dropRight(batchDropRight)
      
    println("sameQueriesBatch size: " + sameQueriesBatch.size)	  
    
    // ----------------------------------------------------------------
    // Examine memory use
    // ----------------------------------------------------------------
    
    println("total memory: " + Runtime.getRuntime().totalMemory())
    //the Xmx value
    println("max memory: " + Runtime.getRuntime().maxMemory())
    println("free memory: " + Runtime.getRuntime().freeMemory())  		        
    println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
    println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
    

    var queryCount = 0
      
    for(sameQueryName <- sameQueriesBatch){        

      queryCount += 1
        
      val firstQuery = sameQueryName(0) 
      val queryName = firstQuery.name
      println
      println("query " + queryCount + ": " + firstQuery.id + " " + queryName)
      println
        
      // -------------------------------------------------------------------
      // singleQueryNamePER -- If query entityType is PER and query name is 
      // a single name (i.e. couldn't be resolved to a full name above),
      // set flag here to true, to use to drop it
      // -------------------------------------------------------------------
        
	  var singleQueryNamePER = false
     
      singleQueryNamePER = firstQuery.entityType match{
          case PER if(queryName.split(" ").size == 1) => true
	      case _ => false
	  }
      
      // -----------------------------------------------------------------------
      // anyRelevantSlots -- If the query set has no slots being filled by
      // this PERLOC Multir model, set flag here to false, to use to drop it
      // -----------------------------------------------------------------------
                
      var anyRelevantSlots = false
        
      val sameQueriesRelevantSlot = sameQueryName.filter(q => ColdStartSlots_ImplIE.slots.contains(q.slotsToFill.toList(0).name))
        
      println("sameQueriesRelevantSlot size: " + sameQueriesRelevantSlot.size)        
              
      if(sameQueriesRelevantSlot.size > 0) anyRelevantSlots = true  
                
      // ------------------------------------------------------------------------------------------------
      // Proceed if this set of queries does not have a single PER name, and has relevant slots to fill
      // ------------------------------------------------------------------------------------------------
      if(!singleQueryNamePER && anyRelevantSlots){
          
	    try{

	      // --------------------------------------------------------------------------
	      // Set of Relevant Extraction Candidates from the set of relevant documents        
	      // --------------------------------------------------------------------------
	      var allRelevantCandidates: Seq[Candidate] = Nil
          var allExtractions: Seq[KBPExtraction] = Nil
          var totalSentences = 0
	      
	      
	      // --------------------------------------
          // Process Documents for this Query Set  
          // --------------------------------------    
		        		              	      
		  val relevantDocs = entityRelevantDocSerialization(firstQuery.id).toSet		      
		  
		  val documents :List[Option[Annotation]] = { 
  		          processDocuments(relevantDocs)  		  
  		          //processDocuments(nwDocuments) 
		  }  
	      
		  println("Getting Extractions")  
		  
		  for(doc <- documents){
		    
		    val (sentences, corefMap) = getSentencesAndCorefMap(doc)  		        
  		    val matchingCorefMentions = getMatchingCorefMentions(corefMap, firstQuery)  	
  		         
  		    val relevantSentencesCoref = getRelevantSentencesIncludingCoref(doc, firstQuery, matchingCorefMentions)    
  		    val relevantSentences = relevantSentencesCoref.filter(s => s.get(classOf[TextAnnotation]).size < 400 )  
  		        
            totalSentences += relevantSentences.size
  		        
  		    //println("relevantSentences size: " + relevantSentences.size)
  		        
  		    if(relevantSentences.size > 0){
  		          
  		      //println("Getting Extractions")
  		          
  		      val extractions = getExtractions(relationExtractor, relevantSentences, outputStreamExtractions)
                  
              //println("extractions size: " + extractions.size)

              if(extractions.size > 0)
                allExtractions = allExtractions ++ extractions.toSeq

              //println("Filtering Extractions")  
                    
              var filteredExtractions = filterExtractionsIncludingCoref(extractions, matchingCorefMentions, firstQuery)

              //For GPE relations, only keep extractions where the location name matches the query name
              filteredExtractions = firstQuery.entityType match {
                case GPE => filteredExtractions.filter(e => e.getArg2().argName.trim.equalsIgnoreCase(firstQuery.name))
                case _ => filteredExtractions
              }       
                  
              //println("filteredExtractions size: " + filteredExtractions.size)
                  
              val relevantCandidates = wrapWithCandidate(filteredExtractions.toSeq)

              if(relevantCandidates.size > 0)
  		        allRelevantCandidates = allRelevantCandidates ++ relevantCandidates  
                  
  		      }	          
		  }
		  
		  
    
	    }
	    catch {case e: Exception => 
	      {e.printStackTrace()
	         println("EXCEPTION: " + firstQuery.id + " " + firstQuery.name) 
	         //outFmt.printEmpty(query)
	      }	  
	    }		
      }
      else{
	    println("Skipping this query: anyRelevantSlots= " + anyRelevantSlots + " singleQueryNamePER: " + singleQueryNamePER)
	    //Don't need to print NIL for Cold Start
	    //outFmt.printEmpty(query)
	  }
	     
	  println("Finished, going to next query")
      println
      
    }
		      
    /*
    for(query <- queries){

      println("Query Name: " + query.id)
     
      var singleQueryNamePER = false
      
      // -------------------------------------------------------------------------------
      // Cold Start Restrictions - i.e. only run if...
      // -------------------------------------------------------------------------------
      if(query.slotsToFill.size==1 && ColdStartSlots_ImplIE.slots.contains(query.slotsToFill.toList(0).name) && !singleQueryNamePER){
      
          //println("Slot to Fill: " + query.slotsToFill.toList(0).name)
      
	      var allRelevantExtractions: Seq[KBPExtraction] = Nil		
	       		  
		  try{
		    
              // ---------------------------------------
  		      // Get the sentences for Implie
              // ---------------------------------------  		      

  		      val documents :List[Option[Annotation]] = { 
  		          processDocuments(relevantDocs)  		  
  		          //processDocuments(nwDocuments) 
		      }  		      

  		      //val docNames = nwDocuments.toList
              //var docCount = -1
  		      
  		      //println("Number of Annotated Documents: " + documents.size)  		        		      
  		      
  		      //print all extractions
		      //outputStreamExtractions.println
		      //outputStreamExtractions.println("All Extractions ----------------------------------------------------------")
              //outputStreamExtractions.println
		      //allExtractions.foreach(e => outputStream.println(e.getArg1().argName + "\t" + e.getArg2().argName + "\t" + e.getRel()))
  		      
  		      for(doc <- documents){  

  		        /*doc match {
  		          case Some(x) => {
  		            outputStreamExtractions.println
  		            outputStreamExtractions.println("docName: " + x.get(classOf[DocIDAnnotation]) )
                    outputStreamExtractions.println
                  } 
  		          case _ => 
  		        }*/
  		        
  		        val (sentences, corefMap) = getSentencesAndCorefMap(doc)  		        
  		        val matchingCorefMentions = getMatchingCorefMentions(corefMap, query)  	
  		         
  		        val relevantSentencesCoref = getRelevantSentencesIncludingCoref(doc, query, matchingCorefMentions)    
  		        val relevantSentences = relevantSentencesCoref.filter(s => s.get(classOf[TextAnnotation]).size < 400 )  
  		        
                totalSentences += relevantSentences.size
  		        
  		        //println("relevantSentences size: " + relevantSentences.size)
  		        
  		        if(relevantSentences.size > 0){
  		          
  		          //println("Getting Extractions")
  		          
  		          val extractions = getExtractions(relationExtractor, relevantSentences, outputStreamExtractions)
                  
                  //println("extractions size: " + extractions.size)

                  if(extractions.size > 0)
                    allExtractions = allExtractions ++ extractions.toSeq

                  //println("Filtering Extractions")  
                    
                  var filteredExtractions = filterExtractionsIncludingCoref(extractions, matchingCorefMentions, query)

                  //For GPE relations, only keep extractions where the location name matches the query name
                  filteredExtractions = query.entityType match {
                    case GPE => filteredExtractions.filter(e => e.getArg2().argName.trim.equalsIgnoreCase(query.name))
                    case _ => filteredExtractions
                  }       
                  
                  //println("filteredExtractions size: " + filteredExtractions.size)
                  
                  val relevantCandidates = wrapWithCandidate(filteredExtractions.toSeq)

                  if(relevantCandidates.size > 0)
  		            allRelevantCandidates = allRelevantCandidates ++ relevantCandidates  
                  
  		        }	        
  		      }
		  }
	      catch {case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION: " + query.id + " " + query.name) 
	         //outFmt.printEmpty(query)
	         //outFmtExtractions.printEmpty(query)
	        }	  
	      }	
                
	  	 //println("SubstituteKBPRelations")    	
		 val kbpAllRelevantCandidates = substituteKBPRelations(allRelevantCandidates, query, topJobs, mapper)
		               
	     //println("Make Slot Map - Best Answers")		      
		 val bestAnswers = slots map { slot => ( slot, SelectBestAnswers.reduceToMaxResults(slot, kbpAllRelevantCandidates.filter(_.extr.getRel() == slot.name)) ) } toMap

		 //println("bestAnswers size: " + bestAnswers.size)
		 
		 //outputStreamExtractions.println
		 //print filtered extractions
		 //outputStreamExtractions.println("Filtered Extractions ----------------------------------------------------------")
         //outputStreamExtractions.println
		 //allRelevantCandidates.foreach(c => outputStreamExtractions.println(c.extr.getArg1().argName + "\t" + c.extr.getArg2().argName + "\t" + c.extr.getRel()))
		 //outputStreamExtractions.println
		 //outputStreamExtractions.println
		 
		 //print Query Name
		 //outputStreamExtractions.println
		 //outputStreamExtractions.println("Query Name: " + query.name )
		 //outputStreamExtractions.println
		 
		 //print KBP report
		 //outputStreamExtractions.println("KBP Report ----------------------------------------------------------")
         //outputStreamExtractions.println
		 outFmt.printAnswers(bestAnswers, query) 
		 //outFmtExtractions.printAnswers(bestAnswers, query)  
		 //println("Total Sentences: " + totalSentences)		    
	     
      }
      else{
        //outFmt.printEmpty(query)
	    //outFmtExtractions.printEmpty(query)
      }
      
      println("Finished, going to next query")	    
	     
	  }   	  
    
    
    println("Finished with Queries")
	  
    outputStream.close()
    outputStreamExtractions.close()
    outputStreamRelDocs.close()
    
    println("Closed outputStreams")  
    
    println("total memory: " + Runtime.getRuntime().totalMemory())
    //the Xmx value
    println("max memory: " + Runtime.getRuntime().maxMemory())
    println("free memory: " + Runtime.getRuntime().freeMemory())  		        
    println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
    println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
    
  } */

  
  def substituteKBPRelations(candidates: Seq[Candidate], query: KBPQuery, topJobs: Set[String], mapper: SingularCountryMapper): Seq[Candidate] = {

    val queryEntityType = query.entityType.toString    
    var newCandidates: Seq[Candidate] = Nil
    
    for(c <- candidates){
      
       if(queryEntityType == "PER"){
         c.extr.getRel() match {
              //relations to substitute
              case s if (s.contains("nationality")) => { 
                c.extr.setRel("per:origin")
                val newExtraction = new KBPExtraction(c.extr.getArg1(), c.extr.getArg2(), c.extr.getRel(), 
                    c.extr.getScore(), c.extr.getArg1Link(), c.extr.getArg1BestMention(), c.extr.getArg2Link(), c.extr.getArg2BestMention(),
			        c.extr.getDocName(), c.extr.getSentNum(), c.extr.getArg1BestMentionSentNum(), c.extr.getArg2BestMentionSentNum(), c.extr.getNers(),
			        c.extr.getSentence())
                val newCandidate = new Candidate(queryCounter.getAndIncrement, newExtraction)
                newCandidate.extr.setRel("per:countries_of_residence")
                val country = mapper.getCountryName(c.extr.getArg2().getArgName(), lowercase=true)
                country match {
                  case null => 
                  case _ => {val newArg2 = new Argument(country, c.extr.getArg2().getStartOffset(), c.extr.getArg2().getEndOffset())
                             newCandidate.extr.setArg2(newArg2)}   
                }                                           
                newCandidates = newCandidates ++ Seq(newCandidate)
              }
              case s if (s.contains("city")) => c.extr.setRel("per:cities_of_residence") 
              case s if (s.contains("province")) => c.extr.setRel("per:statesorprovinces_of_residence")
              case s if (s.contains("jobTitle")) => c.extr.setRel("per:title")
              case s if (s.contains("religion")) => c.extr.setRel("per:religion")
              case s if (s.contains("school")) => //c.extr.setRel("per:schools_attended")
              //The above list should cover all relations identified by Implie
              case _ => 
         }
       }
       else if(queryEntityType == "ORG"){
         
         c.extr.getRel() match {
              //relations to substitute
              case s if (s.contains("nationality")) => {
                c.extr.setRel("org:country_of_headquarters")
                val country = mapper.getCountryName(c.extr.getArg2().getArgName(), lowercase=true)
                country match {
                  case null => 
                  case _ => {val newArg2 = new Argument(country, c.extr.getArg2().getStartOffset(), c.extr.getArg2().getEndOffset())
                             c.extr.setArg2(newArg2)}   
                }                 
                
                } 
              case s if (s.contains("city")) => c.extr.setRel("org:city_of_headquarters") 
              case s if (s.contains("province")) => c.extr.setRel("org:stateorprovince_of_headquarters")
              case s if (s.contains("jobTitle")) => {
                val persons = c.extr.getNers().asScala.toList.filter(n => n.ner == "PERSON")
                if(c.extr.getArg1().argName.contains(query.name) && topJobs.contains(c.extr.getArg2().argName.toLowerCase()) && persons.size > 0 ) {                 
                  c.extr.setArg2(extractPerson(c))
                  c.extr.setRel("org:top_members_employees")                  
                }                
              }
              case s if (s.contains("religion")) => c.extr.setRel("org:political_religious_affiliation")
              case s if (s.contains("school")) => 
              //The above list should cover all relations identified by Implie
              case _ => 
         }         
       }  
       
       else if(queryEntityType == "GPE"){  
       
         c.extr.getRel() match {  

           // Country
           case s if (s.contains("nationality")) => {
             val persons = c.extr.getNers().asScala.toList.filter(n => n.ner == "PERSON")
             val orgs = c.extr.getNers().asScala.toList.filter(n => n.ner == "ORGANIZATION")
             val country = mapper.getCountryName(c.extr.getArg2().getArgName(), lowercase=true)
             country match {
               case null => 
               case _ => {val newArg2 = new Argument(country, c.extr.getArg2().getStartOffset(), c.extr.getArg2().getEndOffset())
                          c.extr.setArg2(newArg2)}   
             }               
             (persons.size,orgs.size) match { 
               //person only in arg1 - take first person 
               case(p,o) if(p>0 && o==0) => {                 
                 val person = extractPerson(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(person)
                 c.extr.setRel("gpe:residents_of_country")
               }
               //org only in arg1 - take first org
               case(p,o) if(p==0 && o>0) => {
                 val org = extractOrganization(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(org)
                 c.extr.setRel("gpe:headquarters_in_country")    
               }
               //person and org in arg1 - take first person and first org
               case(p,o) if(p>0 && o>0) => {
                 val person = extractPerson(c)
                 val org = extractOrganization(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(person)
                 c.extr.setRel("gpe:residents_of_country")
                 val newExtraction = new KBPExtraction(c.extr.getArg1(), org, c.extr.getRel(), 
                    c.extr.getScore(), c.extr.getArg1Link(), c.extr.getArg1BestMention(), c.extr.getArg2Link(), c.extr.getArg2BestMention(),
			        c.extr.getDocName(), c.extr.getSentNum(), c.extr.getArg1BestMentionSentNum(), c.extr.getArg2BestMentionSentNum(), c.extr.getNers(),
			        c.extr.getSentence())
                 val newCandidate = new Candidate(queryCounter.getAndIncrement, newExtraction)
                 newCandidate.extr.setRel("gpe:headquarters_in_country")   
                 newCandidates = newCandidates ++ Seq(newCandidate)
               }
               case _ => 
             }
             
           }           
           
           // City
           case s if (s.contains("city")) => {
             val persons = c.extr.getNers().asScala.toList.filter(n => n.ner == "PERSON")
             val orgs = c.extr.getNers().asScala.toList.filter(n => n.ner == "ORGANIZATION")           

             //println
             //println("City: Size Persons: " + persons.size)
             //println("City: Size Orgs: " + orgs.size)
             //println("extr.arg1: " + c.extr.getArg1())
             //println("extr.arg2: " + c.extr.getArg2())
             //println("extr.ners: " + c.extr.getNers())
             //println
             
             (persons.size,orgs.size) match { 
               //person only in arg1 - take first person 
               case(p,o) if(p>0 && o==0) => {                 
                 val person = extractPerson(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(person)
                 c.extr.setRel("gpe:residents_of_city")
               }
               //org only in arg1 - take first org
               case(p,o) if(p==0 && o>0) => {
                 val org = extractOrganization(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(org)
                 c.extr.setRel("gpe:headquarters_in_city")    
               }
               //person and org in arg1 - take first person and first org
               case(p,o) if(p>0 && o>0) => {
                 val person = extractPerson(c)
                 val org = extractOrganization(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(person)
                 c.extr.setRel("gpe:residents_of_city")
                 val newExtraction = new KBPExtraction(c.extr.getArg1(), org, c.extr.getRel(), 
                    c.extr.getScore(), c.extr.getArg1Link(), c.extr.getArg1BestMention(), c.extr.getArg2Link(), c.extr.getArg2BestMention(),
			        c.extr.getDocName(), c.extr.getSentNum(), c.extr.getArg1BestMentionSentNum(), c.extr.getArg2BestMentionSentNum(), c.extr.getNers(),
			        c.extr.getSentence())
                 val newCandidate = new Candidate(queryCounter.getAndIncrement, newExtraction)
                 newCandidate.extr.setRel("gpe:headquarters_in_city")   
                 newCandidates = newCandidates ++ Seq(newCandidate)
               }
               case _ => 
             }
             
           }
           // State 
           case s if (s.contains("province")) => {
             val persons = c.extr.getNers().asScala.toList.filter(n => n.ner == "PERSON")
             val orgs = c.extr.getNers().asScala.toList.filter(n => n.ner == "ORGANIZATION")           
             (persons.size,orgs.size) match { 
               //person only in arg1 - take first person 
               case(p,o) if(p>0 && o==0) => {                 
                 val person = extractPerson(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(person)
                 c.extr.setRel("gpe:residents_of_stateorprovince")
               }
               //org only in arg1 - take first org
               case(p,o) if(p==0 && o>0) => {
                 val org = extractOrganization(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(org)
                 c.extr.setRel("gpe:headquarters_in_stateorprovince")    
               }
               //person and org in arg1 - take first person and first org
               case(p,o) if(p>0 && o>0) => {
                 val person = extractPerson(c)
                 val org = extractOrganization(c)
                 c.extr.setArg1(c.extr.getArg2)
                 c.extr.setArg2(person)
                 c.extr.setRel("gpe:residents_of_stateorprovince")
                 val newExtraction = new KBPExtraction(c.extr.getArg1(), org, c.extr.getRel(), 
                    c.extr.getScore(), c.extr.getArg1Link(), c.extr.getArg1BestMention(), c.extr.getArg2Link(), c.extr.getArg2BestMention(),
			        c.extr.getDocName(), c.extr.getSentNum(), c.extr.getArg1BestMentionSentNum(), c.extr.getArg2BestMentionSentNum(), c.extr.getNers(),
			        c.extr.getSentence())
                 val newCandidate = new Candidate(queryCounter.getAndIncrement, newExtraction)
                 newCandidate.extr.setRel("gpe:headquarters_in_stateorprovince")   
                 newCandidates = newCandidates ++ Seq(newCandidate)
               }
               case _ => 
             }
           }
           case _ =>   
         }        
       }              
    }
    
    candidates ++ newCandidates
    
  }

  def extractPerson(c: Candidate): Argument = {
    
    val persons = c.extr.getNers().asScala.filter(n => n.ner == "PERSON")
    var firstPersonName = ""
    var startOffset = 0
    var endOffset = 0
    if(persons.size > 0){
      firstPersonName = c.extr.getNers().asScala.filter(n => n.ner == "PERSON")(0).entityString
      val arg1 = c.extr.getArg1()
      startOffset = arg1.getStartOffset() + arg1.argName.indexOf(firstPersonName)
      endOffset = startOffset + firstPersonName.size - 1
    }
    
    val newArg = new Argument(firstPersonName, startOffset, endOffset)
    
    newArg
  }
  
  def extractOrganization(c: Candidate): Argument = {
    
    val orgs = c.extr.getNers().asScala.filter(n => n.ner == "ORGANIZATION")
    var firstOrganizationName = ""
    var startOffset = 0
    var endOffset = 0
    if(orgs.size > 0){
      firstOrganizationName = c.extr.getNers().asScala.filter(n => n.ner == "ORGANIZATION")(0).entityString
      val arg1 = c.extr.getArg1()
      startOffset = arg1.getStartOffset() + arg1.argName.indexOf(firstOrganizationName)
      endOffset = startOffset + firstOrganizationName.size - 1
    }
    
    val newArg = new Argument(firstOrganizationName, startOffset, endOffset)
    
    newArg
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
      query: KBPQuery): List[CorefMention] = {
  //def getMatchingCorefMentions(corefMap: scala.collection.mutable.Map[Integer, edu.stanford.nlp.dcoref.CorefChain], 
  //    queryFullName: String): List[CorefMention] = {
    
     var matchingCorefMentions: List[edu.stanford.nlp.dcoref.CorefChain.CorefMention] = Nil
     
     //println("corefMap size: " + corefMap.size)
     //println("queryFullName: " + queryFullName)
     //println("queryFullName: " + query.name)
     //val pattern = s"$queryFullName\\W".r
     
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
         
         val numAliases = query.aliases.size
         var aliasCount = 0
         var aliasMatch = false
         var queryName = query.aliases(aliasCount)
         var pattern = s"$queryName\\W".r
         
         while(!aliasMatch && aliasCount < numAliases){
         
           queryName = query.aliases(aliasCount)
           pattern = s"$queryName\\W".r
           
           pattern.findFirstIn(m.mentionSpan) match {
             case Some(x) => { matchingCorefMentions = matchingCorefMentions ::: corefMentions 
                               aliasMatch = true
                             }
             case None =>             
           }
           
           aliasCount += 1
           
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
  
  def filterExtractionsIncludingCoref(extractions: List[KBPExtraction], matchingCorefMentions: List[CorefMention], query: KBPQuery): List[KBPExtraction] = {
  //def filterExtractionsIncludingCoref(extractions: List[KBPExtraction], matchingCorefMentions: List[CorefMention], queryName: String): List[KBPExtraction] = {
  
    //val filteredExtractions = extractions.filter(e => e.getArg1().argName.contains(queryName) || checkForCorefMatch(e, matchingCorefMentions))
    
     val filteredExtractions = extractions.filter(e => containsQueryAlias(e.getArg1().argName, query) |
      (checkForCorefMatch(e, matchingCorefMentions) && checkForOverlapWithQueryName(e.getArg1().argName, query.name)) )    
    
    filteredExtractions    
  }

   def checkForOverlapWithQueryName(arg1Name: String, queryName: String): Boolean = {

    var overlap = false
    val queryNameTokens = queryName.split(" ")
    var tokenCount = 0
    val numTokens = queryNameTokens.size
    
    while(!overlap && tokenCount < numTokens){
      if(arg1Name.contains(queryNameTokens(tokenCount))) overlap = true                   
      tokenCount += 1
    }
    overlap
  }
  
  def containsQueryAlias(arg1: String, query: KBPQuery): Boolean = {
      
    var aliasMatch = false
    var aliasCount = 0
    val aliasSize = query.aliases.size  
    var queryName = query.aliases(aliasCount)
    var pattern = s"$queryName\\W".r
    //println("Alias Size: " + aliasSize)
    //println("Query Name: " + queryName)
    //println("Arg1: " + arg1)
    
    while(!aliasMatch && aliasCount < aliasSize){

      queryName = query.aliases(aliasCount)
      //println("Query Name: " + queryName)
      //println("Arg1: " + arg1)
      //pattern = s"$queryName\\W".r
      //pattern = s"$queryName\\z".r
      
      if(arg1.contains(queryName)){aliasMatch = true}
      //pattern.findFirstIn(arg1) match {
      //  case Some(x) => {aliasMatch = true}
      //  case None =>             
      //}       
      aliasCount += 1
    }
    //println("Alias Match: " + aliasMatch)
    aliasMatch
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
      //outputStream.println
      //outputStream.println("sentenceOffset/Text " + sentenceOffset + ": " + sentenceText)
      //outputStream.println
      
      //println("sentenceOffset: " + sentenceOffset)      

      relationExtractor.clearAllCaches()
      val implicitRelations = relationExtractor.extractRelations(sentenceText)      
      //println("implicitRelations size: " + implicitRelations.size)
      //val tagsToIgnore = Nil
      //val taggedImplicitRelations = tagNERs(implicitRelations, sentence, tagsToIgnore)
      
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
                    
          //println("getNERs size: " + ir.getNERs.size)
          
          val arg1BestMentionSentNum = 0
          val arg2BestMentionSentNum = 0
        
          val e = new KBPExtraction(arg1, arg2, rel, score,
			arg1Link, arg2Link, arg1BestMention,
			arg2BestMention, docName, sentNum,
			arg1BestMentionSentNum, arg2BestMentionSentNum, ir.getNERs.asJava, sentenceText)
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

        // -------------------------------------
        // Document Level -- Coref Cluster IDs
        // -------------------------------------
        for(k <- corefMap.keySet.toList.sorted){

          println("-------------------------")
          println("coref key: " + k)
          println("-------------------------")
          val corefMentions = corefMap(k).getMentionsInTextualOrder().asScala

          for(m <- corefMentions){            
            //println(m.toString())
            println("mention id: " + m.mentionID)
            println(">mention span: " + m.mentionSpan)
            println(">mention span length: " + m.mentionSpan.length())
            println(">mention type: " + m.mentionType)
            println(">mention type name(): " + m.mentionType.name())
            println(">mention sentnum: " + m.sentNum)
            println(">mention start index: " + m.startIndex)            
            println(">mention animacy: " + m.animacy)
            println(">mention gender: " + m.gender)
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
   
  def getRelevantSentencesIncludingCoref(document: Option[Annotation], query: KBPQuery, matchingCorefMentions: List[CorefMention]): List[CoreMap] = {
  //def getRelevantSentencesIncludingCoref(document: Option[Annotation], nameFilter: String, matchingCorefMentions: List[CorefMention]): List[CoreMap] = {
        
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
 
        //val pattern = s"$nameFilter\\W".r
        for (s <- sentencesAll) {          
          
          var sentenceMatch = false
          val numAliases = query.aliases.size
          var aliasCount = 0
          var aliasName = query.aliases(aliasCount)
          var pattern = s"$aliasName\\W".r

          while(!sentenceMatch && aliasCount < numAliases){

            aliasName = query.aliases(aliasCount)
            pattern = s"$aliasName\\W".r
            
            pattern.findFirstIn(s.get(classOf[TextAnnotation])) match {
              case Some(x) => { sentenceNums += s.get(classOf[SentenceIndexAnnotation])
                                sentenceMatch = true
                              }
              case None =>             
            }
            //if(s.get(classOf[TextAnnotation]).contains(nameFilter)) sentenceNums += s.get(classOf[SentenceIndexAnnotation])
          
            aliasCount += 1
            
          }
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
    // Setting max number of documents to 500
    val maxSize = 500
    //val maxSize = 5
    if(docs.size > maxSize){docs = docs.dropRight(docs.size-maxSize)}
    println("Docs.size: " + docs.size)
    var startTime :Long = 0
	var endTime: Long = 0 
    var docCount = 0
    for(doc <- docs) yield{
    // ---------------------------------------------------------------------  
    //for(doc <- documents.toList) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount + " " + doc)
        var a :Option[Annotation] = None
        val t = new Thread {
          override def run() {    
            startTime = System.currentTimeMillis()
            a = stanfordProcessDocument(doc)
            endTime = System.currentTimeMillis()
            println("Thread: Document took " + (endTime-startTime) + " milliseconds")     
          }         
        }  
        t.start()
        t.join(180000)     
        t.stop()
        a
    }
  }
  
  /*def processDocuments2(documents: Set[String]): List[Option[Annotation]] = {
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
      //docid is working here
      //val docID = processedDoc.get(classOf[DocIDAnnotation])
      //println("Document was Stanford Annotated: " + docID)
      println("Document was Stanford Annotated: " + docName)
      Some(processedDoc)
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }

}




