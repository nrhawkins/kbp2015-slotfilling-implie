package tac

import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source
import collection.JavaConverters._

import com.typesafe.config.ConfigFactory

import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.ling.CoreAnnotations.DocIDAnnotation
import edu.stanford.nlp.ling.CoreAnnotations._


object SerializeColdStartCorpus {

  val config = ConfigFactory.load("serialize-cold-start-corpus.conf")
  val coldStartFilesFileName = config.getString("cold-start-files-file")
  val outputDirName = config.getString("output-dir")
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()

  
  def main(args: Array[String]) {

     // Access the Cold Start Corpus, Solr Index
     val corpus = "cs"
    
     val drop = args(0).toInt
     val dropRight = args(1).toInt
    
     SolrHelper.setConfigurations(corpus, false)
     
     var fileNameSet: Set[String] = Set()
    
    // -----------------------------------------------------
    // Read the list of file names in the cold start corpus     
    // ----------------------------------------------------- 
    val inputFilename = coldStartFilesFileName
    // Does file exist?
    if (!Files.exists(Paths.get(inputFilename))) {
      System.out.println(s"coldStartFiles file $inputFilename doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    } 
    // Read file, line by line
    var coldStartFileNames = Source.fromFile(inputFilename).getLines().toList
    
    coldStartFileNames = for(name <- coldStartFileNames) yield {
      val tokens = name.trim.split("\\.") 
      tokens.size match {
        case 2 => tokens(0)
        case 3 if tokens(1) == "mpdf" => tokens(0)
        case 3 => tokens(0) + "." + tokens(1)
        case _ => {println(name)
                   "not-a-file"
        }
      }      
    }

    //Drop file names from bad lines, if any
    coldStartFileNames = coldStartFileNames.filter(n => n != "not-a-file")
 
    //Check for duplicate file names
    coldStartFileNames.foreach(n => {
        if(!fileNameSet.contains(n)) fileNameSet += n
        else{
          println(n)
        }
      }
    )
   
    println("Num cs files: " + coldStartFileNames.size)
    println("outputDirName: " + outputDirName)
    
    val docsToProcess = coldStartFileNames.drop(drop).dropRight(dropRight).toSet

    //val docsToProcess = Set("NYT_ENG_20131231.0228")
    //val docsToProcess = Set("fffaa1de2ac709e2d7771a1ba40a816c.mpdf","fffaa1de2ac709e2d7771a1ba40a816c")

    println("Num files to process: " + docsToProcess.size)

    //return
    
      try{

        // -----------------------------
        // Process Documents
        // -----------------------------
        
        //val documents :List[Option[Annotation]] = {		       
		//  processDocuments(docsToProcess)		        
	    //}
        
        //println("Processed Documents size: " + documents.size)        

        var docCount = 0
        for(document <- docsToProcess){          

          docCount += 1
          println("Processing Doc # :" + docCount + " " + document)
          
          val doc : Option[Annotation] = processDocument(document)
          
          if(doc.isDefined){
            val x = doc.get
            //var sentences = x.get(classOf[SentencesAnnotation]).asScala.toList           
            val docID = x.get(classOf[DocIDAnnotation]) + ".ann"
            val fullDocIDName = outputDirName + docID
            //println("B4 Serializing - num sentences: " + sentences.size)        
            println("Serializing: " + fullDocIDName)

            // ------------------------------------------
            // Serialize the Annotation object
            // ------------------------------------------            
            Serializer.serialize(x, fullDocIDName)
            
            // ------------------------------------------
            // Test: deSerialize the Annotation object
            // ------------------------------------------
            //val docDS = Serializer.deserialize(fullDocIDName).asInstanceOf[Annotation]
            //var sentencesDS = docDS.get(classOf[SentencesAnnotation]).asScala.toList           
            //val docIDDS = x.get(classOf[DocIDAnnotation]) + ".ann"
            
            //println("deSerializing - doc id + num sentences: " + docIDDS + " " + sentencesDS.size)        
            
          }          
        }
        
      }
      catch {case e: Exception => 
	    {e.printStackTrace()
	      println("EXCEPTION: " ) 
	    }	  
	  }	
    
  }
 
  
  def processDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var startTime :Long = 0
	var endTime: Long = 0    	 
	var docCount = 0
	//var docs = documents.toList
	// Setting max number of documents to 500
    //val maxSize = 500
    //val maxSize = 5
    //if(docs.size > maxSize){docs = docs.dropRight(docs.size-maxSize)}
    //println("Docs.size: " + docs.size)
    for(doc <- documents.toList) yield{
    //for(doc <- docs) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount + " " + doc)
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
      t.stop()
      a
    }
  }
  
  def processDocument(document: String): Option[Annotation] = {
    var startTime :Long = 0
	var endTime: Long = 0    	    
    //println("Processing Doc:" + document)
    var a :Option[Annotation] = None
    val t = new Thread {
      override def run() {    
        startTime = System.currentTimeMillis()
        a = stanfordProcessDocument(document)
        endTime = System.currentTimeMillis()
        println("Thread: Document took " + (endTime-startTime) + " milliseconds")      
      }
    }                                              
    t.start()
    //t.join(10000)
    t.join(180000) 
    t.stop()
    a   
  }
  
  
  def stanfordProcessDocument(docName: String) : Option[Annotation] = { 	     
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      if(rawDoc.length < 20000){
        //println("Processing Doc # :" + docName)
        val processedDoc = new Annotation(rawDoc)
        annotatorHelper.getCorefPipeline().annotate(processedDoc)
        //need to set below when not running CJ
        processedDoc.set(classOf[DocIDAnnotation], docName)
        println("Document was Stanford Annotated: " + processedDoc.get(classOf[DocIDAnnotation]))
        //this is null below, comes from CJ call
        //println("DwSA SentDocName: " + processedDoc.get(classOf[SentDocName]))
        Some(processedDoc)
      }
      else{
        None
      }  
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }
      
}