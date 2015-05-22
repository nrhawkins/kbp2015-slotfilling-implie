package tac

import scala.collection.JavaConversions._
import collection.JavaConverters._
import edu.knowitall.openie.OpenIETs
import com.typesafe.config.ConfigFactory
//import java.io.File
//import java.io.IOException
import java.io._
import java.util.zip.GZIPInputStream
//import edu.washington.multirframework.util.DocUtils
import edu.stanford.nlp.ling.CoreAnnotations._
//import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.util.CoreMap
import extractor.{ImplicitRelationExtractor,ImplicitRelationExtractorNoLists,ConstrainedHighRecallImplIE,ModHighRecallImplIE,TaggerLoader}

object CreateKnowledgeBase {

  val config = ConfigFactory.load("tac-create-knowledge-base.conf")
  val corpusDirName = config.getString("corpus-dir")
  val splitDocsDirName = config.getString("split-docs-dir")  
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()
  val basicPipeline = annotatorHelper.getBasicPipeline()

  println("Loading ImplIE Tagger.")
  //val tagger = TaggerLoader.defaultTagger
  val tagger = TaggerLoader.extendedKeywordHighRecallTagger

  println("Loading ImplIE Extractor.")
  //val relationExtractor = new ImplicitRelationExtractor(tagger)
  //val relationExtractor = new ImplicitRelationExtractorNoLists(tagger)
  //val relationExtractor = new ModHighRecallImplIE(tagger)
  val relationExtractor = new ConstrainedHighRecallImplIE(tagger)
  
  //val openie = new OpenIE()
  val openie = new OpenIETs()
  
  
  def main(args: Array[String]) {
  
    val runID = "UWashington1"
    val detailed = false
    
    try {
      val corpusDir = new File(corpusDirName)  
      val multiDocFiles = FileUtils.findFiles(corpusDir)
      
      println("total memory: " + Runtime.getRuntime().totalMemory())
      //the Xmx value
      println("max memory: " + Runtime.getRuntime().maxMemory())
      println("free memory: " + Runtime.getRuntime().freeMemory())  		        
      println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
      println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
      
      println("Number of Files: " + multiDocFiles.size)
      val splitDocs = new DocSplitter()
      val cleanXML = true 
      var allDocs: Seq[AnnotatedDoc] = Nil
        
      for(file <- multiDocFiles){
        	
        //Replace the <tags> and /n with spaces, so Stanford CoreNLP can separate sentences,
        //and provide document-level offsets for the sentences.
        	
        if(!file.isDirectory()){	
          val docs = splitDocs.convert(file, cleanXML).toSeq
          allDocs = allDocs ++ docs
        }
        
      }      

      println("Number of Docs: " + allDocs.size)
      
      val firstDoc = allDocs(0)
      val rawDoc = firstDoc.getFirstSentence()
      val processedDoc = new Annotation(rawDoc)
      basicPipeline.annotate(processedDoc)
      val sentences = processedDoc.get(classOf[SentencesAnnotation]).asScala.toList      
      println("Number of Sentences: " + sentences.size)      

      //val testSentence = "U.S. President Barack Obama, born in Hawaii, spoke at the White House on Saturday about hula dancing, while his wife, Michelle looked on."   
      val testSentence = "Barack Obama lives in Detroit, Michigan, and Michelle Obama lived in Chicago, IL."
      
      val extractionsImplIE = relationExtractor.extractRelations(testSentence)
       
      println("Number of Extractions ImplIE: " + extractionsImplIE.size)
      
      extractionsImplIE.foreach(e => { println(e) 
                                 println })
      
      val extractionsOpenIE = openie.extract(testSentence)
 
      println("Number of Extractions OpenIE: " + extractionsOpenIE.size)
      
      extractionsOpenIE.foreach(e => { println(e)
                                 println(e.extraction.arg1.displayText)
                                 println(e.extraction.rel)
                                 println(e.extraction.arg2s(0).displayText)
                                 println })
      
      
      //for(file <- corpusDir.listFiles if file.getName endsWith ".gz"){
      //  val fileName = file.getAbsolutePath()
        //process the file
        //println("File exists: " + file.exists())  
        //println("File Path: " + file.getAbsolutePath())  
        //InputStream fileStream = new FileInputStream(filename);
        //InputStream gzipStream = new GZIPInputStream(fileStream);
        //Reader decoder = new InputStreamReader(gzipStream, encoding);
        //BufferedReader buffered = new BufferedReader(decoder);
        //bufferedReader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file.getAbsolutePath()), "UTF-8"))))        
        //val fileStream = new FileInputStream(fileName)
        //val gzipStream = new GZIPInputStream(fileStream)
        //val decoder = new InputStreamReader(gzipStream, "UTF-8")
        //val buffered = new BufferedReader(decoder)
        //val firstLine = buffered.readLine()       
        //val secondLine = buffered.readLine()
        //println("firstLine: " + firstLine)
        //println("secondLine: " + secondLine)
      //}  
    }
    catch { case e : Exception => e.printStackTrace()
    }
    
    
    println("KBP 2015!")
    
    println("total memory: " + Runtime.getRuntime().totalMemory())
    //the Xmx value
    println("max memory: " + Runtime.getRuntime().maxMemory())
    println("free memory: " + Runtime.getRuntime().freeMemory())  		        
    println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
    println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
    
    //val sentence = "U.S. President Barack Obama, born in Hawaii, spoke at the White House on Saturday about hula dancing, while his wife, Michelle looked on."      
    /*val extraction = openie.extract(sentence)
    println("Length extraction: " + extraction.size)
    extraction.foreach(e => println(e))
    println(extraction(0).extraction.arg1.displayText)
    println(extraction(0).extraction.arg1.offsets)
    println(extraction(0).extraction.rel)
    println(extraction(0).extraction.arg2s.size)
    println(extraction(0).extraction.arg2s)
    println(extraction(0).extraction.context)
    println(extraction(0).extraction.negated)
    println(extraction(0).extraction.passive)*/
    
  }  
    
}