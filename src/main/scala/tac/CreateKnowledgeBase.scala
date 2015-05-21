package tac

import scala.collection.JavaConversions._
import edu.knowitall.openie._
import com.typesafe.config.ConfigFactory
//import java.io.File
//import java.io.IOException
import java.io._
import java.util.zip.GZIPInputStream
//import edu.washington.multirframework.util.DocUtils

object CreateKnowledgeBase {

  val config = ConfigFactory.load("tac-create-knowledge-base.conf")
  val corpusDirName = config.getString("corpus-dir")
  val splitDocsDirName = config.getString("split-docs-dir")  
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()
  
  //val openie = new OpenIE()
  val openie = new OpenIETs()
  
  
  def main(args: Array[String]) {
  
    val runID = "UWashington1"
    val detailed = false
    
    try {
      val corpusDir = new File(corpusDirName)  
      val multiDocFiles = FileUtils.findFiles(corpusDir)
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
      //doc.getFirstSentence()
      
      
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