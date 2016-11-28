package tac

import java.io._
//import scala.collection.JavaConversions._
//import collection.JavaConverters._
//import edu.knowitall.openie.OpenIETs
//import com.typesafe.config.ConfigFactory
//import java.io.File
//import java.io.IOException
//import java.io._
//import java.util.zip.GZIPInputStream
//import edu.washington.multirframework.util.DocUtils
//import edu.stanford.nlp.ling.CoreAnnotations._
//import edu.stanford.nlp.ling.CoreLabel
//import edu.stanford.nlp.pipeline.Annotation
//import edu.stanford.nlp.util.CoreMap
//import extractor.{ImplicitRelationExtractor,ImplicitRelationExtractorNoLists,ConstrainedHighRecallImplIE,ModHighRecallImplIE,TaggerLoader}

object QSubTest {

  val outStreamDir = "/projects/WebWare6/KBP_2015/test/qsubTest"
  
  def main(args: Array[String]) {
   
    val outputStream = new PrintStream(outStreamDir)
 
    outputStream.println("this is a qsub test.")

    outputStream.close()
  
  }  
    
}