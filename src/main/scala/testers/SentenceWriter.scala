package testers

import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import scala.collection.mutable._
import scala.io.Source

import com.typesafe.config.ConfigFactory

/**
 * Main method takes:
 * 1)a file of sentences (in the correct format)
 * 2)a file of a subset of sentences created in Excel (incorrect format) 
 * and outputs a new file of the subset of sentences in the correct format.  
 */

object SentenceWriter {

  
  // ----------------------------------------------------------
  // Configuration File - specifies input and output files
  // ----------------------------------------------------------  
  val config = ConfigFactory.load("sentence-writer.conf")  
  val sentences_file = config.getString("sentences-file")
  val sentences_id_file = config.getString("sentences-id-file")
  val newsentences_file = config.getString("new-sentences-file")
  
  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  // Main - args not used, 
  //        the inputs and outputs are specified by the .conf file,
  //        which has already be read-in and is a val of the object
  // -----------------------------------------------------------------      
  // -----------------------------------------------------------------
  def main(args: Array[String]) {

    //-------------------------------------------
    //-------------------------------------------
      println("Reading Sentences")
    //-------------------------------------------
    //-------------------------------------------
      
      val inputFilename = sentences_file

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      val sentences :HashMap[Int, String] = new HashMap()
      val sentenceLines = Source.fromFile(inputFilename).getLines()
      
      sentenceLines.foreach(l => {
        val tokens = l.trim.split("\t")  
        sentences(tokens(0).toInt) = tokens(1) + "\t" + tokens(2) 
      })
                  
    println("Sentences size: " + sentences.keySet.size)  
     
    //-------------------------------------------
    //-------------------------------------------
      println("Reading SentenceIndices")
    //-------------------------------------------
    //-------------------------------------------    
     val sentenceIndices = {
      
      val inputFilename = sentences_id_file

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }
      
      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        tokens(0).toInt   }).toSet
      
    } 
          
    println("Sentence Indices size: " + sentenceIndices.size)
    
    println("Open File for New Sentences")
       
    //---------------------------------------------------------
    //---------------------------------------------------------
    // Write subset of sentences in correct format
    //---------------------------------------------------------  
    //---------------------------------------------------------
    val newsentences = new PrintWriter(newsentences_file)    
        
    println("Write New Sentences")
    
    sentenceIndices.toList.sorted.foreach(si => {

      val sentIndex = si
      val docIdTabSentence = sentences(si)
      
      newsentences.append(sentIndex + "\t" + docIdTabSentence + "\n")
      
      }
    )    
    //-------------------------------------------
    //-------------------------------------------
      println("Closing PrintWriters")    
    //-------------------------------------------
    //-------------------------------------------
      
    newsentences.close()    
        
  }  
  
}
