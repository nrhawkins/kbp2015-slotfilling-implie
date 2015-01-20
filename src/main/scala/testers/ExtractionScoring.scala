package testers

import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import scala.collection.mutable
import scala.io.Source

import com.typesafe.config.ConfigFactory

import extractor._
//import utils.ExtractionFormatUtils

/**
 * Main method takes an extractions file and an answer key file,
 * specified in a configurations file, extraction-scoring.conf, and
 * outputs a score report file. 
 */

object ExtractionScoring {

  // ------------------------------------------------------------------------
  // AnswerKeyItem fields:
  // 1)ExtractionIndex 2)SentenceIndex 3)DocId 4)Entity(NP) 5)Relation 
  // 6)Slotfill(tag) 7)Correct 8)Incorrect 9)Sentence
  // *Don't need to keep all of the fields
  // ------------------------------------------------------------------------
  case class AnswerKeyItem(sentIndex: Int, docid: String, relation: String, 
      slotfill: String, correct: Int, incorrect: Int, sentence: String)
  // ------------------------------------------------------------------------
  // ExtractionInputLine fields:
  // 1)ExtractionIndex 2)SentenceIndex 3)DocId 4)Entity(NP) 5)Relation 
  // 6)Slotfill(tag) 7)Sentence
  // *Keep all of the fields so untagged extractions can be written to file    
  // ------------------------------------------------------------------------
  case class ExtractionInputLine(sentIndex: Int, docid: String, entity: String,
      relation: String, slotfill: String, sentence: String)
  // ------------------------------------------------------------------------
  // MatchKey fields:
  // 1)SentenceIndex 2)DocId 3)Relation 4)Slotfill(tag)
  // example: Relation=jobtitle, Slotfill=coroner
  // ------------------------------------------------------------------------
  case class MatchKey(sentIndex: Int, docid: String, relation: String, 
      slotfill: String)  

  // --------------------------------------------------------------
  // seq - append this number in front of the files being output
  //     - this number is one greater than the last one written
  // --------------------------------------------------------------
  val seq = getSeqNum(seqFilename) - 1
  
  // ----------------------------------------------------------
  // Configuration File - specifies input and output files
  // ----------------------------------------------------------  
  // TODO: identify files to specify in the .conf file
  val config = ConfigFactory.load("extraction-scoring.conf")  
  val seqFilename = config.getString("sequence-file")
  val extractions_file = config.getString("extractions-file")
  val answerkey_file = config.getString("answer-key")
  val scoringreport_file = config.getString("output-dir") + seq + config.getString("score-report-file-tail")
  val newextractions_file = config.getString("output-dir") + seq + config.getString("new-extractions-file-tail")

  // ------------------------------------------------
  // Options - 1)verbose 2)taginfo 3)showtrace
  // ------------------------------------------------
  /*type OptionMap = Map[Symbol, Any]
  val usage =
    """
      |Usage: ExtractorTester [--verbose | -v] [--tag-info | -ti bool] [-show-trace | -t]
    """.stripMargin
  */

  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  // Main - args are options, 
  //        the inputs and outputs are specified by the .conf file,
  //        which has already be read-in and is a val of the object
  // -----------------------------------------------------------------      
  // -----------------------------------------------------------------
  def main(args: Array[String]) {
  
    //val arglist = args.toList

    // --------------------------------------------------------
    // Default Options: --verbose = false
    //                  --taginfo = true
    //                  --showtrace = false
    // --------------------------------------------------------
    //val defaultOptions =
    //  Map('verbose -> false, 'taginfo -> true, 'showtrace -> false)
      
    // ------------------------------------------------------------------------------
    // def nextOption - if the arglist is Nil, returns defaultOptions, 
    //                - else, recursively builds an option map based on the arglist 
    // ------------------------------------------------------------------------------
    /*  def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case ("--verbose" | "-v") :: tail =>
          nextOption(map ++ Map('verbose -> true), tail)
        case ("--tag-info" | "-ti") :: value :: tail =>
          nextOption(map ++ Map('taginfo -> value.toBoolean), tail)
        case ("--show-trace" | "-t") :: tail =>
          nextOption(map ++ Map('showtrace -> true), tail)
        case option :: tail =>
          println("Unknown option "+option)
          println(usage)
          sys.exit(1)
      }
    }
    */
    
    // ---------------------------------------------------
    // Options - set them to the values in the arglist,
    //           if there is a valid arglist, 
    //           else set them to the default values
    // ---------------------------------------------------
    //val options = nextOption(defaultOptions, arglist)
    
    // -------------------------------------------------------
    // Extractions to Score -  
    //   list of lines from extractions file
    // -------------------------------------------------------
    //val extractionsLines = Source.fromFile(extractions_file).getLines
    //  .map(line=>line.trim()).filter(line=>line!="").toList

    // -------------------------------------------------------
    // Extractions to Score -  
    //   list of ExtractionInputLine's
    // -------------------------------------------------------
    def extractions = {
     
      val inputFilename = extractions_file
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        ExtractionInputLine(tokens(1).toInt, tokens(2), tokens(3), tokens(4), tokens(5), tokens(6))
      }).toList
    } 
        
    // -------------------------------------------------------
    // Answer Key - 
    //   list of lines from answerkey file
    // -------------------------------------------------------    
    //val answerkeyLines = Source.fromFile(answerkey_file).getLines
    //  .map(line=>line.trim()).filter(line=>line!="").toList

    // -------------------------------------------------------
    // Answer Key -  
    //   list of AnswerKeyItem's
    // -------------------------------------------------------
    def answerkeyItems = {
     
      val inputFilename = answerkey_file
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        AnswerKeyItem(tokens(1).toInt, tokens(2), tokens(4), tokens(5), tokens(6).toInt,
            tokens(7).toInt, tokens(8))
      }).toList
    }       
      
    // -------------------------------------------------------
    // Scoring Report - write out
    //   1) the extractions and their score
    //   2) summary of results, overall and by relation
    //      -- # correct, # incorrect, precision
    // -------------------------------------------------------  
    val scoringreport = new PrintWriter(scoringreport_file)    
    
    // -------------------------------------------------------
    // New Extractions - write out
    //   1) the extractions not found in the answer key,
    //        so they can be tagged
    // -------------------------------------------------------  
    val newextractions = new PrintWriter(newextractions_file)    
    
    // -------------------------------------------------------
    // Scoring Steps:
    // 
    // 1. Build Answer Key
    // 2. Check each extraction
    // 3. Write out extractions not found in answer key
    // 4. Compute and write summary stats
    // 
    // -------------------------------------------------------

    // -------------------------------------------------------
    // Build Answer Key
    // -------------------------------------------------------    
    val answerkeyItemsCorrect = answerkeyItems.filter(aki => aki.correct == 1).toSet
    val answerkeyItemsIncorrect = answerkeyItems.filter(aki => aki.incorrect == 1).toSet     

    // -------------------------------------------------------
    // Build a Set of MatchKey's from AnswerKeyItem's above
    // -------------------------------------------------------
    val matchkeyItemsCorrect = for(answerkeyItem <- answerkeyItemsCorrect) yield {
       MatchKey(answerkeyItem.sentIndex,answerkeyItem.docid, answerkeyItem.relation, 
           answerkeyItem.slotfill)  
    }
    
    val matchkeyItemsIncorrect = for(answerkeyItem <- answerkeyItemsIncorrect) yield {
       MatchKey(answerkeyItem.sentIndex,answerkeyItem.docid, answerkeyItem.relation, 
           answerkeyItem.slotfill)  
    }
    
    // -------------------------------------------------------
    // Summary Stats
    // -------------------------------------------------------
    var numCorrect = 0
    var numIncorrect = 0
    var numUntagged = 0
    
    // -------------------------------------------------------
    // Check each extraction
    // -------------------------------------------------------
    for(extraction <- extractions){

      var correct = false
      var incorrect = false
      var untagged = false
      var correctField = ""
      var incorrectField = ""  
      
      val extrCheck = MatchKey(extraction.sentIndex, extraction.docid, extraction.relation, 
          extraction.slotfill)
       
      correct = matchkeyItemsCorrect.contains(extrCheck)
      if(correct){ numCorrect += 1
                   correctField = "1"} 
      else {
        incorrect = matchkeyItemsIncorrect.contains(extrCheck)        
        if(incorrect){ numIncorrect += 1
                       incorrectField = "1"}
        else {
          numUntagged += 1                        
          untagged = true
        }
      }

      if(untagged){
        newextractions.append(extraction.sentIndex + "\t" + extraction.docid + "\t" + 
            extraction.entity + "\t" + extraction.relation + "\t" + extraction.slotfill + 
            "\t" + extraction.sentence + "\n")  
      }
      else{                     
        scoringreport.append(extraction.sentIndex + "\t" + extraction.docid + "\t" + 
            extraction.entity + "\t" + extraction.relation + "\t" + extraction.slotfill + 
            "\t" + correctField + "\t" + incorrectField + "\t" + extraction.sentence + "\n")        
      }
            
    }
    
    // -------------------------------------------------------
    // Compute and Output Summary Stats
    //
    // Overall and by relation,
    // -number correct, number incorrect, precision
    // -------------------------------------------------------

    //TODO: this section
    //
    // scoringreport.append()    

    
  }

  // This method is a bit of a hack.
  // We know that the sentence num is incremented after the sentence extractor,
  // so we take one less than what is recorded there.
  def getSeqNum(sequenceFile: String) = Source.fromFile(sequenceFile)
    .getLines().next().trim.toInt
  
  
}
