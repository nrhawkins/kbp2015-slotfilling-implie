package testers

import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import scala.collection.mutable._
import scala.io.Source
//import collection.mutable.HashMap

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
  // *Keep all of the fields so can re-write file with new extractions
  // ------------------------------------------------------------------------
  case class AnswerKeyItem(extrIndex: Int, sentIndex: Int, docid: String, 
      entity: String, relation: String, slotfill: String, correct: String, 
      incorrect: String, sentence: String)
  // ------------------------------------------------------------------------
  // ExtractionInputLine fields:
  // 1)ExtractionIndex 2)SentenceIndex 3)DocId 4)Entity(NP) 5)Relation 
  // 6)Slotfill(tag) 7)Sentence
  // *Keep all of the fields so untagged extractions can be written to file    
  // ------------------------------------------------------------------------
  case class ExtractionInputLine(extrIndex: Int, sentIndex: Int, docid: String, 
      entity: String, relation: String, slotfill: String, sentence: String)
  // ------------------------------------------------------------------------
  // MatchKey fields:
  // 1)SentenceIndex 2)DocId 3)Relation 4)Slotfill(tag)
  // example: Relation=jobtitle, Slotfill=coroner
  // ------------------------------------------------------------------------
  case class MatchKey(sentIndex: Int, docid: String, relation: String, 
      slotfill: String) 
      
  // ------------------------------------------------------------------------
  // RelationCounts:
  // 1)relation 2)numCorrect 3)numIncorrect 4)numUntagged 
  // ------------------------------------------------------------------------
  //case class RelationCounts(relation: String, correct: Int, incorrect: Int) 
      
  // ----------------------------------------------------------
  // Configuration File - specifies input and output files
  // ----------------------------------------------------------  
  val config = ConfigFactory.load("extraction-scoring.conf")  
  val seqFilename = config.getString("sequence-file")
  // --------------------------------------------------------------
  // seq - append this number in front of the files being output
  //     - this number is one greater than the last one written
  // --------------------------------------------------------------
  val seq = getSeqNum(seqFilename) - 1 
  val extractions_file = config.getString("extractions-file")
  val answerkey_file = config.getString("answer-key")
  val scoringreport_file = config.getString("output-dir") + seq + 
    config.getString("score-report-file-tail")
  //val newextractions_file = config.getString("answer-key")
  val newextractions_file = config.getString("output-dir") + seq + 
    config.getString("new-extractions-file-tail")
  
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

    
    println("Reading Extractions")
    
    // -------------------------------------------------------
    // Extractions to Score -  
    //   list of ExtractionInputLine's
    // -------------------------------------------------------
    val extractions = {
     
      val inputFilename = extractions_file
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        ExtractionInputLine(tokens(0).toInt, tokens(1).toInt, tokens(2), tokens(3), tokens(4), 
            tokens(5), tokens(6))}).toList
    } 
    
    println("Extractions size: " + extractions.size)
    
    println("Reading Answer Key")
        
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
    val answerkeyItems = {
     
      val inputFilename = answerkey_file
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
          //println("tokens size: " + tokens.length)
          if(tokens.length ==9){
            //extrIndex: Int, sentIndex: Int, docid: String, entity: String, relation: String, 
            //slotfill: String, correct: String, incorrect: String, sentence: String)
            AnswerKeyItem(tokens(0).toInt, tokens(1).toInt, tokens(2), tokens(3), tokens(4), 
               tokens(5), tokens(6), tokens(7), tokens(8))
          }
          else{//tokens.length==7
            AnswerKeyItem(tokens(0).toInt, tokens(1).toInt, tokens(2), tokens(3), tokens(4), tokens(5), 
               "","", tokens(6))
          }
      }).toList
    }       
    
    println("AKI size: " + answerkeyItems.size)
    
    println("Building Answer Key")
           
    // -------------------------------------------------------
    // Build Answer Key 
    // -- Divide Answer Key Items by Correct or Incorrect
    // -------------------------------------------------------    
    val answerkeyItemsCorrect = answerkeyItems.filter(aki => aki.correct == "1").toSet
    val answerkeyItemsIncorrect = answerkeyItems.filter(aki => aki.incorrect == "1").toSet     

    println("AK Correct size: " + answerkeyItemsCorrect.size)
    println("AK Incorrect size: " + answerkeyItemsIncorrect.size)
    
    // -------------------------------------------------------------
    // Create MatchKey sets for Correct and Incorrect
    // -- can use these to check if the extraction is in either set
    // -------------------------------------------------------------
    val matchkeyItemsCorrect = for(answerkeyItem <- answerkeyItemsCorrect) yield {
       MatchKey(answerkeyItem.sentIndex,answerkeyItem.docid, answerkeyItem.relation, 
           answerkeyItem.slotfill)  
    }
    
    val matchkeyItemsIncorrect = for(answerkeyItem <- answerkeyItemsIncorrect) yield {
       MatchKey(answerkeyItem.sentIndex,answerkeyItem.docid, answerkeyItem.relation, 
           answerkeyItem.slotfill)  
    }

    println("MK Correct size: " + matchkeyItemsCorrect.size)
    println("MK Incorrect size: " + matchkeyItemsIncorrect.size)
    
    
    println("Opening files for scoring report and new extractions")

    // -------------------------------------------------------
    // -------------------------------------------------------
    // Scoring Report - write out
    //   1) the extractions and their score
    //   2) summary of results, overall and by relation
    //      -- # correct, # incorrect, precision
    // -------------------------------------------------------
    // -------------------------------------------------------
    val scoringreport = new PrintWriter(scoringreport_file)    
    
    scoringreport.append("SentenceIndex" + "\t" + "DocumentId" + "\t" + 
            "Entity" + "\t" + "Relation" + "\t" + "Slotfill(tag)" + 
            "\t" + "Correct" + "\t" + "Incorrect" + "\t" + "Sentence" + "\n")               
    
    // -------------------------------------------------------        
    // -------------------------------------------------------
    // New Extractions - write out
    //   1) the extractions not found in the answer key,
    //        so they can be tagged
    // -------------------------------------------------------  
    // -------------------------------------------------------        
    val newextractions = new PrintWriter(newextractions_file)    
    //val newextractions = new PrintWriter(new BufferedWriter(new FileWriter(newextractions_file, true)))    

    // -----------------------------------------------------------------------------
    // Write existing Answer Key to a new file, 
    //    here: write the header and existing answer key
    //    further down in this file: append new extractions 
    // -----------------------------------------------------------------------------       
    
    // Write header line
    newextractions.append("ExtractionIndex" + "\t" + "SentenceIndex" + "\t" + "DocumentId" +
        "\t" + "Entity" + "\t" + "Relation" + "\t" + "Slotfill" + "\t" + "Correct" + "\t" + 
        "Incorrect" + "\t" + "Sentence" + "\n")

    // Write correct answer key items
    answerkeyItemsCorrect.foreach(k => {
       newextractions.append(k.extrIndex + "\t" + k.sentIndex + "\t" + k.docid +
           "\t" + k.entity + "\t" + k.relation + "\t" + k.slotfill + "\t" + 
           k.correct + "\t" + k.incorrect + "\t" + k.sentence + "\n")             
    })

    // Write incorrect answer key items
    answerkeyItemsIncorrect.foreach(k => {
       newextractions.append(k.extrIndex + "\t" + k.sentIndex + "\t" + k.docid +
           "\t" + k.entity + "\t" + k.relation + "\t" + k.slotfill + "\t" + 
           k.correct + "\t" + k.incorrect + "\t" + k.sentence + "\n")             
    })
    
    
    println("Scoring Extractions")
    
    // -------------------------------------------------------
    // Summary Stats
    // -------------------------------------------------------
    var numCorrect = 0
    var numIncorrect = 0
    var numUntagged = 0
    val numMissed = matchkeyItemsCorrect.size
    val testResults = new TestResults()
    //var relationScores :HashMap[String,RelationCounts] = HashMap()
    var relationScoresCorrect :HashMap[String,Int] = HashMap()
    var relationScoresIncorrect :HashMap[String,Int] = HashMap()
    
    // -------------------------------------------------------
    // Check each extraction
    // -------------------------------------------------------
    for(extraction <- extractions){

      var correct = false
      var incorrect = false
      var untagged = false
      var correctField = ""
      var incorrectField = ""  
      //var correctRelation = 0
      //var incorrectRelation = 0
        
      val extrCheck = MatchKey(extraction.sentIndex, extraction.docid, extraction.relation, 
          extraction.slotfill)
       
      correct = matchkeyItemsCorrect.contains(extrCheck)
      if(correct){ numCorrect += 1
                   correctField = "1"  
                   //if(relationScores.contains(extraction.relation)){
                   //   correctRelation = relationScores(extraction.relation).correct + 1 
                   //   incorrectRelation = relationScores(extraction.relation).incorrect 
                   //}
                   //else{
                   //  correctRelation = 1 
                   //  incorrectRelation = 0 
                   //}
                   //relationScores += (extraction.relation -> 
                   //  RelationCounts(extraction.relation,correctRelation,incorrectRelation))
                   
                   if(relationScoresCorrect.contains(extraction.relation)){
                     relationScoresCorrect(extraction.relation) += 1
                   }  
                   else{
                     relationScoresCorrect(extraction.relation) = 1 
                   }                       
                     
                 } 
      else {
        incorrect = matchkeyItemsIncorrect.contains(extrCheck)        
        if(incorrect){ numIncorrect += 1
                       incorrectField = "1"
                       //if(relationScores.contains(extraction.relation)){
                       //  val correctRelation = relationScores(extraction.relation).correct
                       //  val incorrectRelation = relationScores(extraction.relation).incorrect + 1
                       //}
                       //else{
                       //  correctRelation = 0 
                       //  incorrectRelation = 1 
                       //}
                       //relationScores += (extraction.relation -> 
                       //   RelationCounts(extraction.relation,correctRelation,incorrectRelation))
                       if(relationScoresIncorrect.contains(extraction.relation)){
                         relationScoresIncorrect(extraction.relation) += 1
                       }  
                       else{
                         relationScoresIncorrect(extraction.relation) = 1 
                       }     
                           
                     }
        else {
          numUntagged += 1                        
          untagged = true
        }
      }

      if(untagged){
        newextractions.append(extraction.extrIndex + "\t" + extraction.sentIndex + "\t" + 
            extraction.docid + "\t" + extraction.entity + "\t" + extraction.relation + "\t" + 
            extraction.slotfill + "\t" + correctField + "\t" + incorrectField + "\t" + 
            extraction.sentence + "\n")
      }
      else{                     
        scoringreport.append(extraction.sentIndex + "\t" + extraction.docid + "\t" + 
            extraction.entity + "\t" + extraction.relation + "\t" + extraction.slotfill + 
            "\t" + correctField + "\t" + incorrectField + "\t" + extraction.sentence + "\n")        
      }
            
    }

    println("Writing Scoring Report")
    
    // -----------------------------------------------------------
    // Compute and Output Summary Statistics
    //
    // Overall and by relation, write:
    // 
    // --number correct, number incorrect, precision, relation
    // -----------------------------------------------------------
 
    scoringreport.append("\n" + "Number of Extractions: " + extractions.size + "\n")
    scoringreport.append("Number of Tagged Extractions: " + (extractions.size - numUntagged) + "\n")
    scoringreport.append("Number of Untagged Extractions: " + numUntagged + "\n")
    scoringreport.append("Number of AnswerKeyItems: " + answerkeyItems.size + "\n")
    scoringreport.append("Number of AnswerKeyItems Correct: " + answerkeyItemsCorrect.size + "\n")
    scoringreport.append("Number of AnswerKeyItems Incorrect: " + answerkeyItemsIncorrect.size + "\n")
    
    // ------------------------------------------------
    // Write heading line for table
    // ------------------------------------------------
    var xh = "Correct"
    val formattedCorrectHeading = f"$xh%10s"
    xh = "Incorrect"
    val formattedIncorrectHeading = f"$xh%10s"
    xh = "Precision"
    val formattedPrecisionHeading = f"$xh%10s"
    xh = "Relation"
    val formattedRelationHeading = f"$xh%10s"
    scoringreport.append("\n" + formattedCorrectHeading + "\t\t" + 
        formattedIncorrectHeading + "\t\t" + 
        formattedPrecisionHeading + "\t\t" + 
        formattedRelationHeading + "\n")    
    //val columnWidth = 10
        
    // ------------------------------------------------    
    // Write results by relation    
    // ------------------------------------------------    
    //relationScores.foreach(kv => { 
    //   val extractionRelation = kv._1
    //   testResults.correct = kv._2.correct
    //   testResults.incorrect = kv._2.incorrect
    //   testResults.missed = matchkeyItemsCorrect.filter(mki => mki.relation == extractionRelation).size
    //   scoringreport.append(testResults.correct + "\t" + testResults.incorrect + 
    //    "\t" + testResults.precision + "\t" + extractionRelation + "\n")  
    //  }
    //)
        
    val relationScoresKeys = relationScoresCorrect.keySet ++ relationScoresIncorrect.keySet    
        
    relationScoresKeys.foreach(k => {
      val extractionRelation = k
      val countCorrect = relationScoresCorrect.getOrElse(extractionRelation, 0)
      val countIncorrect = relationScoresIncorrect.getOrElse(extractionRelation, 0)
      val countMissed = matchkeyItemsCorrect.filter(mki => mki.relation == extractionRelation).size
      val testResultsRelation = new TestResults(countCorrect,countIncorrect,countMissed)
      var x = testResultsRelation.correct
      val formattedCorrect = f"$x%10s"
      x = testResultsRelation.incorrect
      val formattedIncorrect = f"$x%10s"
      var formattedPrecision = "%1.2f".format(testResultsRelation.precision)
      formattedPrecision = f"$formattedPrecision%10s"
      println(formattedCorrect)
      scoringreport.append(formattedCorrect + "\t\t" + formattedIncorrect + "\t\t" + 
           formattedPrecision + "\t\t" + extractionRelation + "\n")  
    })
        
        
    // -----------------------------------------------------------        
    // Write results for the total (i.e. includes all relations)   
    // -----------------------------------------------------------
    testResults.correct = numCorrect
    testResults.incorrect = numIncorrect
    testResults.missed = numMissed    
    
    var x = testResults.correct
    val formattedCorrect = f"$x%10s"
    x = testResults.incorrect
    val formattedIncorrect = f"$x%10s"    
    var formattedPrecision = "%1.2f".format(testResults.precision)
    formattedPrecision = f"$formattedPrecision%10s"
    
    scoringreport.append("\n" + formattedCorrect + "\t\t" + 
        formattedIncorrect + "\t\t" + 
        formattedPrecision + "\t\t" 
        + "total" + "\n")    
        
    
    println("Closing PrintWriters")    
                
    newextractions.close()    
    scoringreport.close()     
        
  }

  // This method is a bit of a hack.
  // We know that the sentence num is incremented after the sentence extractor,
  // so we take one less than what is recorded there.
  def getSeqNum(sequenceFile: String) = Source.fromFile(sequenceFile)
    .getLines().next().trim.toInt
  
  
}
