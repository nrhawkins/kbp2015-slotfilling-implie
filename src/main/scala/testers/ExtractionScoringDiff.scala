package testers

import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import scala.collection.mutable._
import scala.io.Source

import com.typesafe.config.ConfigFactory

import extractor._

/**
 * Main method takes: 
 * 
 * two extraction-scoring files 
 * both specified in the configuration file, extraction-scoring-diff.conf, 
 *
 * and outputs: 
 * a diff score report file, which is a print-out of the 
 * correct extractions in the first scoring report but not the second
 * and the correct extractions in the second scoring report but not the
 * first
 * 
 */

object ExtractionScoringDiff {

  // ------------------------------------------------------------------------
  // ScoringReportExtractionLine fields:
  // 1)SentenceIndex 2)DocId 3)Entity(NP) 4)Relation 
  // 5)Slotfill(tag) 6)Correct 7)Incorrect 8)Sentence
  // *Keep all of the fields so can write the diff correct extractions
  // ------------------------------------------------------------------------
  case class ScoringReportExtractionLine(sentIndex: Int, docid: String, 
      entity: String, relation: String, slotfill: String, correct: String, 
      incorrect: String, sentence: String)
  // ------------------------------------------------------------------------
  // MatchKey fields:
  // 1)SentenceIndex 2)DocId 3)Relation 4)Slotfill(tag) 5)Entity
  // example: Relation=jobtitle, Slotfill=coroner, Entity="coroner John Smith"
  // ------------------------------------------------------------------------
  case class MatchKey(sentIndex: Int, docid: String, relation: String, 
      slotfill: String, entity: String)
      
  // ----------------------------------------------------------
  // Configuration File - specifies input and output files
  // ----------------------------------------------------------  
  val config = ConfigFactory.load("extraction-scoring-diff.conf")  
  val seqFilename = config.getString("sequence-file")
  // --------------------------------------------------------------
  // seq - append this number in front of the files being output
  //     - this number is one greater than the last one written
  // --------------------------------------------------------------
  var seq = getSeqNum(seqFilename) - 1
  var scoringreport1_file = config.getString("input-dir") + 
     seq + config.getString("score-report-file-tail")
  var scoringreport2_file = config.getString("input-dir") + 
     (seq+1) + config.getString("score-report-file-tail")
  var scoringreportdiff_file = config.getString("output-dir") + 
     seq + "-" + (seq+1) + config.getString("score-report-diff-file-tail")

  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  // Main - args can optionally be used to specify the sequence
  //        number identifying the scoring reports to be diff'ed, 
  //        the inputs and outputs are specified by the .conf file,
  //        which has already be read-in and is a val of the object
  // -----------------------------------------------------------------      
  // -----------------------------------------------------------------
  def main(args: Array[String]) {
    
    println("es: Args length: " + args.length)

    if(args.length > 0){
      try{
         val seqNum = args(0).toInt              
         seq = seqNum - 1
         scoringreport1_file = config.getString("input-dir") + seq + 
            config.getString("score-report-file-tail")
         scoringreport2_file = config.getString("input-dir") + (seq+1) + 
            config.getString("score-report-file-tail")
         scoringreportdiff_file = config.getString("output-dir") + seq + "-" + (seq+1) + 
            config.getString("score-report-diff-file-tail")
      } 
      catch{
        case e: Exception => println("es: Command line argument for sequence number is not an integer.")
      }
    }
        
    println("es: Reading Scoring Report 1's Correct Extractions")
    
    // -------------------------------------------------------
    // Correct Extractions to Compare -  
    //   list of ScoringReportExtractionLine's
    //
    // Note: 
    // Any line not starting with an integer is ignored,
    // i.e. falls into the catch{}
    // This allows #comment lines in the input file
    // -------------------------------------------------------
    val extractionsCorrectSR1 = {
     
      val inputFilename = scoringreport1_file
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
             ScoringReportExtractionLine(tokens(0).toInt, tokens(1), fixEntityParens(tokens(2)), tokens(3), tokens(4),
                tokens(5), tokens(6), tokens(7))
        }catch{ 
           // if first field is not an integer, ignore it, by creating an ExtractionInputLine here
           // and filtering it out before returning the list
           // Eg. a header line will not start with an integer
           case e: Exception => ScoringReportExtractionLine(-1, "tokens(1)", "tokens(2)", "tokens(3)",
                "tokens(4)", "tokens(5)", "tokens(6)", "tokens(7)")
        }
        
      }).toList.filter(l => l.sentIndex >= 0).filter(l => l.correct == "1")
           
    } 
       
    println("es: Scoring Report 1's Correct Extractions size: " + extractionsCorrectSR1.size)

    println("es: Reading Scoring Report 2's Correct Extractions")
    
    // -------------------------------------------------------
    // Correct Extractions to Compare -  
    //   list of ScoringReportExtractionLine's
    //
    // Note: 
    // Any line not starting with an integer is ignored,
    // i.e. falls into the catch{}
    // This allows #comment lines in the input file
    // -------------------------------------------------------
    val extractionsCorrectSR2 = {
     
      val inputFilename = scoringreport2_file
    
      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"Sentence file $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
             ScoringReportExtractionLine(tokens(0).toInt, tokens(1), fixEntityParens(tokens(2)), tokens(3), tokens(4),
                tokens(5), tokens(6), tokens(7))
        }catch{ 
           // if first field is not an integer, ignore it, by creating an ExtractionInputLine here
           // and filtering it out before returning the list
           // Eg. a header line will not start with an integer
           case e: Exception => ScoringReportExtractionLine(-1, "tokens(1)", "tokens(2)", "tokens(3)",
                "tokens(4)", "tokens(5)", "tokens(6)", "tokens(7)")
        }
        
      }).toList.filter(l => l.sentIndex >= 0).filter(l => l.correct == "1")
           
    } 
       
    println("es: Scoring Report 2's Correct Extractions size: " + extractionsCorrectSR2.size)
    
    
    println("es: Create MatchKey Sets to Find Diffs")
    
    // -------------------------------------------------------------
    // Create MatchKey sets for Correct and Incorrect
    // -- can use these to check if the extraction is in either set
    // -------------------------------------------------------------
    val matchkeyItemsCorrectSR1 = for(correctExtraction <- extractionsCorrectSR1) yield {
       MatchKey(correctExtraction.sentIndex,correctExtraction.docid, correctExtraction.relation, 
           correctExtraction.slotfill, correctExtraction.entity)
    }
    
    val matchkeyItemsCorrectSR2 = for(correctExtraction <- extractionsCorrectSR2) yield {
       MatchKey(correctExtraction.sentIndex,correctExtraction.docid, correctExtraction.relation, 
           correctExtraction.slotfill, correctExtraction.entity)
    }

    println("es: MK Correct SR1 size: " + matchkeyItemsCorrectSR1.size)
    println("es: MK Correct SR2 size: " + matchkeyItemsCorrectSR2.size)
    
    
    println("es: Opening files for diff scoring report")

    // --------------------------------------------------------
    // --------------------------------------------------------
    // Check if output files exist already
    // If they do, exit with error message
    // --------------------------------------------------------
    // --------------------------------------------------------
    
    // Check if the diff scoring report file exists; if it does, exit with error message
    if (Files.exists(Paths.get(scoringreportdiff_file))) {
      System.out.println(s"Diff Scoring Report file $scoringreportdiff_file already exists!  " +
        s"Please set the number in $seqFilename to a non conflicting " +
        s"sequence number.\nExiting...")
      sys.exit(1)
    }

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    // Diff Scoring Report - write out
    //   1) Correct extractions in Scoring Report 1 but not in Scoring Report 2
    //   2) Correct extractions in Scoring Report 2 but not in Scoring Report 1
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    
    val diffscoringreport = new PrintWriter(scoringreportdiff_file)    

    
    // ------------------------------------------------------
    // Diff for Scoring Report 1
    // ------------------------------------------------------

    diffscoringreport.append("\n\n")
    diffscoringreport.append("Correct Extractions in Scoring Report 1 not in Scoring Report 2")
    diffscoringreport.append("\n\n")
    
    var extractionsCorrectDiffSR1 :Set[ScoringReportExtractionLine] = Set()

    // Do diff for Scoring Report 1
    extractionsCorrectSR1.foreach(e => {
      
       val extrCheck = MatchKey(e.sentIndex, e.docid, e.relation, e.slotfill, e.entity)       
       val correct = matchkeyItemsCorrectSR2.contains(extrCheck)
       if(!correct){
         extractionsCorrectDiffSR1 = extractionsCorrectDiffSR1 + e         
       }
       
      }
    )

    // If any elements in the diff write them to the diff scoring report
    if(extractionsCorrectDiffSR1.size > 0){
      
       diffscoringreport.append("SentenceIndex" + "\t" + "DocumentId" + "\t" + 
            "Entity" + "\t" + "Relation" + "\t" + "Slotfill(tag)" + 
            "\t" + "Correct" + "\t" + "Incorrect" + "\t" + "Sentence" + "\n")               

       extractionsCorrectDiffSR1.toList.sortBy(e => e.sentIndex).foreach(e => 
           diffscoringreport.append(e.sentIndex + "\t" + 
           e.docid + "\t" + e.entity + "\t" + e.relation + "\t" + e.slotfill + "\t" + 
           e.correct + "\t" + e.incorrect + "\t" + e.sentence + "\n"))       
    }
    
    // ------------------------------------------------------
    // Diff for Scoring Report 2
    // ------------------------------------------------------
    diffscoringreport.append("\n\n")
    diffscoringreport.append("Correct Extractions in Scoring Report 2 not in Scoring Report 1")
    diffscoringreport.append("\n\n")
    
    var extractionsCorrectDiffSR2 :Set[ScoringReportExtractionLine] = Set()

    // Do diff for Scoring Report 2
    extractionsCorrectSR2.foreach(e => {
      
       val extrCheck = MatchKey(e.sentIndex, e.docid, e.relation, e.slotfill, e.entity)       
       val correct = matchkeyItemsCorrectSR1.contains(extrCheck)
       if(!correct){
         extractionsCorrectDiffSR2 = extractionsCorrectDiffSR2 + e         
       }
       
      }
    )

    // If any elements in the diff write them to the diff scoring report
    if(extractionsCorrectDiffSR2.size > 0){
      
       diffscoringreport.append("SentenceIndex" + "\t" + "DocumentId" + "\t" + 
            "Entity" + "\t" + "Relation" + "\t" + "Slotfill(tag)" + 
            "\t" + "Correct" + "\t" + "Incorrect" + "\t" + "Sentence" + "\n")               

       extractionsCorrectDiffSR1.toList.sortBy(e => e.sentIndex).foreach(e => 
           diffscoringreport.append(e.sentIndex + "\t" + 
           e.docid + "\t" + e.entity + "\t" + e.relation + "\t" + e.slotfill + "\t" + 
           e.correct + "\t" + e.incorrect + "\t" + e.sentence + "\n"))       
    }
    
   
    println("es: Closing PrintWriters")    
                
    diffscoringreport.close()     
        
  }

  // This method is a bit of a hack.
  // We know that the sentence num is incremented after the sentence extractor,
  // so we take one less than what is recorded there.
  def getSeqNum(sequenceFile: String) = Source.fromFile(sequenceFile)
    .getLines().next().trim.toInt

  /**
   * If the entire entity is surrounded by quotes, remove them.
   * Replace any double quotes with a single quote.
   * @param entity
   */
  private def fixEntityParens(entity: String) = {
    val surroundingRemoved =
      if (entity(entity.length - 1) == '\"') {
        entity.substring(1, entity.length - 1)
      } else {
        entity
      }
    surroundingRemoved.replaceAll("\"\"", "\"")
  }
}
