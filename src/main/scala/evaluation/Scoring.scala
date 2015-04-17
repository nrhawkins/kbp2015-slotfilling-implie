package evaluation

import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import testers.TestResults

import scala.collection.mutable._
import scala.io.Source

import com.typesafe.config.{Config, ConfigFactory}

/**
 * Main method takes:
 * 1) config file
 * 2) an extractions file
 * 3) an answer key file,
 * both specified in the configuration file, extraction-scoring.conf,
 * and outputs:
 * 1) a score report file
 * 2) an answer key file with new extractions (to be scored) appended
 */

object Scoring {

  // ------------------------------------------------------------------------
  // AnswerKeyItem fields:
  // 1)SentenceIndex 2)DocId 3)Entity(NP) 4)Relation
  // 5)Slotfill(tag) 6)Correct 7)Incorrect 8)Sentence
  // *Keep all of the fields so can re-write file with new extractions
  // ------------------------------------------------------------------------
  case class AnswerKeyItem(sentIndex: Int, docid: String,
                           entity: String, relation: String, slotfill: String, correct: String,
                           incorrect: String, sentence: String)
  // ------------------------------------------------------------------------
  // ExtractionInputLine fields:
  // 1)SentenceIndex 2)DocId 3)Entity(NP) 4)Relation
  // 5)Slotfill(tag) 6)Sentence
  // *Keep all of the fields so untagged extractions can be written to file
  // ------------------------------------------------------------------------
  case class ExtractionInputLine(sentIndex: Int, docid: String,
                                 entity: String, relation: String, slotfill: String, sentence: String)
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
  var config: Config = null
  var seqFilename: String = null
  // --------------------------------------------------------------
  // seq - append this number in front of the files being output
  //     - this number is one greater than the last one written
  // --------------------------------------------------------------
  var seq: Int = -1
  var extractions_file: String = null
  var answerkey_file: String = null
  var scoringreport_file: String = null
  var newextractions_file: String = null

  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  // Main - args can optionally be used to specify the sequence number
  //        specifying which input and output files to read and write,
  //        the inputs and outputs are specified by the .conf file,
  //        which has already be read-in and is a val of the object
  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  def main(args: Array[String]) {

    println("es: Args length: " + args.length)

    if (args.length == 0) {
      System.out.println("First argument must specify the config file.")
      System.exit(1)
    }

    // Load in config information.
    config = ConfigFactory.load(args(0))
    seqFilename = config.getString("sequence-file")

    seq = getSeqNum(seqFilename) - 1
    extractions_file = config.getString("input-dir-results") +
      seq + config.getString("extractions-file-tail")
    answerkey_file = config.getString("input-dir-answers") +
      (seq-1) + config.getString("answer-key-file-tail")
    scoringreport_file = config.getString("output-dir") +
      seq + config.getString("score-report-file-tail")
    newextractions_file = config.getString("output-dir") +
      seq + config.getString("new-extractions-file-tail")

    if(args.length > 1){
      try{
        val seqNum = args(1).toInt
        seq = seqNum
        extractions_file = config.getString("input-dir-results") +
          seq + config.getString("extractions-file-tail")
        answerkey_file = config.getString("input-dir-answers") +
          (seq-1) + config.getString("answer-key-file-tail")
        scoringreport_file = config.getString("output-dir") +
          seq + config.getString("score-report-file-tail")
        newextractions_file = config.getString("output-dir") +
          seq + config.getString("new-extractions-file-tail")
      }
      catch{
        case e: Exception => println("es: Command line argument for sequence number is not an integer.")
      }
    }

    println("es: Reading Extractions")

    // -------------------------------------------------------
    // Extractions to Score -
    //   list of ExtractionInputLine's
    //
    // Note:
    // Any line not starting with an integer is ignored,
    // i.e. falls into the catch{}
    // This allows #comment lines in the input file
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
        try{
          //ExtractionInputLine(tokens(0).toInt, tokens(1), fixEntityParens(tokens(2)), tokens(3), tokens(4),
          ExtractionInputLine(tokens(0).toInt, tokens(1), fixEntityParens(tokens(2)), tokens(3), tokens(4).toLowerCase(),
            tokens(5))
        }catch{
          // if first field is not an integer, ignore it, by creating an ExtractionInputLine here
          // and filtering it out before returning the list
          // Eg. a header line will not start with an integer
          case e: Exception => ExtractionInputLine(-1, "tokens(1)", "tokens(2)", "tokens(3)",
            "tokens(4)", "tokens(5)")
        }

      }).toList.filter(l => l.sentIndex >= 0)

    }

    println("es: Extractions size: " + extractions.size)

    println("es: Reading Answer Key")

    // -------------------------------------------------------
    // Answer Key -
    //   list of AnswerKeyItem's
    // Note:
    // Any line not starting with an integer is ignored,
    // i.e. falls into the catch{}
    // This allows #comment lines in the input file
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
        try{
          if(tokens.length == 8){
            //sentIndex: Int, docid: String, entity: String, relation: String,
            //slotfill: String, correct: String, incorrect: String, sentence: String)
            AnswerKeyItem(tokens(0).toInt, tokens(1), fixEntityParens(tokens(2)),
              //tokens(3), tokens(4), tokens(5), tokens(6), tokens(7))
              tokens(3), tokens(4).toLowerCase(), tokens(5), tokens(6), tokens(7))
          }
          else{//tokens.length == 6
            AnswerKeyItem(tokens(0).toInt, tokens(1), fixEntityParens(tokens(2)),
              //tokens(3), tokens(4), "","", tokens(5))
              tokens(3), tokens(4).toLowerCase(), "","", tokens(5))
          }
        }catch{
          // if first field is not an integer, ignore it, by creating an ExtractionInputLine here
          // and filtering it out before returning the list
          // Eg. a header line will not start with an integer
          case e: Exception => AnswerKeyItem(-1, "tokens(1)", "tokens(2)", "tokens(3)",
            "tokens(4)", "tokens(5)", "tokens(6)", "tokens(7)")
        }

      }).toList.filter(l => l.sentIndex >= 0)
    }

    println("es: AKI size: " + answerkeyItems.size)

    println("es: Building Answer Key")

    // -------------------------------------------------------
    // Build Answer Key
    // -- Divide Answer Key Items by Correct or Incorrect
    // -------------------------------------------------------
    val answerkeyItemsCorrect = answerkeyItems.filter(aki => aki.correct == "1").toSet
    val answerkeyItemsIncorrect = answerkeyItems.filter(aki => aki.incorrect == "1").toSet

    println("es: AK Correct size: " + answerkeyItemsCorrect.size)
    println("es: AK Incorrect size: " + answerkeyItemsIncorrect.size)

    // -------------------------------------------------------------
    // Create MatchKey sets for Correct and Incorrect
    // -- can use these to check if the extraction is in either set
    // -------------------------------------------------------------
    val matchkeyItemsCorrect = for(answerkeyItem <- answerkeyItemsCorrect) yield {
      MatchKey(answerkeyItem.sentIndex,answerkeyItem.docid, answerkeyItem.relation,
        answerkeyItem.slotfill, answerkeyItem.entity)
    }

    val matchkeyItemsIncorrect = for(answerkeyItem <- answerkeyItemsIncorrect) yield {
      MatchKey(answerkeyItem.sentIndex,answerkeyItem.docid, answerkeyItem.relation,
        answerkeyItem.slotfill, answerkeyItem.entity)
    }

    println("es: MK Correct size: " + matchkeyItemsCorrect.size)
    println("es: MK Incorrect size: " + matchkeyItemsIncorrect.size)


    println("es: Opening files for scoring report and new extractions")

    // --------------------------------------------------------
    // --------------------------------------------------------
    // Check if output files exist already
    // If they do, exit with error message
    // --------------------------------------------------------
    // --------------------------------------------------------

    // Check if the scoring report file exists; if it does, exit with error message
    if (Files.exists(Paths.get(scoringreport_file))) {
      System.out.println(s"Scoring Report file $scoringreport_file already exists!  " +
        s"Please set the number in $seqFilename to a non conflicting " +
        s"sequence number.\nExiting...")
      sys.exit(1)
    }

    // Check if the new extractions file exists; if it does, exit with error message
    if (Files.exists(Paths.get(newextractions_file))) {
      System.out.println(s"Scoring Report file $newextractions_file already exists!  " +
        s"Please set the number in $seqFilename to a non conflicting " +
        s"sequence number.\nExiting...")
      sys.exit(1)
    }

    // ------------------------------------------------------------
    // ------------------------------------------------------------
    // Scoring Report - write out
    //   1) the extractions and their score (correct or incorrect)
    //   2) summary of results, overall and by relation
    //      -- # correct, # incorrect, precision
    // ------------------------------------------------------------
    // ------------------------------------------------------------

    val scoringreport = new PrintWriter(scoringreport_file)

    scoringreport.append("Input Files: " + "\n")
    scoringreport.append("   Extractions File: " + extractions_file + "\n")
    scoringreport.append("   Answer Key File: " + answerkey_file + "\n")
    scoringreport.append("Output Files: " + "\n")
    scoringreport.append("   Scoring Report: " + scoringreport_file + "\n")
    scoringreport.append("   New Answer Key: " + newextractions_file + "\n")
    scoringreport.append("\n\n")

    //scoringreport.append("SentenceIndex" + "\t" + "DocumentId" + "\t" +
    //        "Entity" + "\t" + "Relation" + "\t" + "Slotfill(tag)" +
    //        "\t" + "Correct" + "\t" + "Incorrect" + "\t" + "Sentence" + "\n")

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
    newextractions.append("SentenceIndex" + "\t" + "DocumentId" +
      "\t" + "Entity" + "\t" + "Relation" + "\t" + "Slotfill" + "\t" + "Correct" + "\t" +
      "Incorrect" + "\t" + "Sentence" + "\n")

    // Write correct answer key items
    answerkeyItemsCorrect.toList.sortBy(e => e.sentIndex).foreach(k => {
      newextractions.append(k.sentIndex + "\t" + k.docid +
        "\t" + k.entity + "\t" + k.relation + "\t" + k.slotfill + "\t" +
        k.correct + "\t" + k.incorrect + "\t" + k.sentence + "\n")
    })

    // Write incorrect answer key items
    answerkeyItemsIncorrect.toList.sortBy(e => e.sentIndex).foreach(k => {
      newextractions.append(k.sentIndex + "\t" + k.docid +
        "\t" + k.entity + "\t" + k.relation + "\t" + k.slotfill + "\t" +
        k.correct + "\t" + k.incorrect + "\t" + k.sentence + "\n")
    })


    println("es: Scoring Extractions")

    // -------------------------------------------------------
    // Summary Stats
    // -------------------------------------------------------
    var numCorrect = 0
    var numIncorrect = 0
    var numUntagged = 0
    val numMissed = matchkeyItemsCorrect.size
    val testResults = new TestResults()
    var relationScoresCorrect :HashMap[String,Int] = HashMap()
    var relationScoresIncorrect :HashMap[String,Int] = HashMap()

    // -------------------------------------------------------
    // Check each extraction
    // -------------------------------------------------------
    var extractionsCorrect :Set[ExtractionInputLine] = Set()
    var extractionsIncorrect :Set[ExtractionInputLine] = Set()

    for(extraction <- extractions){

      var correct = false
      var incorrect = false
      var untagged = false
      var correctField = ""
      var incorrectField = ""

      val extrCheck = MatchKey(extraction.sentIndex, extraction.docid, extraction.relation,
        extraction.slotfill, extraction.entity)

      correct = matchkeyItemsCorrect.contains(extrCheck)
      if(correct){ numCorrect += 1
        correctField = "1"

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
        newextractions.append(extraction.sentIndex + "\t" +
          extraction.docid + "\t" + extraction.entity + "\t" + extraction.relation + "\t" +
          extraction.slotfill + "\t" + correctField + "\t" + incorrectField + "\t" +
          extraction.sentence + "\n")
      }
      else{
        //scoringreport.append(extraction.sentIndex + "\t" + extraction.docid + "\t" +
        //    extraction.entity + "\t" + extraction.relation + "\t" + extraction.slotfill +
        //    "\t" + correctField + "\t" + incorrectField + "\t" + extraction.sentence + "\n")

        //Save the extractions so they can be written in groups (correct and incorrect)
        if(correct){
          extractionsCorrect = extractionsCorrect + extraction
        }
        else if(incorrect){
          extractionsIncorrect = extractionsIncorrect + extraction
        }

      }

    }

    println("es: Writing Scoring Report")

    println("es: Number of Correct Extractions: " + extractionsCorrect.size)
    println("es: Number of Incorrect Extractions: " + extractionsIncorrect.size)

    // -----------------------------------------------------------
    // Write Extractions found in Answer Key
    //
    // 1)Correct Extractions
    // 2)Incorrect Extractions
    //
    // -----------------------------------------------------------

    scoringreport.append("Correct Extractions ------------------------------------------------" + "\n\n")

    scoringreport.append("SentenceIndex" + "\t" + "DocumentId" + "\t" +
      "Entity" + "\t" + "Relation" + "\t" + "Slotfill(tag)" +
      "\t" + "Correct" + "\t" + "Incorrect" + "\t" + "Sentence" + "\n")

    extractionsCorrect.toList.sortBy(e => e.sentIndex).foreach(e =>
      scoringreport.append(e.sentIndex + "\t" + e.docid + "\t" +
        e.entity + "\t" + e.relation + "\t" + e.slotfill +
        "\t" + "1" + "\t" + "" + "\t" + e.sentence + "\n"))

    scoringreport.append("\n\nIncorrect Extractions ------------------------------------------------" + "\n\n")

    scoringreport.append("SentenceIndex" + "\t" + "DocumentId" + "\t" +
      "Entity" + "\t" + "Relation" + "\t" + "Slotfill(tag)" +
      "\t" + "Correct" + "\t" + "Incorrect" + "\t" + "Sentence" + "\n")

    extractionsIncorrect.toList.sortBy(e => e.sentIndex).foreach(e =>
      scoringreport.append(e.sentIndex + "\t" + e.docid + "\t" +
        e.entity + "\t" + e.relation + "\t" + e.slotfill +
        "\t" + "" + "\t" + "1" + "\t" + e.sentence + "\n"))

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

    // -----------------------------------------------------------
    // Write stats for each relation
    // -----------------------------------------------------------

    val relationScoresKeys = relationScoresCorrect.keySet ++ relationScoresIncorrect.keySet

    relationScoresKeys.foreach(k => {
      val extractionRelation = k
      val countCorrect = relationScoresCorrect.getOrElse(extractionRelation, 0)
      val countIncorrect = relationScoresIncorrect.getOrElse(extractionRelation, 0)
      val countMissed = matchkeyItemsCorrect.count(mki => mki.relation == extractionRelation)
      val testResultsRelation = new TestResults(countCorrect,countIncorrect,countMissed)
      var x = testResultsRelation.correct
      val formattedCorrect = f"$x%10s"
      x = testResultsRelation.incorrect
      val formattedIncorrect = f"$x%10s"
      var formattedPrecision = "%1.2f".format(testResultsRelation.precision)
      formattedPrecision = f"$formattedPrecision%10s"
      scoringreport.append(formattedCorrect + "\t\t" + formattedIncorrect + "\t\t" +
        formattedPrecision + "\t\t" + extractionRelation + "\n")
    })

    // -----------------------------------------------------------
    // Write stats for the total (i.e. includes all relations)
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


    println("es: Closing PrintWriters")

    newextractions.close()
    scoringreport.close()

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
