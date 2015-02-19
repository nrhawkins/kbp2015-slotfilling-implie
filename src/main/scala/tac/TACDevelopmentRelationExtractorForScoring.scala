package tac

import java.io.{FileWriter, BufferedWriter, PrintWriter}
import java.nio.file.{Paths, Files}

import com.typesafe.config.ConfigFactory
import extractor.{FormalConstrainedImplIE, ImplicitRelationExtractor, NERFilteredIRE, TaggerLoader}

import scala.io.Source

/**
 * Main method takes the sentence file specified in tac-extractor-for-scoring.conf and
 * outputs the extractions to a file.
 *
 * Results are tab-delimited for easy processing and reading from a spreadsheet
 * program.
 * 
 * Gets sequence number from a file, and uses it in the name of the results file.
 * Increments that sequence number.
 * 
 */
object TACDevelopmentRelationExtractorForScoring {
  
  case class InputLine(index: Int, docid: String, sentence: String)

  val config = ConfigFactory.load("tac-extractor-for-scoring.conf")
  val resultDir = config.getString("result-dir")
  val sentenceDir = config.getString("sentence-dir")
  val seqFilename = config.getString("sequence-file")

  val sentenceFileSuffix = config.getString("sentence-file-suffix")
  val resultFileSuffix = config.getString("result-file-suffix")
  val nullFileSuffix = config.getString("null-result-file-suffix")

  // Use the fact that the current sequence number should be the one in the file
  // Later in this file, when creating the output file, 
  // increment the number in the seqFilename for the next run 
  val seq = getSeqNum(seqFilename) 

  def main(args: Array[String]) {
    // NOTE: very hacky arguments.
    // Functionality changes depending on number of arguments.
    val (inputLines, (out, nullout)) =
      if (args.size > 1) {
        // Run subset of sentences.
        val sentenceMin = args(0).toInt
        val sentenceMax = args(1).toInt
        (subsetInput(sentenceMin, sentenceMax), outputs)
      } else if (args.size > 0) {
        // Restore/Continue run.
        val sequenceNum = args(0).toInt
        (incompleteInput(sequenceNum), appendOutputs(sequenceNum))
      } else {
        val result = (input, outputs)

        // Starting a new file, print the column headers.
        val columnHeaders = Array("Sentence Index", "DocId",
          "Entity(NP)", "Relation", "Slotfill(tag)", "Sentence")
        val nullColumnHeaders = Array("Sentence Index",
          "DocId", "Sentence")
        result._2._1.println(columnHeaders.tail.foldLeft
                             (columnHeaders.head)
                             ((acc, cur) => acc + s"\t$cur"))
        result._2._2.println(nullColumnHeaders.tail.foldLeft
                             (nullColumnHeaders.head)
                             ((acc, cur) => acc + s"\t$cur"))
        result
      }
    
    println("Loading Extractor.")
    val relationExtractor =
      new ImplicitRelationExtractor(
//        new FormalConstrainedImplIE(
//        TaggerLoader.defaultTagger,
        TaggerLoader.correctedCapTagger,
//      new FormalConstrainedImplIE(
//        TaggerLoader.noTagsIgnoredTagger,
        config.getString("tokenization-cache"),
        config.getString("parse-cache"))

    println("Extracting Sentences.")
    println(s"${inputLines.size} sentences to process.")
    for (inputLine <- inputLines) {
      val extractions = relationExtractor.extractRelations(inputLine.sentence)
      if (extractions.length != 0) {
        for (extraction <- extractions) {
          // Sentence Index, Docid, Entity(NP), Relation, Slotfill(tag), Sentence
          out.println(
            s"${inputLine.index}\t${inputLine.docid}" +
              s"\t${extraction.np}\t${extraction.relation}\t${extraction.tag.asIndexedString}" +
              s"\t${inputLine.sentence}")
        }
      } else {
        // If no extraction, write to null outputs.
        nullout.println(
          s"${inputLine.index}\t${inputLine.docid}\t${inputLine.sentence}")
      }

      if (inputLine.index % 100 == 0) {
        println(s"Sentence ${inputLine.index} processed.")
      }
    }
    out.close()
    nullout.close()
  }


  def input = {
    
    // Always read from the 0-sentence-file
    val inputFilename = sentenceDir + sentenceFileSuffix
    //val inputFilename = sentenceDir + seq + sentenceFileSuffix

    // Check if the file exists.
    if (!Files.exists(Paths.get(inputFilename))) {
      System.out.println(s"Sentence file $inputFilename doesn't exist!  " +
        s"Please run the TACDevelopmentSentenceExtractor first, " +
        s"or check that the sequence number in $seqFilename is correct.\n" +
        s"Exiting...")
      sys.exit(1)
    }

    Source.fromFile(inputFilename).getLines().map(line => {
      val tokens = line.trim.split("\t")
      InputLine(tokens(0).toInt, tokens(1), tokens(2))
    }).toList
  }

  def incompleteInput(sequenceNum: Int) = {

    // Always read from the 0-sentence-file
    val inputFilename = sentenceDir + sentenceFileSuffix
    //val inputFilename = sentenceDir + sequenceNum + sentenceFileSuffix
    val outFilename = resultDir + sequenceNum + resultFileSuffix


    val lastSentenceProcessed = Source.fromFile(outFilename).getLines().toList.tail
      .foldLeft(-1)((acc, cur) =>
        Math.max(acc, cur.trim.split("\t")(1).toInt))

    // Remove up to the last sentence processed since later lines may be incomplete.
    val fixedFile = StringBuilder.newBuilder
    val lines = Source.fromFile(outFilename).getLines().toList
    fixedFile.append(lines.head + "\n")
    var done = false
    for (line <- lines.tail) {
      if (line.trim.split("\t")(0).toInt < lastSentenceProcessed && !done) {
        fixedFile.append(line)
      } else {
        done = true
      }
    }
    new PrintWriter(outFilename).println(fixedFile.mkString)

    // Get the input file.
    Source.fromFile(inputFilename).getLines()
      .filter(line => line.split("\t")(0).toInt >= lastSentenceProcessed)
      .map(line => {
      val tokens = line.trim.split("\t")
      InputLine(tokens(0).toInt, tokens(1), tokens(2))
    }).toList
  }

  def subsetInput(min: Int, max: Int) = {
    
    // Always read from the 0-sentence-file
    val inputFilename = sentenceDir + sentenceFileSuffix
    //val inputFilename = sentenceDir + seq + sentenceFileSuffix

    // Get the input file.
    Source.fromFile(inputFilename).getLines()
    .map(line => line.trim.split("\t"))
    .filter(tokens => {
      val index = tokens(0).toInt
      index >= min && index < max
    })
    .map(tokens => InputLine(tokens(0).toInt, tokens(1), tokens(2))).toList
  }


  def outputs = {
    val outFilename = resultDir + seq + resultFileSuffix
    val nullOutFilename = outFilename + nullFileSuffix

    // Check if the file exists.
    if (Files.exists(Paths.get(outFilename))) {
      System.out.println(s"Result file $outFilename already exists!  " +
        s"Please set the number in $seqFilename to a non conflicting " +
        s"sequence number.\nExiting...")
      sys.exit(1)
    }

    // If not, 
    // increment number in sequence-file
    new PrintWriter(seqFilename).append(s"${seq + 1}").close()
    // create new results file with the given sequence num.    
    (new PrintWriter(outFilename), new PrintWriter(nullOutFilename))
    
  }

  def appendOutputs(sequenceNum: Int) = {
    val outFilename = resultDir + sequenceNum + resultFileSuffix
    val nullOutFilename = outFilename + nullFileSuffix
    (new PrintWriter(new BufferedWriter(new FileWriter(outFilename, true))),
      new PrintWriter(new BufferedWriter(new FileWriter(nullOutFilename, true))))
  }

  // This method is a bit of a hack.
  // We know that the sentence num is incremented after the sentence extractor,
  // so we take one less than what is recorded there.
  def getSeqNum(sequenceFile: String) = Source.fromFile(sequenceFile)
    .getLines().next().trim.toInt
}
