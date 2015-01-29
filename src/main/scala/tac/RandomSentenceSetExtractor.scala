package tac

import java.io.{FileWriter, BufferedWriter, PrintWriter}

import extractor.{ImplicitRelation, TaggerLoader, NERFilteredIRE}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.io.Source
import scala.util.Random

/**
 * Given a file with sentences, it extracts from a random subset of them
 * that have not been used before.
 *
 * The file must be the following format for each line:
 * [sentence index]\t[doc id]\t[sentence]
 */
object RandomSentenceSetExtractor {
  case class SentenceEntry(index: Int, docId: String, sentence: String)
  def main(args: Array[String]) {
    val numberOfSentencesToExtract = 1000
    val sentenceFile = "sentence_selection_data/all_sentences"
    val usedFile = "sentence_selection_data/used_sentences"
    val extractOutputDir = "results/random_sentence_set_extraction/"
    val seqNumFile = "results/random_sentence_set_extraction/file_sequence_num"

    println("Loading and selecting sentences.")

    val all = getSentenceEntries(sentenceFile)
    val used = getSentenceEntries(usedFile)
    val remaining = all -- used

    val selected = selectSentences(
      Nil, remaining.toList, numberOfSentencesToExtract, new Random)
    val split = splitList(Nil, selected, 20)

    println("Sentences Selected.")

    println("Loading Extractor.")
    val extractor = new NERFilteredIRE(TaggerLoader.basicTestTagger)

    println("Extracting Sentences.")
    var extractions: List[(List[ImplicitRelation], SentenceEntry)] = Nil
    var percent = 0
    for (sublist <- split) {
      extractions = sublist.map(se => (extractor.extractRelations(se.sentence), se)):::extractions
      percent += 5
      println(s"$percent%")
    }

    println("Printing Results.")
    val outFileName = extractOutputDir + outputFilename(seqNumFile)
    outputResults(outFileName, extractions)

    // At the end update the usedFile
    appendUsed(usedFile, selected)
  }

  def appendUsed(file: String, used: List[SentenceEntry]) {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(file, true)))
    used.foreach(se => out.println(s"${se.index}\t${se.docId}\t${se.sentence}"))
    out.close()
  }

  def outputResults(file: String, results: List[(List[ImplicitRelation], SentenceEntry)]) {
    val out = new PrintWriter(file)

    // Column Headers.
    val columnHeaders = Array("Sentence Index", "DocId",
      "Entity(NP)", "Relation", "Slotfill(tag)", "Sentence")
    out.println(
      columnHeaders.tail.foldLeft
      (columnHeaders.head)
      ((acc, cur) => acc + s"\t$cur"))

    // Data.
    for ((extractions, se) <- results) {
      if (extractions.length != 0) {
        for (extraction <- extractions) {
          // Sentence Index, Docid, Entity(NP), Relation, Slotfill(tag), Sentence
          out.println(
            s"${se.index}\t${se.docId}" +
              s"\t${extraction.np}\t${extraction.relation}\t${extraction.tag}" +
              s"\t${se.sentence}")
        }
      } else {
        // If no extraction, write NULL for the extraction.
        // Add 2 tabs to line up the sentence with the rest.
        out.println(
          s"${se.index}\t${se.docId}" +
            s"\tNULL\t\t" +
            s"\t${se.sentence}")
      }
    }
    out.close()
  }

  def splitList[T](acc: List[List[T]], lst: List[T], splits: Int): List[List[T]] = {
    splits match {
      case 0 => acc
      case _ =>
        val (front, back) = lst.splitAt(lst.size / splits)
        splitList(front::acc, back, splits - 1)
    }
  }

  def selectSentences(
    acc: List[SentenceEntry],
    sentences: List[SentenceEntry],
    remaining: Int,
    random: Random): List[SentenceEntry] = {

    if (remaining == 0) {
      return acc
    }

    if (sentences.size == 0) {
      println(s"No more sentences remaining, selected the ${acc.size} final " +
        s"sentences to extract.")
      return acc
    }

    val index = random.nextInt(sentences.size)
    val newSentence = sentences(index)
    val (front, back) = sentences.splitAt(index)
    selectSentences(newSentence::acc, front ++ back.tail, remaining - 1, random)
  }

  def getSentenceEntries(file: String): Set[SentenceEntry] = {
    Source.fromFile(file)
          .getLines()
          .filter(line => line.trim != "")
          .map(line => {
      val tokens = line.trim.split("\t").map(t => t.trim)
      assert(tokens.size == 3)
      SentenceEntry(tokens(0).toInt, tokens(1), tokens(2))
    }).toSet
  }

  def outputFilename(seqNumFile: String): String = {
    val seqNum = Source.fromFile(seqNumFile)
                 .getLines().next().trim.toInt
    new PrintWriter(seqNumFile).append(s"${seqNum + 1}").close()
    val dateStr = DateTimeFormat.forPattern("MM-dd-yyyy").print(DateTime.now)
    s"$seqNum[$dateStr]"
  }
}
