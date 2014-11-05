import com.github.nscala_time.time.Imports._

import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.{TaggerCollection, ParseRule}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type

import java.io.PrintWriter

import scala.collection.mutable
import scala.io.Source

/**
 * Automatically run and test the tagger.
 *
 * The term lists, input file, solutions file, and output directory are
 * specified in variables in the beginning of the class.
 * The results are written to a file [Joda current Datetime]-tagger-test and
 * shown on the console.
 *
 * The type of tagger affects the results.
 * Therefore the results folder has separate results for each tagger type.
 * The NormalizedKeywordTagger can identify non-normalized versions of normalized
 * tags, but will not identify tags that it doesn't consider to be normalized.
 *
 * @author Gene Kim (genelkim@cs.washington.edu)
 */
object TaggerTester {
  /*
  TODO: Next steps
    1. Create more tests for this to ensure that it works,
    2. Create tests for the extractor and format for it.
    3. Begin planning out extractor implementation.
   */

  type Tags = Set[(String, Int)]

  // TODO: reorganize file so main is either on top or bottom of other methods
  val CLASS_DIR = "classTermLists/runfiles/"
  val CLASSES = List(
    ("nationality", List(CLASS_DIR + "nationality.txt", CLASS_DIR + "country.txt")),
    ("jobTitle", List(CLASS_DIR + "jobTitle.txt")),
    ("religion", List(CLASS_DIR + "religion.txt"))
  )

  // Tagger things.
  case class Tagger(name: String, filePrefix: String)
  // Look in nlptools/taggers for other tagger options.
  val TAGGERS = Array(
    Tagger("CaseInsensitiveKeywordTagger", "case_insensitive"),
    Tagger("NormalizedKeywordTagger", "normalized"))
  val TAGGER = TAGGERS(0) // TODO: make a way to choose the tagger from command.
  val TAGGER_TYPE = TAGGER.name

  val TEST_DIR = "dev_Set/tagger/"
  val INPUT_FILE = TEST_DIR + "sentences.txt"
  val SOLUTION_FILE = TEST_DIR + s"${TAGGER.filePrefix}_solutions.txt"

  val RESULT_DIR = "results/tagger/"
  val RESULT_FILE_POSTFIX = "-tagger-test"
  val OUTPUT_FILE = outputFile
  val out = new PrintWriter(OUTPUT_FILE)

  // Tagger processing things.
  val chunker = new OpenNlpChunker()
  def process(text: String): Sentence with Chunked with Lemmatized = {
    new Sentence(text) with Chunker with Lemmatizer {
      val chunker = TaggerTester.this.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  /*
   * TODO: fill out comment better
   *  Steps:
   *    create a tagger instance with classes
   *    load sentences
   *    load solutions
   *    parse solutions into structures in memory
   *    run tagger on each sentence
   *    compare with expected values
   *    output final results
   */
  def main(args: Array[String]) {
    val taggerPattern = buildPatternString(CLASSES)

    // TODO: Print configurations in the beginning so that we can look at the
    //        output appropriately in the future:
    //          Class -> files, tagger type, etc.
    // TODO: Take the input files as arguments
    // Initialize inputs.
    val inputString = Source.fromFile(INPUT_FILE).mkString
    val solutions = Source.fromFile(SOLUTION_FILE).mkString

    // Setup structures for representing data.
    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(taggerPattern).get
    val t = rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    val lines = trimSplit(inputString, "\n")
    val results = getTaggerResults(t, lines)

    // Run through solutions and results.
    val counter = new TestResults
    val solutionIter = trimSplit(solutions, "\n").iterator
    for (result <- results) {
      out.println(s"Sentence:\t${result._2}")

      // Put solutions in a map from class to term (with its index).
      val solutionString = solutionIter.next()
      out.println(s"Expected:\t$solutionString")
      val solMap = getSolutions(solutionString)

      compareResultsToSolutions(result, solMap, counter)
      // Print missing tags, count them and add space before printing next line's results.
      out.print("\nMissing tags:\t")
      solMap.foreach(x => if (x._2.size > 0) out.print(s"$x\t"))
      counter.missed += solMap.foldLeft(0){ (acc, kv) => acc + kv._2.size }
      out.print("\n\n")
    }

    val resultString = getTestResults(counter)
    out.println(resultString)
    out.close()

    // Print to the console what was written to the file.
    print(Source.fromFile(OUTPUT_FILE).mkString)
  }

  /**
   * Builds string with the definitions of class term relation for tagger.
   * @param classes List of class to term list mappings.
   * @return String definitions of each class as a NormalizedKeywordTagger.
   */
  private def buildPatternString(classes: List[(String, List[String])]): String = {
    val builder = StringBuilder.newBuilder
    for (clas <- classes) {
      // TODO: determine which tagger to use, or create a method that merges the two tagger outputs.
      builder.append(s"${clas._1} := $TAGGER_TYPE {\n")
      for (file <- clas._2) {
        val lines = Source.fromFile(file).getLines()
        for (line <- lines) {
          if (line.trim.length != 0) {
            builder.append(line.trim.toLowerCase).append("\n")
          }
        }
      }
      builder.append("}\n")
    }
    builder.mkString
  }

  // Splits a string by a given regular expression and trims the results.
  private def trimSplit(str: String, regex: String): List[String] = str.split(regex).map(s => s.trim()).filter(s => s != "").toList

  /**
   * Gets the overall results for the tagger test.
   * @param counter Class containing the counts of correct, incorrect, and missing tags.
   * @return A String of the results.
   */
  private def getTestResults(counter: TestResults): String = {
    val builder = new StringBuilder
    builder ++= "Total Results\n"
    builder ++= s"Correct: ${counter.correct}\tIncorrect: ${counter.incorrect}\tMissing: ${counter.missed}\n"
    builder ++= f"Precision: ${counter.precision}%.3f\tRecall: ${counter.recall}%.3f\n"
    builder.toString()
  }

  /**
   * Runs the tagger on the given lines and returns results.
   * The lines are lowercased so that the casing doesn't matter.
   * Assumes that the tagger uses lowercased terms to tag as well.
   * @param tagger Tagger to run on the lines.
   * @param lines Lines to tag.
   * @return a list of pairs.  Each pairs holds a list of tags (Type),
   * and the line that the tags are from.
   */
  private def getTaggerResults(tagger: TaggerCollection[Sentence with Chunked with Lemmatized],
                               lines: List[String]): List[(List[Type], String)] = {
    var results: List[(List[Type], String)] = Nil
    for (line <- lines) {
      // TODO: tokenize line, then lowercase, then tag.  Lowercasing first causes errors in the tokenizer.
      val types = tagger.tag(process(line)).toList
      results ::= (types, line)
    }
    results.reverse
  }

  /**
   * Creates a mapping of the class to terms represented in the solution string.
   * @param sol String representation of the solution.
   * @return A map from a class name to a (mutable) set of term-index pairs.
   */
  private def getSolutions(sol: String): mutable.Map[String, mutable.Set[(String, Int)]] = {
    val solMap = mutable.Map[String, mutable.Set[(String, Int)]]()
    val solClasses = trimSplit(sol, "CLASS:")
    for (solClass <- solClasses) {
      // First one item is the class name, then term;index
      val terms = trimSplit(solClass, "TERM:")
      val clas = terms(0)
      val termSet = mutable.Set[(String, Int)]()
      for (i <- 1 until terms.length) {
        val termVal = trimSplit(terms(i), ";")
        if (termVal.length == 2) {
          termSet.add((termVal(0), termVal(1).toInt))
        } else {
          out.println(s"Wat... this is termVal? $termVal.  This usually " +
            s"indicates a solution input error such as using a : instead of ;")
        }
      }
      solMap.put(clas, termSet)
    }
    solMap
  }

  /**
   * Compares the result and solutions for a single line.
   * Prints the comparison to the results file.
   * @param result Results from the nlptools/tagger paired with the source string.
   * @param solMap Mapping of the solution classes to terms.
   * @param counter Counter for the total results for the test.
   */
  private def compareResultsToSolutions(
      result: (List[Type], String),
      solMap: mutable.Map[String, mutable.Set[(String, Int)]],
      counter: TestResults) {
    if (result._1.length != 0) {
      val typeByClass = result._1.sortBy(t => t.name)

      // Two tabs to line up with the others.
      out.print(s"Actual:\t\t")
      var curClass = ""
      for (typ <- typeByClass) {
        val text = typ.text.toLowerCase
        if (typ.name != curClass) {
          curClass = typ.name
          out.print(s"CLASS:$curClass\t")
        }
        out.print(s"TERM:$text;${typ.tokenInterval.end}")
        solMap.get(typ.name) match {
          // Unexpected class
          case None =>
            counter.incorrect += 1
            out.print(";Incorrect\t")
          case Some(termSet: mutable.Set[(String, Int)]) =>
            termSet.contains((text, typ.tokenInterval.end)) match {
              case true =>
                counter.correct += 1
                out.print("\t")
                // Remove from solutions so later we can see how many we missed.
                termSet.remove((text, typ.tokenInterval.end))
              case false =>
                counter.incorrect += 1
                out.print(";incorrect\t")
            }
          case leftovers =>
            out.print(s"EXTRA CASE: $leftovers\tTYPE: ${leftovers.getClass}")
            out.print("unexpected\t")
        }
      }
    }
  }

  // Gets the String for the output file.
  private def outputFile: String = {
    val datetime = DateTime.now
    RESULT_DIR + (datetime.toString + RESULT_FILE_POSTFIX).replace(":", ";")
  }
}

