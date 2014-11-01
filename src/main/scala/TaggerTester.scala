import java.io.{PrintWriter, File}

import com.github.nscala_time.time.Imports._

import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.{TaggerCollection, ParseRule}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type

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
 * @author Gene Kim (genelkim@cs.washington.edu)
 */
object TaggerTester {
  // TODO: comment methods
  val CLASS_DIR = "classTermLists/runfiles/"
  val CLASSES = List(
    ("nationality", List(CLASS_DIR + "nationality.txt", CLASS_DIR + "country.txt")),
    ("jobTitle", List(CLASS_DIR + "jobTitle.txt")),
    ("religion", List(CLASS_DIR + "religion.txt"))
  )

  val TEST_DIR = "dev_Set/tagger/"
  val INPUT_FILE = TEST_DIR + "sentences.txt"
  val SOLUTION_FILE = TEST_DIR + "solutions.txt"

  val RESULT_DIR = "results/tagger/"
  val RESULT_FILE_POSTFIX = "-tagger-test"

  val chunker = new OpenNlpChunker()
  def process(text: String): Sentence with Chunked with Lemmatized = {
    new Sentence(text) with Chunker with Lemmatizer {
      val chunker = TaggerTester.this.chunker
      val lemmatizer = MorphaStemmer
    }
  }

  def buildPatternString(classes: List[(String, List[String])]): String = {
    val builder = StringBuilder.newBuilder
    for (clas <- classes) {
      builder.append(clas._1 + " := NormalizedKeywordTagger {\n")
      for (file <- clas._2) {
        val lines = Source.fromFile(file).getLines()
        for (line <- lines) {
          if (line.trim.length != 0) {
            builder.append (line.trim.toLowerCase).append ("\n")
          }
        }
      }
      builder.append("}\n")
    }
    builder.mkString
  }

  def trimSplit(str: String, regex: String): List[String] = {
    str.split(regex).map(s => s.trim()).filter(s => s != "").toList
  }

  def getResults(counter: TestResults): String = {
    val builder = new StringBuilder
    builder ++= "Total Results\n"
    builder ++= s"Correct: ${counter.correct}\tIncorrect: ${counter.incorrect}\tMissing: ${counter.missed}\n"
    builder ++= f"Precision: ${counter.precision}%.3f\tRecall: ${counter.recall}%.3f\n"
    builder.toString()
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

    // TODO: Take the input files as arguments
    val inputString = Source.fromFile(INPUT_FILE).mkString
    val solutions = Source.fromFile(SOLUTION_FILE).mkString

    val datetime = DateTime.now
    val outFileName = (datetime.toString + RESULT_FILE_POSTFIX).replace(":", ";")
    val out = new PrintWriter(RESULT_DIR + outFileName)

    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(taggerPattern).get
    val t = rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    val lines = trimSplit(inputString, "\n")

    // TODO: Put results grabbing in a method.
    var results: List[(List[Type], String)] = Nil
    for (line <- lines) {
      val types = t.tag(process(line)).toList
      results ::= (types, line)
    }
    results = results.reverse

    // Counter for results (correct, incorrect, missed).
    val counter = new TestResults

    // Compare the results to the solutions.
    val solutionIter = trimSplit(solutions, "\n").iterator
    for (result <- results) {
      val sentence = result._2
      out.println(s"Sentence:\t$sentence")

      // TODO: put solution grabbing in a mehtod.
      // Put solutions in a map from class to term (with its index).
      val sol = solutionIter.next()
      out.println(s"Expected:\t$sol")
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
            out.println(s"Wat... this is termVal? $termVal")
          }
        }
        solMap.put(clas, termSet)
      }

      // TODO: put solution and result comparing in a method.
      // Compare each value with the solutions.
      if (result._1.length != 0) {
        val resultClass = result._1(0).name
        // Two tabs to line up with the others.
        out.print(s"Actual:\t\tCLASS:$resultClass\t")
        for (typ <- result._1) {
          out.print(s"TERM:${typ.text};${typ.tokenInterval.end}\t")
          solMap.get(typ.name) match {
            // Unexpected class
            case None => counter.incorrect += 1
            case Some(termSet: mutable.Set[(String, Int)]) =>
              termSet.contains((typ.text, typ.tokenInterval.end)) match {
                case true =>
                  counter.correct += 1
                  // Remove from solutions so later we can see how many we missed.
                  termSet.remove((typ.text, typ.tokenInterval.end))
                case false => counter.incorrect += 1
              }
            case leftovers =>
              out.print(s"EXTRA CASE: $leftovers\tTYPE: ${leftovers.getClass}")
          }
        }
      }
      counter.missed += solMap.foldLeft(0){ (acc, kv) => acc + kv._2.size }
      out.println()
      out.println()
    }

    val resultString = getResults(counter)
    out.println(resultString)
    out.close()

    // Print to the console what was written to the file.
    print(Source.fromFile(RESULT_DIR + outFileName).mkString)
  }
}

