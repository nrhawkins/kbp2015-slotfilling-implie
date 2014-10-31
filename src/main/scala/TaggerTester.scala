import java.io.File

import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.{NamedGroupType, TaggerCollection, ParseRule}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type

import scala.collection.mutable
import scala.io.Source

/**
 * Automatically run and test the tagger.
 *
 * @author Gene Kim (genelkim@cs.washington.edu
 */
object TaggerTester {
  // TODO: comment methods
  /**
   * Test result counter.  Has modifiable correct, incorrect and missing fields.
   * Default constructs sets all fields to zero.
   * Includes basic calculations: precison and recall.
   * TODO: add any other relevant calculations.
   * @param c
   * @param i
   * @param m
   */
  class TestResults(c: Int = 0, i: Int = 0, m: Int = 0) {
    var correct = c
    var incorrect = i
    var missed = m

    def precision = correct.toDouble / (correct + incorrect)
    def recall = correct.toDouble / (correct + missed)
  }


  val CLASS_DIR = "classTermLists/runfiles/"
  val CLASSES = List(
    ("nationality", List(CLASS_DIR + "nationality.txt", CLASS_DIR + "country.txt")),
    ("jobTitle", List(CLASS_DIR + "jobTitle.txt")),
    ("religion", List(CLASS_DIR + "religion.txt"))
  )

  val TEST_DIR = "dev_Set/tagger/"
  val INPUT_FILE = TEST_DIR + "sentences.txt"
  val SOLUTION_FILE = TEST_DIR + "solutions.txt"

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

  def printResults(counter: TestResults) {
    println("Total Results")
    println(s"Correct: ${counter.correct}\tIncorrect: ${counter.incorrect}\tMissing: ${counter.missed}")
    println(f"Precision: ${counter.precision}%.3f\tRecall: ${counter.recall}%.3f")
  }

  /*
   * Steps:
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

    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(taggerPattern).get
    val t = rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    val lines = trimSplit(inputString, "\n")

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
      println(s"Sentence:\t$sentence")

      // Put solutions in a map from class to term (with its index).
      val sol = solutionIter.next()
      println(s"Expected:\t$sol")
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
            println(s"Wat... this is termVal? $termVal")
          }
        }
        solMap.put(clas, termSet)
      }

      // Compare each value with the solutions.
      if (result._1.length != 0) {
        val resultClass = result._1(0).name
        // Two tabs to line up with the others.
        print(s"Actual:\t\tCLASS:$resultClass\t")
        for (typ <- result._1) {
          /*
           * typ.name
           * typ.tokenInterval
           * typ.text
           */
          // TODO: create mechanism that keeps track of which mistakes were made.
          // for now assert
          val term = typ.text
          val index = typ.tokenInterval.end
          print(s"TERM:$term;$index\t")
          // TODO: use match statement to add to correct or incorrect
          solMap.get(typ.name) match {
            // Unexpected class
            case None => counter.incorrect += 1
            case Some(termSet: Set[(String, Int)]) =>
              termSet.contains((typ.text, typ.tokenInterval.end)) match {
                case true =>
                  counter.correct += 1
                  // Remove from solutions so later we can see how many we missed.
                  termSet.remove((typ.text, typ.tokenInterval.end))
                case false => counter.incorrect += 1
              }
            case leftovers =>
              print(s"EXTRA CASE: " + leftovers)
          }
        }
      }
      counter.missed += solMap.foldLeft(0){ (acc, kv) => acc + kv._2.size }
      println()
      println()
    }
    printResults(counter)
  }
}

