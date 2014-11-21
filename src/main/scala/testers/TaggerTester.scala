package testers

import java.io.PrintWriter

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.{TaggerCollection, ParseRule}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type
import org.joda.time.DateTime
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source

/**
 * Automatically run and test the tagger.
 *
 * The term lists, input file, solutions file, and output directory are
 * specified in tagger-test.conf.  The conf files are in the resources directory.
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

  type Tags = Set[(String, Int)]
  type TaggerSolution = (mutable.Map[String, mutable.Set[Tags]], String)
  type Comparison = (TaggerResult, TaggerSolution)
  case class Class(name: String, files: List[String])
  case class TaggerResult(tags: List[Type], source: String)

  // Configuratons.
  // Set it here so we don't have methods that are passing entire configurations.
  val config = ConfigFactory.load("tagger-tester.conf")
  val datetime = DateTime.now
  val INPUT_FILE = config.getString("input-file")
  val SOLUTION_FILE = config.getString("solution-file")
  val TAGGER_TYPE = config.getString("tagger-type")
  val OUTPUT_FILE = config.getString("output-dir") +
    datetime.toString.replace(":", ";") + config.getString("output-file-tail")
  val CLASS_DIR = config.getString("class-dir")
  val CLASSES = getClasses

  val tagger = getTagger
  val chunker = new OpenNlpChunker()

  def main(args: Array[String]) {
    val inputs = trimSplit(Source.fromFile(INPUT_FILE).mkString, "\n")
    val solutionLines = trimSplit(Source.fromFile(SOLUTION_FILE).mkString, "\n")
    val solutions = solutionLines.map(constructSolution)
    val output = new PrintWriter(OUTPUT_FILE)

    val testInfo = new TestInfo[String, TaggerResult, TaggerSolution](
      taggerFunction, inputs, solutions, compareResultToSolutions, output, configHeader())

    ModularTestRunner.runTests(testInfo)
    output.close()

    println(Source.fromFile(OUTPUT_FILE).mkString)
  }

  def taggerConfig(): String = {
    val out = StringBuilder.newBuilder
    out.append(s"Tagger: $TAGGER_TYPE\n")
    out.append(s"Class-term File Directory: $CLASS_DIR\n")
    out.append(s"Class term files:\n")
    for (clas <- CLASSES) {
      out.append(s"\tClass name: ${clas.name}\tFiles: ")
      clas.files.foreach(f => out.append(s"$f\t"))
      out.append("\n")
    }
    out.mkString
  }

  private def getTagger: TaggerCollection[Sentence with Chunked with Lemmatized] = {
    /**
     * Builds string with the definitions of class term relation for tagger.
     * @param classes List of class to term list mappings.
     * @return String definitions of each class as a NormalizedKeywordTagger.
     */
    def createTaggerDefinition(classes: List[Class]): String = {
      val builder = StringBuilder.newBuilder
      for (clas <- classes) {
        builder.append(s"${clas.name} := $TAGGER_TYPE {\n")
        for (file <- clas.files) {
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
    val taggerPattern = createTaggerDefinition(CLASSES)

    // Setup structures for representing data.
    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(taggerPattern).get
    rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
  }

  private def taggerFunction(line: String): TaggerResult = {
    def process(text: String): Sentence with Chunked with Lemmatized = {
      new Sentence(text) with Chunker with Lemmatizer {
        val chunker = TaggerTester.chunker
        val lemmatizer = MorphaStemmer
      }
    }
    TaggerResult(tagger.tag(process(line)).toList, line)
  }

  // Splits a string by a given regular expression and trims the results.
  private def trimSplit(str: String, regex: String): List[String] = str.split(regex).map(s => s.trim()).filter(s => s != "").toList

  /**
   * Creates a mapping of the class to terms represented in the solution string.
   * @param solutionString String representation of the solution.
   * @return A map from a class name to a (mutable) set of term-index pairs.
   */
  private def constructSolution(solutionString: String): TaggerSolution = {
    val solMap = mutable.Map[String, mutable.Set[Tags]]()
    val solClasses = trimSplit(solutionString, "CLASS:")
    for (solClass <- solClasses) {
      // First one item is the class name, then term;index
      val terms = trimSplit(solClass, "TERM:")
      val clas = terms(0)

      val termSet = solMap.getOrElse(clas, mutable.Set[Tags]())
      for (i <- 1 until terms.length) {
        // Break up by possible tags, add into a set.
        val possibleVals = trimSplit(terms(i), "\\|")
        var possibleTags: Set[(String, Int)] = Set.empty
        for (possibleVal <- possibleVals) {
          val termVal = trimSplit(possibleVal, ";")
          if (termVal.length == 2) {
            possibleTags += ((termVal(0), termVal(1).toInt))
          } else {
            println(s"Wat... this is termVal? $termVal.  This usually " +
              s"indicates a solution input error such as using a : instead of ;")
            // Fail out, the tester or files are not correct.
            sys.exit()
          }
        }
        termSet.add(possibleTags)
      }
      solMap.put(clas, termSet)
    }
    (solMap, solutionString)
  }

  /**
   * Compares the result and solutions for a single line.
   * @param comparison A pair of results from the tagger and the solutions
   *                   for the results.
   * @return Pair of test results and test string output.
   */
  private def compareResultToSolutions(comparison: Comparison): (TestResults, String) = {
    val result = comparison._1
    val solMap = comparison._2._1
    val out = mutable.StringBuilder.newBuilder
    val counter = new TestResults()
    if (result.tags.length != 0) {
      val typeByClass = result.tags.sortBy(t => t.name)

      out.append(s"Sentence:\t${comparison._1.source}\n")
      out.append(s"Expected:\t${comparison._2._2}\n")
      // Two tabs to line up with the others.
      out.append(s"Actual:\t\t")
      var curClass = ""
      for (typ <- typeByClass) {
        val text = typ.text.toLowerCase
        val index = typ.tokenInterval.end
        if (typ.name != curClass) {
          curClass = typ.name
          out.append(s"CLASS:$curClass\t")
        }
        out.append(s"TERM:$text;$index")
        // TODO: merge match statements by using getOrElse[empty set] and then just using the inner match
        solMap.get(typ.name) match {
          // Unexpected class
          case None =>
            counter.incorrect += 1
            out.append(";Incorrect\t")
          case Some(termSet: mutable.Set[Tags]) =>
            val matching = termSet.filter(tags => tags.contains((text, index)))
            matching.size match {
              case 0 =>
                counter.incorrect += 1
                out.append(";Incorrect\t")
              case 1 =>
                counter.correct += 1
                out.append("\t")
                termSet.remove(matching.iterator.next())
              case _ =>
                out.append(s"\nMultiple matching terms!?  Sets with matches: $matching, Term: $text, $index\n")
            }
          case leftovers =>
            out.append(s"EXTRA CASE: $leftovers\tTYPE: ${leftovers.getClass}")
            out.append("unexpected\t")
        }
      }
      out.append("\n")
    }
    counter.missed += solMap.count(p => p._2.size > 0)
    for (sol <- solMap) {
      val clas = sol._1
      val tags = sol._2
      out.append(s"MISSED\tCLASS:$clas")
      for (tag <- tags) {
        val hd = tag.seq.head
        out.append(s"\tTERM:${hd._1};${hd._2}")
        for (item <- tag.seq.tail) {
          out.append(s"|${item._1};${item._2}")
        }
      }
      out.append("\n")
    }

    (counter, out.mkString)
  }

  /**
   * Prints the configurations of the current test to the output file.
   */
  private def configHeader(): String = {
    val out = StringBuilder.newBuilder
    out.append(s"DateTime: ${datetime.toString}\n\n")
    out.append("Configurations\n")
    out.append(s"Input File: $INPUT_FILE\n")
    out.append(s"Solution File: $SOLUTION_FILE\n")
    out.append(s"Output File: $OUTPUT_FILE\n")
    out.append(taggerConfig)
    out.append("\n")
    out.mkString
  }

  def getClasses: List[Class] = {
    val classes: List[Config] = config.getConfigList("classes").toList
    classes.map(c => Class(c.getString("name"), c.getStringList("files").toList))
  }
}
