package testers

import java.io.PrintWriter

import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.trees.{TreePrint, TypedDependency}
import extractor._
import org.joda.time.DateTime
import utils.ExtractionFormatUtils

import scala.collection.mutable
import scala.io.Source

/**
 * TODO: print testing progress
 * TODO: complete comment.
 * TODO: put delimiters in config
 *
 * Uses the tagger specified in ModularTaggerTester.
 */

/**
 * Automatically run and test the NounToNounRelationExtractor.
 *
 * Configurations are in extractor-test.conf.  This also relies on some of the
 * TaggerTester, for running the tagger as part of the extractor.  Its
 * conigurations are in tagger-tester.conf.
 * The conf files are in the resources directory.
 *
 * The results are written to a file [Joda current Datetime]-tagger-test and
 * shown on the console.
 *
 * @author Gene Kim (genelkim@cs.washington.edu)
 */
object ExtractorTester {
  type Tags = Set[(String, Int)]
  type NPs = Set[(String, Int)]
  case class SingleSolutionSets(tags: Tags, nps: NPs)
  type ExtractorSolution = (mutable.Map[String, mutable.Set[SingleSolutionSets]], String)
  type Comparison = (ExtractorResult, ExtractorSolution)

  case class ExtractorResult(relations: List[ImplicitRelation], source: String)

  val tagger = TaggerTester.tagger
  val extractor = new ImplicitRelationExtractor(tagger)

  // Configuratons.
  // Set it here so we don't have methods that are passing entire configurations.
  val config = ConfigFactory.load("extractor-tester.conf")
  val datetime = DateTime.now
  val INPUT_FILE = config.getString("input-file")
  val SOLUTION_FILE = config.getString("solution-file")
  val OUTPUT_FILE = config.getString("output-dir") +
    datetime.toString.replace(":", ";") + config.getString("output-file-tail")

  type OptionMap = Map[Symbol, Any]
  val usage =
    """
      |Usage: ExtractorTester [--verbose | -v] [--tag-info | -ti bool] [-show-trace | -t]
    """.stripMargin

  def main(args: Array[String]) {
    // TODO: Add flexible command line arguments (https://github.com/scopt/scopt)
    // TODO: Add command line args to the config header
    // Basic command line arg support
    val arglist = args.toList
    val defaultOptions =
      Map('verbose -> false, 'taginfo -> true, 'showtrace -> false)
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
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
    val options = nextOption(defaultOptions, arglist)

    val inputs = trimSplit(Source.fromFile(INPUT_FILE).mkString, "\n")
    val solutions = trimSplit(Source.fromFile(SOLUTION_FILE).mkString, "\n")
      .map(constructSolution)

    val output = new PrintWriter(OUTPUT_FILE)

    val testInfo = new TestInfo[String, ExtractorResult, ExtractorSolution](
      extractorFunction, inputs, solutions,
      comparator(options),
      output, configHeader())

    ModularTestRunner.runTests(testInfo)
    output.close()

    println(Source.fromFile(OUTPUT_FILE).mkString)
  }

  private def extractorFunction(line: String): ExtractorResult = {
    val relations = extractor.extractRelations(line)
    ExtractorResult(relations, line)
  }

  private def constructSolution(solutionString: String): ExtractorSolution = {
    def getPossibilitySet(s: String): Set[(String, Int)] = {
      val possibleVals = trimSplit(s, "\\|")
      var possibilities: Set[(String, Int)] = Set.empty
      for (value <- possibleVals) {
        val pair = trimSplit(value, ";")
        if (pair.size != 2) {
          println(pair)
        }

        assert(pair.size == 2)
        possibilities += ((pair(0), pair(1).toInt))
      }
      possibilities
    }

    val solMap = mutable.Map[String, mutable.Set[SingleSolutionSets]]()
    val solClasses = trimSplit(solutionString, "CLASS:")
    for (solClass <- solClasses) {
      // First one is class name
      val termStrs = trimSplit(solClass, "TERM:")
      val clas = termStrs.head

      val solutionSets = solMap.getOrElse(clas, mutable.Set[SingleSolutionSets]())
      for (termStr <- termStrs.tail) {
        val tokens = trimSplit(termStr, "NP:")
        assert(tokens.size == 2) // one with the term info, other with the NP info
        val termString = tokens(0)
        val npString = tokens(1)

        val solSets = SingleSolutionSets(
          getPossibilitySet(termString), getPossibilitySet(npString))
        solutionSets.add(solSets)
      }
      solMap.put(clas, solutionSets)
    }
    (solMap, solutionString)
  }

  private def compareTags(solution: Tags, relation: ImplicitRelation): Boolean = {
    solution.contains((relation.tag.asIndexedString.string, relation.tag.index))
  }

  // Check that the solution is fully contained in the result relation.
  // Indices of solution are within the bounds of the result indices,
  // and the solution is a substring of the result.
  private def compareNPs(solution: NPs, relation: ImplicitRelation): Boolean = {
    val end = relation.np.index
    solution.count(np => {
      (np._2 <= end) && relation.np.string.contains(np._1)
    }) > 0
  }

  private def comparator(options: OptionMap)
                        (comparison: Comparison): (TestResults, String) = {
    val result = comparison._1
    val solMap = comparison._2._1
    val source = comparison._1.source
    val out = mutable.StringBuilder.newBuilder
    val counter = new TestResults()

    // Flags
    val verbose = options.getOrElse('verbose, false).asInstanceOf[Boolean]
    val showTagInfo = options.getOrElse('taginfo, true).asInstanceOf[Boolean]
    val showTrace = options.getOrElse('showtrace, false).asInstanceOf[Boolean]

    out.append(s"Sentence:\t${comparison._1.source}\n")
    out.append(s"Expected:\t${comparison._2._2}\n")
    out.append(s"Actual:\t")
    if (result.relations.length != 0) {
      val relationsByClass = result.relations.sortBy(t => t.relation)

      // Make all the tags lowercase for comparison
      relationsByClass.foreach(rel =>
        rel.tag = new TagInfo(rel.tag.tag.toLowerCase, rel.tag.text.toLowerCase,
          rel.tag.intervalStart, rel.tag.intervalEnd))

      for (relation <- relationsByClass) {
        out.append(s"CLASS:${relation.relation}\t" +
          s"TERM:${relation.tag.asIndexedString.string};${relation.tag.index}\t" +
          s"NP:${relation.np.string};${relation.np.index}")
        val solutionSets = solMap.getOrElse(relation.relation, mutable.Set())
        val matching = solutionSets.filter(solution =>
            compareTags(solution.tags, relation) &&
            compareNPs(solution.nps, relation))
        matching.size match {
          case 0 =>
            counter.incorrect += 1
            out.append(";Incorrect\t")
          case 1 =>
            counter.correct += 1
            out.append("\t")
            // TODO: make this less hacky...
            solutionSets.remove(matching.iterator.next())
          case _ =>
            out.append(s"\nMultiple matching terms!?  Sets with matches: $matching, " +
              s"Term: ${relation.tag.asIndexedString.string}, ${relation.tag.index} " +
              s"NP: ${relation.np.string}, ${relation.np.index}")
        }
        if (showTrace) {
          out.append(s"TRACE: ${relation.relationTrace}\t")
        }
      }
      out.append("\n")
    } else {
      out.append("NONE\n")
    }

    val missedSolutions = solMap.filter(p => p._2.foldLeft(0)((acc, cur) => acc + cur.tags.size + cur.nps.size) > 0)
    counter.missed += missedSolutions.size
    for (sol <- missedSolutions) {
      val clas = sol._1
      val solutionSets = sol._2
      out.append(s"MISSED\tCLASS:$clas")
      for (singleSets <- solutionSets) {
        def printHelp(items: Set[(String, Int)], itemType: String) {
          val hd = items.seq.head
          out.append(s"\t$itemType:${hd._1};${hd._2}")
          for (item <- items.seq.tail) {
            out.append(s"|${item._1};${item._2}")
          }
        }
        val tags = singleSets.tags
        printHelp(tags, "TERM")
        val nps = singleSets.nps
        printHelp(nps, "NP")
      }
      out.append("\n")
    }

    if (showTagInfo) {
      // Print the relation hops for each tag.
      out.append(ExtractionFormatUtils.extractionInfo(extractor)(source))
    }

    if (verbose) {
      out.append(ExtractionFormatUtils.verboseOutput(extractor)(source))
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
    out.append("\n")

    out.append("Tagger Config\n")
    out.append(TaggerTester.taggerConfig())

    out.mkString
  }

  // Splits a string by a given regular expression and trims the results.
  private def trimSplit(str: String, regex: String): List[String] = str.split(regex).map(s => s.trim()).filter(s => s != "").toList
}
