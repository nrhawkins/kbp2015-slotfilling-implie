package testers

import java.io.PrintWriter

import scala.collection.mutable

/**
 * Generalized module for running tests.
 * Input an instance of testInfo and the tester runs the test and outputs
 * the results into the PrintWriter given.
 *
 * Main benefit of this is that it makes the test code cleaner and easier
 * to understand by forcing the tester to modularize the test code.
 *
 * @author Gene Kim (genelkim@cs.washington.edu)
 */
object ModularTestRunner {
  def runTests(testInfo: TestInfo) {
    val outputs = testInfo.inputs.map(testInfo.function)
    val results = outputs.zip(testInfo.expected).map(testInfo.comparator)
    val aggregateResults =
      results.foldLeft((new TestResults(), mutable.StringBuilder.newBuilder)) {
        (acc, cur) =>
          (acc._1 += cur._1, acc._2.append("\n").append(cur._2))
      }

    printResults(testInfo.output, aggregateResults._1,
      aggregateResults._2.mkString, testInfo.header, testInfo.footer)
  }

  private def printResults(out: PrintWriter, results: TestResults,
                           outputString: String, header: String, footer: String) {
    out.print(header)
    out.println(outputString)
    out.println()
    out.println(testResultsToString(results))
    out.print(footer)
    out.close()
  }

  /**
   * Gets the overall results for the tagger test.
   * @param counter Class containing the counts of correct, incorrect, and missing tags.
   * @return A String of the results.
   */
  private def testResultsToString(counter: TestResults): String = {
    val builder = new StringBuilder
    builder ++= "Total Results\n"
    builder ++= s"Correct: ${counter.correct}\tIncorrect: ${counter.incorrect}\tMissing: ${counter.missed}\n"
    builder ++= f"Precision: ${counter.precision}%.3f\tRecall: ${counter.recall}%.3f\n"
    builder.toString()
  }
}
