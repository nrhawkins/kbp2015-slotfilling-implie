package evaluation

/**
 * Wrapper class to call:
 * 1) EvaluationExtraction
 * 2) EvaluationScoring
 *
 * ... for convenience.
 */
object EvaluationExtractionAndScoring {
  def main(args: Array[String]) {
    println("wrapper: Extracting Results")
    EvaluationExtraction.main(Array())

    println("wrapper: Scoring Extractions")
    EvaluationScoring.main(Array())
  }
}
