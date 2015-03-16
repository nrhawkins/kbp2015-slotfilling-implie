package evaluation

/**
 * Extraction object for the system evaluation.
 */
object EvaluationExtraction {
  def main(args: Array[String]) {
    // Add config to arguments.
    val config = "extractor-for-evaluation.conf"
    val newargs = config +: args
    Extraction.main(newargs)
  }
}
