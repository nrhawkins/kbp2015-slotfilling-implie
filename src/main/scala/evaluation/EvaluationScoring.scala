package evaluation

/**
 * Scoring object for the system evaluation.
 */
object EvaluationScoring {
  def main(args: Array[String]) {
    val config = "evaluation-scoring.conf"
    val newargs = config +: args
    Scoring.main(newargs)
  }
}
