package tac

/**
 * Created by Gene on 12/21/2014.
 */
object TACDevelopmentExtractFromFiles {
  def main(args: Array[String]) {
    if (args.length != 1) {
      println("Extracting Sentences.")
      TACDevelopmentSentenceExtractor.main(args)
    }
    println("Extracting Relations.")
    TACDevelopmentRelationExtractor.main(args)
    println("COMPLETE")
  }
}
