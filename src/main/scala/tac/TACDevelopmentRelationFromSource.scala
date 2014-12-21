package tac

/**
 * Created by Gene on 12/21/2014.
 */
object TACDevelopmentRelationFromSource {
  def main(args: Array[String]) {
    TACDevelopmentSentenceExtractor.main(Array())
    TACDevelopmentRelationExtractor.main(Array())
  }
}
