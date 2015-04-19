package tac

import testers.ExtractionScoring

/**
 * Wrapper class to call: 
 * 
 * 1)TACDevelopmentRelationExtractorForScoring
 *
 * and
 * 
 * 2)ExtractionScoring
 * 
 */

object TACDevelopmentRelationExtractorAndScoring {


  def main(args: Array[String]) {
   
    val x = Array[String]()

    // Check that the necessary answerkey file exists.
    // -1 uses the most recent sequence number.
    ExtractionScoring.checkAnswerKeyFile(-1)

    println("wrapper: Extracting Results")
    TACDevelopmentRelationExtractorForScoring.main(x)

    println("wrapper: Scoring Extractions")
    ExtractionScoring.main(x)
    
  }

}