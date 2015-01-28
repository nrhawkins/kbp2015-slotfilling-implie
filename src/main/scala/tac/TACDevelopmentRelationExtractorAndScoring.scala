package tac

import testers.{ExtractionScoring}

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
    
    println("wrapper: Extracting Results")
    TACDevelopmentRelationExtractorForScoring.main(x)

    println("wrapper: Scoring Extractions")
    ExtractionScoring.main(x)
    
  }

}