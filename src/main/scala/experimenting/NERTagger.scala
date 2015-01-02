package experimenting

import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConversions._


/**
 * Created by Gene on 1/1/2015.
 */
object NERTagger {
  def main(args: Array[String]): Unit = {
    val classifier = CRFClassifier.DEFAULT_CLASSIFIER
    println(classifier)
/*
    val results = classifier.classify("Hello.")
    for {
      result <- results.toList
    } {
      for (token <- result.toList) {
        println(token)
      }
    }
*/
  }
}
