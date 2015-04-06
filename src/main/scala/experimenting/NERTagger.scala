package experimenting

import edu.stanford.nlp.ie.crf.{CRFCliqueTree, CRFClassifier}
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.util.{StringUtils, CoreMap}

import scala.collection.JavaConversions._


/**
 * Created by Gene on 1/1/2015.
 */
object NERTagger {
  def main(args: Array[String]): Unit = {

    val classifier = CRFClassifier.getClassifier("models/english.all.3class.distsim.crf.ser.gz")
//    val results = classifier.classify("Dr. Anthony Alessi, who co-chairs the sports neurology section of the American Academy of Neurology, said smaller studies had indicated repeated concussions or other brain injuries can bring on early dementia.")
    val results = classifier.classify("Alessi was founded in 1921, and store shelves hold a history of familiar household items, such as the Dr. Skud fly swatter ($16) by French design superstar Philippe Starck.")

    println(results)
    println()

    val cliqueTree = classifier.getCliqueTree(results(0))

    for (i <- 0 until cliqueTree.length()) {
      val wi = results(0).get(i)
      val iter = classifier.classIndex.iterator()
      while (iter.hasNext) {
        val label = iter.next()
        val index = classifier.classIndex.indexOf(label)
        val prob = cliqueTree.prob(i, index)
        System.out.println("\t" + label + "(" + prob + ")")
      }
      val tag = StringUtils.getNotNullString(wi.get(classOf[CoreAnnotations.AnswerAnnotation]))
      println(wi)
      System.out.println("Class : " + tag)
    }


    for {
      result <- results.toList
    } {
      for (token <- result.toList) {
        println(token)
      }
    }
  }
}
