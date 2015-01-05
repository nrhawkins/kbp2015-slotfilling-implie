package experimenting

import edu.stanford.nlp.ie.AbstractSequenceClassifier
import edu.stanford.nlp.ie.crf._
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.ling.{Word, CoreLabel, CoreAnnotations}
import java.util.List

import extractor.TaggerLoader

import scala.collection.JavaConversions._

/** This is a demo of calling CRFClassifier programmatically.
  * <p>
  * Usage: {@code java -mx400m -cp "stanford-ner.jar:." NERDemo [serializedClassifier [fileName]] }
  * <p>
  * If arguments aren't specified, they default to
  * classifiers/english.all.3class.distsim.crf.ser.gz and some hardcoded sample text.
  * <p>
  * To use CRFClassifier from the command line:
  * </p><blockquote>
  * {@code java -mx400m edu.stanford.nlp.ie.crf.CRFClassifier -loadClassifier [classifier] -textFile [file] }
  * </blockquote><p>
  * Or if the file is already tokenized and one word per line, perhaps in
  * a tab-separated value format with extra columns for part-of-speech tag,
  * etc., use the version below (note the 's' instead of the 'x'):
  * </p><blockquote>
  * {@code java -mx400m edu.stanford.nlp.ie.crf.CRFClassifier -loadClassifier [classifier] -testFile [file] }
  * </blockquote>
  *
  * @author Jenny Finkel
  * @author Christopher Manning
  */
object NERDemo {
  @throws(classOf[Exception])
  def main(args: Array[String]) {
    var serializedClassifier: String = "models/english.all.3class.distsim.crf.ser.gz"
    if (args.length > 0) {
      serializedClassifier = args(0)
    }
    val classifier: AbstractSequenceClassifier[CoreLabel] = CRFClassifier.getClassifier(serializedClassifier)
    val tagger = TaggerLoader.basicTestTagger
/*

    val sentence = "I go to school at Stanford University, which is located in California."
    val simpleTokens = tagger.chunker.chunk(sentence)
    val tokens = simpleTokens.map(token => new Word(token.string))
    val classified = classifier.classifySentence(tokens)

    println(s"token size: ${tokens.size}\tclassified size: ${classified.size}")
    var i = 0
    for (tok <- simpleTokens) {
      println(s"$i\t${tok.chunk}\t${tok.offset}\t${tok.string}")
      i += 1
    }

    i = 0
    for (token <- tokens) {
      println(s"$i\t${token.word()}\t${token.value()}")
      i += 1
    }

    i = 0
    for (clas <- classified) {
      println(s"$i\t${clas.word()}\t${clas.get(classOf[CoreAnnotations.AnswerAnnotation])}")
      i += 1
    }
*/

    if (args.length > 1) {
      val fileContents: String = IOUtils.slurpFile(args(1))
      var out = classifier.classify(fileContents)
      for (sentence <- out) {
        for (word <- sentence) {
          System.out.print(word.word + '/' + word.get(classOf[CoreAnnotations.AnswerAnnotation]) + ' ')
        }
        System.out.println()
      }
      System.out.println("---")
      out = classifier.classifyFile(args(1))
      for (sentence <- out) {
        for (word <- sentence) {
          System.out.print(word.word + '/' + word.get(classOf[CoreAnnotations.AnswerAnnotation]) + ' ')
        }
        System.out.println()
      }
    } else {
      val example: Array[String] = Array("Good afternoon Rajat Raina, how are you today?",
        "I go to school at Stanford University, which is located in California.",
        "Both Warduni and a Christian community leader, Iyad al-Ashouri, accused the Iraqi government, notably the Ministry of Defense, of belittling the extent of the crisis in Mosul.",
        "Speaking with Bush at a news conference, Chinese President Hu said the Kyoto Protocol, the 1997 United Nations-sponsored effort that sets targets for industrialized nations to cut greenhouse gas emissions, should \"remain the main channel for international efforts to tackle climate change.\"",
        "He raised money from wealthy Americans, including Henry Luce, the founder of Time and Life magazines, and in 1919 founded and served as president of Yenching University, a Christian institution whose idyllic campus now is the site of Peking University."
      )
      for (str <- example) {
        System.out.println(classifier.classifyToString(str))
      }
      System.out.println("---")
      for (str <- example) {
        System.out.print(classifier.classifyToString(str, "slashTags", false))
      }
      System.out.println("---")
      for (str <- example) {
        System.out.println(classifier.classifyWithInlineXML(str))
      }
      System.out.println("---")
      for (str <- example) {
        System.out.println(classifier.classifyToString(str, "xml", true))
      }
      System.out.println("---")
      var i: Int = 0
      for (str <- example) {
        for (lcl <- classifier.classify(str)) {
          for (cl <- lcl) {
            System.out.print({
              i += 1; i - 1
            } + ": ")
            System.out.println(cl.toShorterString())
          }
        }
      }
    }
  }
}