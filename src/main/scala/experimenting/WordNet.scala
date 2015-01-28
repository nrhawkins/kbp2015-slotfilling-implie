package experimenting

import java.net.URL

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{Pointer, POS}

import scala.collection.JavaConversions._

/**
 * Created by Gene Kim on 1/22/2015.
 */
object WordNet {
  def main(args: Array[String]) {
    // construct the URL to the Wordnet dictionary directory
    val path = "C:/Program Files (x86)/WordNet/2.1/dict"
    val url = new URL("file", null, path)

    // construct the dictionary object and open it
    val dict = new Dictionary(url)
    dict.open()

    // look up first sense of the word "dog "
    stuff("national_capital", dict)
    stuff("capital", dict)

  }

  def stuff(word: String, dict: Dictionary) {
    val idxWord = dict.getIndexWord(word, POS.NOUN)
    val wordIDs = idxWord.getWordIDs
    wordIDs.foreach(wordID => {
      val word = dict.getWord(wordID)
      println("Id = " + wordID)
      println(" Word = " + word)
      println(" Lemma = " + word.getLemma)
      println(" Gloss = " + word.getSynset.getGloss)
      val hypernyms = word.getSynset.getRelatedSynsets(Pointer.HYPERNYM).toList
                      .map(id => dict.getSynset(id))
      println(" Hypernyms = " +
        hypernyms.map(words => words.getWords.map(word => (word.getLemma, dict.getSenseEntry(word.getSenseKey).getSenseNumber))))
      println(" Instance of = " + word.getSynset
                                  .getRelatedSynsets(Pointer.HYPERNYM_INSTANCE)
                                  .map(id => dict.getSynset(id).getWords.map(word => word.getLemma)))
    })
    println
    println
  }
}
