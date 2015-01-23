package extractor

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item._
import edu.stanford.nlp.trees.{TypedDependency, Tree, SemanticHeadFinder}

import scala.collection.JavaConversions._

/**
 * Trait for any class that wants to be able to filter extractions by WordNet
 * attributes.
 *
 * Most notably, it provides the filterWordNet function.
 */
trait WordNetFilterable {
  protected val wordnetFilters: Map[String, WordNetFilter]
  protected val wordnetDictionary: Dictionary

  def getParse(line: String): (Tree, List[TypedDependency])

  // TODO: Add index to the head.
  // TODO: add WordNetStemmer
  def addHeadsToExtractions(extractions: List[ImplicitRelation]) {
    // Get heads of the extractions.
    val headFinder = new SemanticHeadFinder()
    extractions.foreach(rel => {
      val tree = getParse(rel.np.string)._1
      rel.setHead(tree.headTerminal(headFinder).value())
    })
  }

  def filterWordNet(line: String, relations: List[ImplicitRelation]):
      List[ImplicitRelation] = {
    if (!wordnetDictionary.isOpen) {
      wordnetDictionary.open()
    }

    // For each head
    //    1. Search WordNet for all the hypernyms and hypernym instances
    //    2. Filter out ones in the reject list.
    //    3. Filter out any that aren't in the accept list, nor have a
    //         Pronoun head word.
    val results = relations.map(rel => {
      val filter = wordnetFilters.getOrElse(rel.tag.tag,
        WordNetFilter(rel.tag.tag, FilterLists(Nil, Nil), FilterLists(Nil, Nil)))

/*
      println(rel)
      println(rel.head)
      println
*/
      val idxWord = wordnetDictionary.getIndexWord(rel.head, POS.NOUN)
      val hypernyms = findAllHypernyms(idxWord)
      val instanceHypernyms = findAllInstanceHypernyms(idxWord)
      (filter, idxWord, hypernyms, instanceHypernyms, rel)
    })
    // Remove ones in reject.
    .filter(quint => {
      val (filter, idxWord, hypernyms, instanceHypernyms, rel) = quint
      hypernyms.foldLeft(true)((acc, cur) =>
        !filter.reject.hypernyms.contains(cur) && acc) &&
      instanceHypernyms.foldLeft(true)((acc, cur) =>
        !filter.reject.hypernymInstances.contains(cur) && acc)
    })
    // Filter out ones that are neither a Pronoun, nor in the accept list.
    // TODO: Add extra pronouns : he, she, her, one, etc.
    // For now we just check that the head word is not all lowercase.
    .filter(quint => {
      val (filter, idxWord, hypernyms, instanceHypernyms, rel) = quint
      val acceptListResult =
        hypernyms.foldLeft(false)((acc, cur) =>
          filter.accept.hypernyms.contains(cur) || acc) ||
        instanceHypernyms.foldLeft(false)((acc, cur) =>
          filter.accept.hypernymInstances.contains(cur) || acc)
      acceptListResult || rel.head != rel.head.toLowerCase
    }).map(_._5)

    wordnetDictionary.close()

    results
  }

  private def findAllInstanceHypernyms(idxWord: IIndexWord): List[String] = {
    if (idxWord == null) {
      return Nil
    }
    idxWord.getWordIDs.toList.foldLeft(Nil: List[String])((acc, wordID) =>
      expandByPointerThenHypernym(wordnetDictionary.getWord(wordID),
        Pointer.HYPERNYM_INSTANCE))
  }

  private def findAllHypernyms(idxWord: IIndexWord): List[String] = {
    if (idxWord == null) {
      return Nil
    }
    idxWord.getWordIDs.toList.foldLeft(Nil: List[String])((acc, wordID) =>
      expandByPointerThenHypernym(wordnetDictionary.getWord(wordID),
        Pointer.HYPERNYM))
  }

  private def expandByPointerThenHypernym(word: IWord,
                                          pointer: Pointer): List[String] = {
    val hypernyms = word
      .getSynset
      .getRelatedSynsets(pointer).toList
      .map(id => wordnetDictionary.getSynset(id).getWords)
    val lemmas = hypernyms.flatten.map(word => word.getLemma)
//    println(lemmas)

    lemmas:::hypernyms.foldLeft(Nil: List[String])((acc, hypernym) =>
        expandByPointerThenHypernym(hypernym(0), Pointer.HYPERNYM):::acc)
  }
}
