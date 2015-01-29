package extractor

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{IWord, Pointer, IIndexWord, POS}
import edu.mit.jwi.morph.WordnetStemmer

import scala.collection.JavaConversions._

/**
 * Implements a hypernym filter on WordNet which can be used by an ImplIE
 * system.  Filters by hypernyms of the head word, recursed all the way up to
 * "entity" and hypernyms of any class that the head word is an instance of.
 */
trait WordNetHypernymFilter extends WordNetFilterable {
  protected val wordnetFilterParams: Map[String, WordNetFilter]
  protected val wordnetDictionary: Dictionary
  val stemmer = new WordnetStemmer(wordnetDictionary)

  /**
   * Filters by the (recursive) hypernyms as specified in wordnetFilterParams.
   * 
   * The filter parameters are assumed to be hypernyms of the head word,
   * or the hypernyms of the class that the head word is an instance of.
   *
   * To clarify how we treat "instance of",
   * "Amsterdam" is an instance of the class "national capital", which in turn
   * has a hypernym "capital".  Therefore, if the filter paramters include
   * the class "capital" as an acceptable hypernym, then an extraction with
   * the head word "Amsterdam" would be kept by the filter.
   *
   * @param line The source sentence that the extraction relations are from.
   * @param relations Extraction relations to filter.
   * @return Subset of the param{relations} the satisfy the WordNet hypernym 
   *         filters.
   */
  def filterWordNet(line: String, relations: List[ImplicitRelation]): List[ImplicitRelation] = {
    if (!wordnetDictionary.isOpen) {
      wordnetDictionary.open()
    }

    // For each head
    //    1. Search WordNet for all the hypernyms and hypernym instances
    //    2. Filter out ones in the reject list.
    //    3. Filter out any that aren't in the accept list.
    val results = relations.map(rel => {
      val filterParams = wordnetFilterParams.getOrElse(rel.tag.tag,
        WordNetFilter(rel.tag.tag, FilterLists(Nil, Nil), FilterLists(Nil, Nil)))
      val stemmedHead = stem(rel.head)

      if (stemmedHead == "") {
        return null
      } else {
        val idxWord = wordnetDictionary.getIndexWord(stemmedHead, POS.NOUN)
        val hypernyms = findAllHypernyms(idxWord)
        val instanceHypernyms = findAllInstanceHypernyms(idxWord)
        (filterParams, idxWord, hypernyms, instanceHypernyms, rel)
      }
    })
    .filter(q => q != null)
    // Remove ones in reject.
    .filter(quint => {
      val (filter, idxWord, hypernyms, instanceHypernyms, rel) = quint
      hypernyms.foldLeft(true)((acc, cur) =>
        !filter.reject.hypernyms.contains(cur) && acc) &&
        instanceHypernyms.foldLeft(true)((acc, cur) =>
          !filter.reject.hypernymInstances.contains(cur) && acc)
    })
    // Filter out ones that are not in the accept list.
    // TODO: Add extra pronouns : he, she, her, one, etc.
    .filter(quint => {
      val (filter, idxWord, hypernyms, instanceHypernyms, rel) = quint
//      val acceptListResult =
        hypernyms.foldLeft(false)((acc, cur) =>
          filter.accept.hypernyms.contains(cur) || acc) ||
          instanceHypernyms.foldLeft(false)((acc, cur) =>
            filter.accept.hypernymInstances.contains(cur) || acc)
//      No longer filtering pronouns here.  They can be added in another step if
//      wanted.
//      acceptListResult || rel.head != rel.head.toLowerCase
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
        Pointer.HYPERNYM_INSTANCE):::acc)
  }

  private def findAllHypernyms(idxWord: IIndexWord): List[String] = {
    if (idxWord == null) {
      return Nil
    }
    idxWord.getWordIDs.toList.foldLeft(Nil: List[String])((acc, wordID) =>
      expandByPointerThenHypernym(wordnetDictionary.getWord(wordID),
        Pointer.HYPERNYM):::acc)
  }

  private def expandByPointerThenHypernym(word: IWord,
                                          pointer: Pointer): List[String] = {
    val hypernyms = word
      .getSynset
      .getRelatedSynsets(pointer).toList
      .map(id => wordnetDictionary.getSynset(id).getWords)
    val lemmas = hypernyms.flatten.map(word => word.getLemma)

    lemmas:::hypernyms.foldLeft(Nil: List[String])((acc, hypernym) =>
      expandByPointerThenHypernym(hypernym(0), Pointer.HYPERNYM):::acc)
  }

  /**
   * Find a word's corresponding WordNet stem.
   *
   * @requires word must be a noun in the WordNet dictionary.
   * @param word the word to find the stem of.
   * @param n which stem to return - set this to 0, rarely will you want others.
s   * @return If word's nth WordNet stem exists, returns it; else empty string.
   */
  private def stem(word: String, n: Int = 0): String = {
    val stems = stemmer.findStems(word, POS.NOUN)
    if (stems.length > n) stems(n) else ""
  }
}
