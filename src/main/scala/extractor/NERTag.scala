package extractor

import edu.stanford.nlp.ling.CoreLabel

/**
 * Created by Gene on 1/3/2015.
 */
class NERTag(
  entityStr: String,
  nerAnswer: String,
  bIndex: Int,
  eIndex: Int,
  indexedTokens: List[(CoreLabel, Int)]) {

  def entityString = entityStr
  def ner = nerAnswer
  def beginIndex = bIndex
  def endIndex = eIndex
  def tokens = indexedTokens

  override def toString =
    s"$entityStr\t$nerAnswer\t$bIndex\t$eIndex\t$indexedTokens"
}
