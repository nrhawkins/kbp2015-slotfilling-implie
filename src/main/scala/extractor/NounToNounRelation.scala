package extractor

/**
 * TODO: complete
 * tag = tagged phrase
 * relation = class (nationality, jobTitle, etc)
 * np = noun phrase
 * sentence = original sentence
 */
class NounToNounRelation(t: IndexedString, r: String, n: IndexedString, s: String = "") {
  var tag = t
  var relation = r
  var np = n
  var sentence = s
  override def toString: String = s"($tag, $relation, $np), $sentence"
}
