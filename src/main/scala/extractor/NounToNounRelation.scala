package extractor

import edu.stanford.nlp.trees.TypedDependency

/**
 * TODO: complete
 * tag = tagged phrase
 * relation = class (nationality, jobTitle, etc)
 * np = noun phrase
 * sentence = original sentence
 * relationTrace = trace of dependency relations hopped to make the extraction
 */
class NounToNounRelation(t: IndexedString, r: String, n: IndexedString,
                         s: String = "", rt: List[TypedDependency] = Nil) {
  var tag = t
  var relation = r
  var np = n
  var sentence = s
  var relationTrace = rt
  override def toString: String = s"($tag, $relation, $np)"
  def longString: String = s"($tag, $relation, $np), $sentence"
}
