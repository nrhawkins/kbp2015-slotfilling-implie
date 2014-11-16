package extractor

/**
 * Created by Gene on 11/10/2014.
 */
class NounToNounRelation(t: String, r: String, n: String, s: String = "") {
  var tag = t
  var relation = r
  var np = n
  var sentence = s
  override def toString: String = s"($tag, $relation, $np), $sentence"
}
