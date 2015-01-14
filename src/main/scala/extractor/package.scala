import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.trees.{Tree, TypedDependency}

import scala.collection.mutable

/**
 * Created by Gene on 1/12/2015.
 */
package object extractor {
  case class Rule(rel: String, gov: String, dep: String)
  case class NounToNounTDL(tdl: List[TypedDependency], tag: ImplicitRelation)
  case class EnclosingPunctuation(open: String, close: String)

  type Phrase = String
  type TagName = String
  type RelationPattern = Map[String, List[Rule]]
  type IDTable = mutable.Map[String, Set[IndexedString]]
  type ExpansionFunction = (TypedDependency, Rule) => (TypedDependency, String, IndexedString)
  type EntityExtractionFunction = (Tree, List[TypedDependency], TagInfo,
    Seq[ChunkedToken], String, ImplicitRelationExtractor) =>
      (IndexedSubstring)
}
