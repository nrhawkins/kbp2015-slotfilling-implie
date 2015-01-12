package extractor

import java.io.Serializable

import edu.stanford.nlp.trees.{TypedDependency, Tree}

/**
 * Created by Gene on 1/11/2015.
 */
class ParseEntry(_sentence: String, _tree: Tree, _tdl: java.util.ArrayList[TypedDependency]) extends Serializable {
  def sentence = _sentence
  def tree = _tree
  def tdl = _tdl
}