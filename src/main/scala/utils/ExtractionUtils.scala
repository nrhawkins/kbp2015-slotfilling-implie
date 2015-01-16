package utils

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.typer.Type
import extractor.TagInfo

/**
 * General utility functions for extractions.
 */
object ExtractionUtils {
  // Pre: each index can have at most 1 tag
  // Creates a mapping from the token index to the TagInfo.
  def createTagMap(tags: List[Type]): Map[Int, TagInfo] = {
    def tagMapHelper (acc: Map[Int, TagInfo], tags: List[Type]): Map[Int, TagInfo] = {
      tags match {
        case Nil => acc
        case (tag :: tail) =>
          val newacc = acc + ((tag.tokenInterval.end, new TagInfo(tag)))
          tagMapHelper(newacc, tail)
      }
    }
    tagMapHelper(Map(), tags)
  }

// Extracts a substring from token/word indices rather than character indices.
  def substringFromWordIndicies(
    string: String,
    tokens: Seq[ChunkedToken],
    beginIndex: Int,
    endIndex: Int): String = {

    string.substring(tokens(beginIndex).offset,
      tokens(endIndex).offset + tokens(endIndex).string.length)
  }
}
