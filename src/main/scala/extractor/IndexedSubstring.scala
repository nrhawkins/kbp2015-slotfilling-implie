package extractor

/**
 * Created by Gene on 1/2/2015.
 */
class IndexedSubstring(s: String, bWordIndex: Int, eWordIndex: Int, bOffset: Int, eOffset: Int)
    extends IndexedString(s, eWordIndex) {
  private var sourceSentence: String = null

  def this(s: String, bWordIndex: Int, eWordIndex: Int, bOffset: Int, eOffset: Int, src: String) = {
    this(s, bWordIndex, eWordIndex, bOffset, eOffset)
    sourceSentence = src
  }

  // Word offsets.  Both inclusive.
  def beginWordIndex = bWordIndex
  def endWordIndex = eWordIndex
  // Substring offsets.  Begin is inclusive, end is exclusive.
  def beginOffset = bOffset
  def endOffset = eOffset
  def source = sourceSentence

  def setSource(newSource: String) {
    sourceSentence = newSource
  }
}

object IndexedSubstring {
  def emptyInstance = new IndexedSubstring("", -1, -1, -1, -1)
}
