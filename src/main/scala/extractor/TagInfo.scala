package extractor

import edu.knowitall.tool.typer.Type

/**
 * Created by Gene on 12/8/2014.
 */
class TagInfo(tag_ : String, text_ : String, index_ : Int) {
  def tag = tag_
  def text = text_
  def index = index_

  def this(tag_ : String, iStr : IndexedString) = this(tag_, iStr.string, iStr.index)
  def this(typ: Type) = this(typ.name, typ.text, typ.tokenInterval.end)

  def asIndexedString = new IndexedString(text, index)

  override def toString(): String = {
    s"$tag: $text-$index"
  }

  override def hashCode(): Int = tag.hashCode() + text.hashCode() + index.hashCode()

  override def equals(other: Any): Boolean = other match {
    case o: TagInfo =>
      o.tag.equals(this.tag) &&
      o.text.equals(this.text) &&
      o.index.equals(this.index)
    case _ => false
  }
}