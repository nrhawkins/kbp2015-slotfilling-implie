package extractor

/**
 * Created by Gene on 11/21/2014.
 */
class IndexedString(s: String, i: Int) {
  def string = s
  def index = i

  override def toString(): String = {
    s"$string-$index"
  }

  override def hashCode(): Int = string.hashCode() + index.hashCode()

  override def equals(other: Any): Boolean = other match {
    case o: IndexedString => o.index.equals(this.index)
    case _ => false
  }
}
