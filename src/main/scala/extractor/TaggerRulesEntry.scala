package extractor

/**
 * Created by Gene on 4/23/2015.
 */
class TaggerRulesEntry(_name: String, _taggerIdentifier: String, _arguments: java.util.List[String]) extends Serializable {
  def name = _name
  def taggerIdentifier = _taggerIdentifier
  def arguments = _arguments
}
