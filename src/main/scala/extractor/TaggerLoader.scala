package extractor

import com.typesafe.config.{ConfigFactory, Config}
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers.{ParseRule, TaggerCollection}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type

import scala.collection.JavaConversions._
import scala.io.Source

/**
 * Object for loading taggers from configurations.
 * There are pre-specified tagger configurations for most needs as well as
 * a buildTagger function that takes a Config for a tagger.
 */
object TaggerLoader {
  case class TagClass(name: String, taggerType: String, files: List[String])
  case class TaggerResult(tags: List[Type], source: String)

  val default_tagger_config = ConfigFactory.load("taggers/default-extractor-tagger.conf")
  val basic_test_tagger_config = ConfigFactory.load("taggers/basic-test-tagger.conf")

  val chunker = new OpenNlpChunker()

  val taggerMemo = scala.collection.mutable.Map
    [String, TaggerCollection[Sentence with Chunked with Lemmatized]]()
  def memoizedTagger(key: String, tagger_config: Config)() = {
    taggerMemo.get(key) match {
      case None =>
        val result = buildTagger(tagger_config)
        taggerMemo.put(key, result)
        result
      case Some(result) => result
    }
  }

  def defaultTagger = memoizedTagger("default", default_tagger_config)
  def basicTestTagger = memoizedTagger("basic_test", basic_test_tagger_config)

  /**
   * Constructs a tagger from a tagger configuration.
   * @param config Configuration for the tagger.
   * @return TaggerCollection (tagger).
   */
  def buildTagger(config: Config): TaggerCollection[Sentence with Chunked with Lemmatized] = {
    /**
     * Builds string with the definitions of class term relation for tagger.
     * @param classes List of class to term list mappings.
     * @return String definitions of each class.
     */
    def createTaggerDefinition(classes: List[TagClass]): String = {
      val builder = StringBuilder.newBuilder
      for (clas <- classes) {
        builder.append(s"${clas.name} := ${clas.taggerType} {\n")
        for (file <- clas.files) {
          val lines = Source.fromFile(file).getLines()
          for (line <- lines) {
            if (line.trim.length != 0) {
              builder.append(line.trim.toLowerCase).append("\n")
            }
          }
        }
        builder.append("}\n")
      }
      builder.mkString
    }

    def getClasses: List[TagClass] = {
      val classes: List[Config] = config.getConfigList("classes").toList
      classes.map(c => TagClass(c.getString("name"), c.getString("tagger-type"),
        c.getStringList("files").toList))
    }

    val taggerPattern = createTaggerDefinition(getClasses)

    // Setup structures for representing data.
    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(taggerPattern).get
    rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
  }

  private def taggerFunction
    (tagger: TaggerCollection[Sentence with Chunked with Lemmatized])
    (line: String): TaggerResult = {
    def process(text: String): Sentence with Chunked with Lemmatized = {
      new Sentence(text) with Chunker with Lemmatizer {
        val chunker = TaggerLoader.chunker
        val lemmatizer = MorphaStemmer
      }
    }
    TaggerResult(tagger.tag(process(line)).toList, line)
  }
}