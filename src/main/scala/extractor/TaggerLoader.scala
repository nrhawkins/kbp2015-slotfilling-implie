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
 * Created by Gene on 12/21/2014.
 */
class TaggerLoader {

  // Create functions that are more general.  Uses args rather than config
  // to load tagger.
}

// TODO: reorganize so that the defaults are in one place and the functions
// can take arguments instead.
object TaggerLoader {
  val default_config = ConfigFactory.load("default-tagger.conf")
  case class TagClass(name: String, files: List[String])
  case class TaggerResult(tags: List[Type], source: String)

  val chunker = new OpenNlpChunker()
  val defaultTagger = getTagger

  // TODO: put default values here
  val DEFAULT_TAGGER_TYPE = default_config.getString("tagger-type")


  def getClasses: List[TagClass] = {
    val classes: List[Config] = default_config.getConfigList("classes").toList
    classes.map(c => TagClass(c.getString("name"), c.getStringList("files").toList))
  }

  private def taggerFunction(line: String): TaggerResult = {
    def process(text: String): Sentence with Chunked with Lemmatized = {
      new Sentence(text) with Chunker with Lemmatizer {
        val chunker = TaggerLoader.chunker
        val lemmatizer = MorphaStemmer
      }
    }
    TaggerResult(defaultTagger.tag(process(line)).toList, line)
  }

  private def getTagger: TaggerCollection[Sentence with Chunked with Lemmatized] = {
    /**
     * Builds string with the definitions of class term relation for tagger.
     * @param classes List of class to term list mappings.
     * @return String definitions of each class.
     */
    def createTaggerDefinition(classes: List[TagClass]): String = {
      val builder = StringBuilder.newBuilder
      for (clas <- classes) {
        val taggerType = default_config.getString("tagger-type")

        builder.append(s"${clas.name} := $taggerType {\n")
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
    val taggerPattern = createTaggerDefinition(getClasses)

    // Setup structures for representing data.
    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(taggerPattern).get
    rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
  }
}