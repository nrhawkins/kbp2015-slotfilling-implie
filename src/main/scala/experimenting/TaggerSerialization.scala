package experimenting

import java.io.{PrintWriter, BufferedWriter, FileWriter}

import com.typesafe.config.Config
import edu.knowitall.repr.sentence._
import edu.knowitall.taggers._
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import extractor.TaggerLoader
import utils.SerializationUtils

import scala.io.Source

/**
 * Created by Gene on 4/22/2015.
 */
object TaggerSerialization {
  val chunker = new OpenNlpChunker()
  def process(text: String): Sentence with Chunked with Lemmatized = {
    new Sentence(text) with Chunker with Lemmatizer {
      val chunker = TaggerSerialization.this.chunker
      val lemmatizer = MorphaStemmer
    }
  }
  def addSerializedTagger(file: String, tagger: TaggerCollection[Sentence with Chunked with Lemmatized]) {

    val defs = tagger.definitions
    val newdefs = defs.map(df => DefinitionRule(df.name, df.definition))
    val copy = newdefs.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }


    val out = new PrintWriter(new BufferedWriter(new FileWriter(file, true)))
    out.close()
  }

  def makeTaggerCopy(tagger: TaggerCollection[Sentence with Chunked with Lemmatized]) = {
    val defs = tagger.definitions
    val newdefs = defs.map(df => DefinitionRule(df.name, df.definition))
    newdefs.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
  }

  def createTaggerFromConfig(conf: Config) = {
    val rules = TaggerLoader.taggerRuleMemo.getOrElse(conf.hashCode(), List())
    rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
  }

  def createTaggerFromRuleCopy(conf: Config) = {
    val rules = TaggerLoader.taggerRuleMemo.getOrElse(conf.hashCode(), List())

    rules(0) match {
      case tr: TaggerRule[Sentence with Chunked with Lemmatized] =>
        println("TaggerRule")
        println(s"Tagger ID: ${tr.taggerIdentifier}")
        println(s"name: ${tr.name}")
        println(s"constraints: ${tr.constraints}")
        println(s"arguments(0): ${tr.arguments(0)}")
      case dr: DefinitionRule[Sentence with Chunked with Lemmatized] =>
        println("Definition Rule")
      case _ =>
        println("None.......")
    }
    val taggerRules = rules.map(_.asInstanceOf[TaggerRule[Sentence with Chunked with Lemmatized]])
    val stringForm = taggerRules.map(rule => (rule.name, rule.taggerIdentifier, Seq(), rule.arguments))

    val reconstructed = stringForm.map(tuple => {
        val (name, id, const, args) = tuple
        TaggerRule[Sentence with Chunked with Lemmatized](name, id, const, args)})
    reconstructed.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    //    val rulecpy: Seq[Rule[Sentence with Chunked with Lemmatized]] =
//      rules.map(rule => DefinitionRule[Sentence with Chunked with Lemmatized](rule.name, rule.definition))
//    rulecpy.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
  }

  def storeTagger(conf: Config) {
    val rules = TaggerLoader.taggerRuleMemo.getOrElse(conf.hashCode(), List())
    val taggerRules = rules.map(_.asInstanceOf[TaggerRule[Sentence with Chunked with Lemmatized]])
    SerializationUtils.saveSerializedTaggerRules("cache/taggers/experimental.ser", taggerRules)
  }

  def main(args: Array[String]) {
    val tagger = TaggerLoader.basicTestTagger
    val conf = TaggerLoader.basic_test_tagger_config

    print("Loading tagger...")
    val start = System.nanoTime()
    val copy = SerializationUtils.loadSerializedTaggerCollection("cache/taggers/experimental.ser")
//    val copy = createTaggerFromConfig(conf)
//    val copy = createTaggerFromRuleCopy(conf)
    val end = System.nanoTime()
    val diff = (end - start).toDouble / 1000000000
    println(f"[$diff%.3f sec]")


    // Test that they both work.
    val lines = Source.fromFile("test_sentences.txt").getLines()
    for (line <- lines) {
      println("Line: " + line)
      val type1 = tagging(tagger, line)
      val type2 = tagging(copy, line)
      println(tagger)
      println(copy)
      for ((typ1, typ2) <- type1.zip(type2)) {
        println(typ1.name.equals(typ2.name))
        println(typ1.text.equals(typ2.text))
        println(typ1.tokenInterval.equals(typ2.tokenInterval))
      }
    }
    // Save tagger.
//    storeTagger(conf)
    print("Done.")
  }

  def tagging(t: TaggerCollection[Sentence with Chunked with Lemmatized], line: String) = {
    val types = t.tag(process(line)).toList
    for(typ <- types){
      val interval = typ.tokenInterval
      println("TaggerName: " +typ.name + "\tTypeInterval: " + typ.tokenInterval + s"\tIntervalStart: ${interval.start}\tIntervalEnd: ${interval.end}" + "\t TypeText: " + typ.text)
    }
    //filter out the NamedGroupTypes
    for(typ <- types.filter(p => p.isInstanceOf[NamedGroupType])){
      val namedGroupType = typ.asInstanceOf[NamedGroupType]
      if(namedGroupType.groupName == "color"){
        println("COLOR:\t" + namedGroupType.text)
      }
    }
    types
  }
}
