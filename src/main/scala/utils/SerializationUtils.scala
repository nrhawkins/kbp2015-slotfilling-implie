package utils

import java.io._

import edu.knowitall.repr.sentence.{Sentence, Chunked, Lemmatized}
import edu.knowitall.taggers.{TaggerCollection, TaggerRule}
import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.trees.{Tree, TypedDependency}
import extractor.{TaggerRulesEntry, ParseEntry}

import scala.collection.JavaConversions._
import scala.io.Source

/**
 * Created by Gene on 1/11/2015.
 */
object SerializationUtils {
  def addSerializedObject[T <: Serializable](file: String, obj: T) {
    addSerializedObjects(file, obj::Nil)
  }

  def addSerializedObjects(file: String, objs: List[_ <: Serializable]) {
    val out = outputStream(file)
    objs.foreach(out.writeObject(_))
    out.close()
  }


  def getSerializedObjects[T <: Serializable](file: String): List[T] = {
    if (!new java.io.File(file).exists) {
      return Nil
    }

    var results = scala.collection.mutable.MutableList[T]()

    val fileIn = new FileInputStream(file)
    val in = try {
      new ObjectInputStream(fileIn)
    } catch {
      case eof: EOFException =>
        return Nil
    }

    try {
      while(true) {
        results += in.readObject().asInstanceOf[T]
      }
    } catch {
      case exception: EOFException =>
        fileIn.close()
        in.close()
    }
    results.toList
  }

  def addSerializedChunkedSentence(
    file: String, sentence: String, tokens: Seq[ChunkedToken]) {

    addSerializedChunkedSentences(file, (sentence, tokens)::Nil)
  }

  def addSerializedChunkedSentences(
    file: String, sentences: List[(String, Seq[ChunkedToken])]) {

    val out = new PrintWriter(new BufferedWriter(new FileWriter(file, true)))
    sentences.foreach(sentenceData => {
      val (sentence, tokens) = sentenceData
      val serials = tokens.map(ChunkedToken.stringFormat.write)
      val serializedSentence = (sentence /: serials)(_ + "\t" + _)
      out.println(serializedSentence)
    })
    out.close()
  }

  def getSerializedChunkedSentences(file: String): List[(String, Seq[ChunkedToken])] = {
    if (new java.io.File(file).exists) {
      Source.fromFile(file).getLines().map(line => {
        val tokens = line.trim.split("\t").toSeq
        (tokens.head, tokens.tail.map(ChunkedToken.stringFormat.read))
      }).toList
    } else {
      Nil
    }
  }

  /*
   * Serializes the tagger rules given and stores it in the given file.
   * ImplIE doesn't use constraints on the tags so they are ignored.
   * Upon loading, constraints should be initialized to and empty Sequence.
   */
  def saveSerializedTaggerRules(
    file: String,
    rules: Seq[TaggerRule[Sentence with Chunked with Lemmatized]],
    overwrite: Boolean = false) {

    val out = outputStream(file, appending = false, overwrite = overwrite)
    val ruleStrings = rules.map(rule => new TaggerRulesEntry(rule.name, rule.taggerIdentifier, rule.arguments.toList))
    out.writeObject(new java.util.ArrayList(ruleStrings.toList))
    out.close()
  }

  def loadSerializedTaggerCollection(file: String):
      TaggerCollection[Sentence with Chunked with Lemmatized] = {
    print("Loading serialized tagger collection...")
    val start = System.nanoTime()
    val result = getSerializedObjects[java.util.ArrayList[TaggerRulesEntry]](file)

    val tagger = result match {
      case Nil =>
        print(s"NO TAGGER COLLECTION IN FILE $file...")
        null
      case x::xs =>
        // Just take the first one.
        val reconstructedRules = x.map(r =>
          TaggerRule[Sentence with Chunked with Lemmatized](r.name, r.taggerIdentifier, Seq(), r.arguments))
        reconstructedRules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    }
    val end = System.nanoTime()
    val diff = (end - start).toDouble / 1000000000
    println(f"[$diff%.3f sec]")
    tagger
  }

  def loadSerializedTokenizedSentences(file: String): Map[String, Seq[ChunkedToken]] = {
    print("Loading serialized tokenized sentences...")
    val start = System.nanoTime()
    val result = getSerializedChunkedSentences(file).toMap
    val end = System.nanoTime()
    val diff = (end - start).toDouble / 1000000000
    println(f"[$diff%.3f sec]")
    result
  }

  def loadSerializedParses(file: String): Map[String, (Tree, List[TypedDependency])] = {
    print("Loading serialized parses...")
    val start = System.nanoTime()
    val result = getSerializedObjects[ParseEntry](file).map(pe => (pe.sentence, (pe.tree, pe.tdl.toList))).toMap
    val end = System.nanoTime()
    val diff = (end - start).toDouble / 1000000000
    println(f"[$diff%.3f sec]")
    result
  }

  /*
   * Creates an output stream with given flags.  Returns null if overwrite and
   * appending are set to false and the file already exists.
   */
  def outputStream(file: String, appending: Boolean = true, overwrite: Boolean = false): ObjectOutputStream = {
    val exists = new java.io.File(file).exists
    if (appending && exists) {
      new AppendingObjectOutputStream(new FileOutputStream(file, true))
    } else if (overwrite || !exists) {
      // Create file first.
      val writer = new PrintWriter(file)
      writer.close()
      new ObjectOutputStream(new FileOutputStream(file))
    } else {
      println("Could not create output stream, file exists and flag set to not overwrite")
      null
    }
  }
}
