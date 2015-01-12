package extractor

import java.io._

import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.trees.{Tree, TypedDependency}

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

  def loadSerializedTokenizedSentences(file: String): Map[String, Seq[ChunkedToken]] = {
    getSerializedChunkedSentences(file).toMap
  }

  def loadSerializedParses(file: String): Map[String, (Tree, List[TypedDependency])] = {
    getSerializedObjects[ParseEntry](file).map(pe => (pe.sentence, (pe.tree, pe.tdl.toList))).toMap
  }

  def outputStream(file: String): ObjectOutputStream = {
    if (new java.io.File(file).exists) {
      new AppendingObjectOutputStream(new FileOutputStream(file, true))
    } else {
      // Create file first.
      val writer = new PrintWriter(file)
      writer.close()
      new ObjectOutputStream(new FileOutputStream(file))
    }
  }
}
