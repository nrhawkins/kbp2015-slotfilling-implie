package testers

import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpHandler, HttpExchange, HttpServer}
import extractor.{ImplicitRelationExtractor, TaggerLoader}
import net.roeim.minihttpserver.MiniHttpServer
import utils.ExtractionFormatUtils

/**
 * Simple HTTP server that processes a sentence and
 * returns a verbose trace.
 */
object WebTesterServer {
  def main(args: Array[String]) {
    println("Setting up server...")
    val server = new WebTesterServer
    server.start()
    println("Server running...")
  }
}

class WebTesterServer extends MiniHttpServer {
  val tagger = TaggerLoader.defaultTagger
  val extractor = new ImplicitRelationExtractor(tagger)

  def brfy(s: String) = {
    val br = "<br>"
    val sp = s.replaceAll("\n", "<br>").replaceAll("\t", "    ")
    s"$br$sp$br$br"
  }

  get("/") { exchange =>
    exchange.getResponseHeaders.add("Content-type", "text/html")
    exchange.getResponseHeaders.add("Access-Control-Allow-Origin", "*")
    println("/ entered")
    val params = queryToMap(exchange.getRequestURI.getQuery)
    params.get("sentence") match {
      case None => "No sentence provided."
      case Some(sentence) =>
        val extractions = extractor.extractRelations(sentence)
        println("extracted")
        val tags = extractor.getTags(sentence)
        println("get tags")
        val verboseOut = ExtractionFormatUtils.verboseOutput(extractor)(sentence)
        println("got verbose output")
        val unfiltered = extractor.unfilteredExtractions(sentence)
        println("got unfiltered")

        val builder = StringBuilder.newBuilder

        val brsen = brfy(sentence)
        val brtags = brfy(tags.toString())
        val brparse = brfy(verboseOut)
        val brunfiltered = brfy(unfiltered.toString())
        val brfinal = brfy(extractions.toString())
        builder.append(s"Sentence:$brsen")
        builder.append(s"Tags:$brtags")
        builder.append(s"Dependency Parse:$brparse")
        builder.append(s"Unfiltered:$brunfiltered")
        builder.append(s"Final:$brfinal")
        builder.mkString
    }
  }

  get("/foo") { exchange =>
    exchange.getResponseHeaders.add("Content-type", "text/plain")
    println("/foo entered")
    "And here's foo."
  }

  /**
   * returns the url parameters in a map
   * @param query
   * @return map
   */
  def queryToMap(query: String) = {
    val result = new scala.collection.mutable.HashMap[String, String]()
    for (param <- query.split("&")) {
      val pair = param.split("=")
      if (pair.length > 1) {
        result.put(pair(0), pair(1))
      } else {
        result.put(pair(0), "")
      }
    }
    result
  }
}