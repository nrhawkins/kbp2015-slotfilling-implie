package testers

import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.collection.mutable.HashMap

/**
 * Handles server configuration details, starting and stopping
 */
abstract class SimpleHttpServerBase(val socketAddress: String = "127.0.0.1",
                                    val port: Int = 8080,
                                    val backlog: Int = 0) extends HttpHandler {
  private val address = new InetSocketAddress(socketAddress, port)
  private val server = HttpServer.create(address, backlog)
  server.createContext("/", this)

  /**
   * Sends a HTTP response with the specified HTTP response code and string as content
   */
  def respond(exchange: HttpExchange, code: Int = 200, body: String = "") {
    val bytes = body.getBytes
    exchange.sendResponseHeaders(code, bytes.size)
    exchange.getResponseBody.write(bytes)
    exchange.getResponseBody.write("\r\n\r\n".getBytes)
    exchange.getResponseBody.close()
    exchange.close()
  }

  def start() = server.start()

  def stop(delay: Int = 1) = server.stop(delay)
}

/**
 * Identical to vetler's MiniHttpServer except for class arguments:
 * https://github.com/vetler/minihttpserver/blob/master/src/main/scala/net/roeim/minihttpserver/MiniHttpServer.scala
 *
 * Allows the server to have configurations besides socket address 127.0.0.1,
 * port 8080, and backlog 0.  The defaults are the same as the values used
 * by MiniHttpServer.
 */
abstract class SimpleConfigServer
    (override val socketAddress: String = "127.0.0.1",
     override val port: Int = 8080,
     override val backlog: Int = 0)
    extends SimpleHttpServerBase(socketAddress, port, backlog) {

  // Contains mappings from URL paths to functions serving the page content
  private val mappings = new HashMap[String, (HttpExchange) => Any]

  /**
   * Set a function to serve a specific URL path. The following example will set
   * the path /monkey to return the content "foo":
   * <code>
   * get("/monkey") ((exchange: HttpExchange) => { "foo" }
   * </code>
   */
  def get(path: String)(action: HttpExchange => Any) = mappings += path -> action

  /**
   * Handle a HTTP request
   */
  def handle(exchange: HttpExchange) = mappings.get(exchange.getRequestURI.getPath) match {
    case None => respond(exchange, 404)
    case Some(action) => try {
      respond(exchange, 200, action(exchange).toString)
    } catch {
      case ex: Exception => respond(exchange, 500, ex.toString)
    }
  }
}
