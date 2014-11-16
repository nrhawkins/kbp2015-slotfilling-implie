package testers

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._

/**
 * Created by Gene on 11/16/2014.
 */
object ModularTaggerTester {

  // TODO: put inputs in configuration file
  type Tags = Set[(String, Int)]

  def main(args: Array[String]) {
    val config = ConfigFactory.load("tagger-tester.conf")
    println(config.getString("input-file"))
    val classes: List[Config] = config.getConfigList("classes").toList
    for (classdef: Config <- classes) {
      println(classdef.getString("relation"))
      val files = classdef.getStringList("files")
      for (file <- files) {
        println(file)
      }
    }
  }
}
