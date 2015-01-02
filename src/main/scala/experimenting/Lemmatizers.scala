package experimenting

import edu.knowitall.tool.stem.MorphaStemmer

/**
 * Created by Gene on 12/31/2014.
 */
object Lemmatizers {

  val keywords = List[String](
    "classically singing teacher",
    "cat",
    "kitten",
    "dog",
    "red dog",
    "I have a",
    "have a red dog",
    "cliff has a",
    "Cliff has a",
    "puppy",
    "scientology",
    "Hinduism",
    "Hindu",
    "Zimbabwean",
    "American",
    "Americans",
    "Seattlites",
    "Seattlite",
    "happiness"
    )

  def main(args: Array[String]): Unit = {
    val morpha = MorphaStemmer
    println("lemmatized")
    // lemmatized
    for (keyword <- keywords) {
      println(keyword.split(" +").foldLeft("")((acc, cur) =>
        acc + " " + morpha.lemmatize(cur)).trim)
    }

    println()
    println("stemmed")
    // stemmed
    for (keyword <- keywords) {
      println(keyword.split(" +").foldLeft("")((acc, cur) =>
        acc + " " + morpha.stem(cur)).trim)
    }
  }
}
