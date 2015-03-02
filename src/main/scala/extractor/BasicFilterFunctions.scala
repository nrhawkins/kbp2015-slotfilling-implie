package extractor

import edu.knowitall.tool.chunk.ChunkedToken

/**
 * Created by Gene on 3/1/2015.
 */
trait BasicFilterFunctions {
  def getTokens(line: String): Seq[ChunkedToken]

  val daysOfWeek = List("monday", "tuesday", "wednesday", "thursday", "friday",
    "saturday", "sunday")

  val punctPostags =
    List("#", "$", ".", ",", ":", "(", ")", "\"", "\'", "\'\'", "`", "``")

  private def firstNonPunct(src: String, relation: ImplicitRelation): Int = {
    val tokens = getTokens(src)

    for (i <- relation.np.beginWordIndex to relation.np.endWordIndex) {
      if (!punctPostags.contains(tokens(i))) {
        return i
      }
    }
    -1
  }

  private def lastNonPunct(src: String, relation: ImplicitRelation): Int = {
    val tokens = getTokens(src)

    for (i <- relation.np.endWordIndex to relation.np.beginWordIndex by -1) {
      if (!punctPostags.contains(tokens(i))) {
        return i
      }
    }
    -1
  }

  def filterNNOfEntities(src: String, relations: List[ImplicitRelation]) = {
    val tokens = getTokens(src)

    println("in filterNNofEntities")
    relations.filter(r => {
      val symbolIndex = firstNonPunct(src, r)
      val symbol = tokens(symbolIndex).postagSymbol.name

      println(tokens(r.np.beginWordIndex))
      println(tokens(r.np.endWordIndex))
      println(tokens(symbolIndex))
      println(tokens(symbolIndex + 1))
      println()

      // Can add NNP if we want to filter proper nouns as well.
      // Filter out if entity is 'NN/NNS of ...'
      symbolIndex != -1 &&
        ((!symbol.equals("NN") && !symbol.equals("NNS")) ||
          !tokens(symbolIndex + 1).string.equals("of"))
    })

  }

  def filterDaysOfWeek(src: String, relations: List[ImplicitRelation]) = {
    val tokens = getTokens(src)

    println("in filterDaysOfWeek")
    relations.filter(r => {
      val firstSymIndex = firstNonPunct(src, r)
      val lastSymIndex = lastNonPunct(src, r)
      println(tokens(r.np.beginWordIndex))
      println(tokens(r.np.endWordIndex))
      println(tokens(firstSymIndex))
      println(tokens(lastSymIndex))
      println()

      // Filter out any that have a day of the week as the first or last token.
      firstSymIndex != -1 && lastSymIndex != 1 &&
        (!daysOfWeek.contains(tokens(firstSymIndex).string.toLowerCase) &&
          !daysOfWeek.contains(tokens(lastSymIndex).string.toLowerCase))
    })
  }

  // This checks by the entire sentence, rather than the entity...
  // So it either returns Nil or an unchanged list.
  def filterStrangeDateFormats(src: String, relations: List[ImplicitRelation]) = {
    val indicators = List("UTC", "CNA")
    val tokens = getTokens(src)

    val containsIndicator = tokens.foldLeft(false)((acc, cur) => {
      indicators.contains(cur.string) || acc
    })

    if (containsIndicator) {
      Nil
    } else {
      relations
    }
  }

  def filterVsEntities(src: String, relations: List[ImplicitRelation]) = {
    val tokens = getTokens(src)

    val vs = List("vs", "vs.")

    relations.filter(r => {
      var containsVs = false
      for (i <- r.np.beginWordIndex to r.np.endWordIndex) {
        if (vs.contains(tokens(i).string.toLowerCase)) {
          containsVs = true
        }
      }
      containsVs
    })
  }
}
