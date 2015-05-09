package tac

import scala.collection.mutable
import scala.io.Source

/**
 * Maps alternate forms countries and nations to the basic country name.
 */
class SingularCountryMapper(
    mapperFile: String = "classTermLists/tac_single_nationality/singular_country_mapping",
    countryListFile: String = "classTermLists/capitalize/capitalized_country.txt") {
  private val countryMap = constructMap(mapperFile, false)
  private val lowercaseCountryMap = constructMap(mapperFile, true)
  private val countryList = constructCountryList(countryListFile)
  private val lowercaseCountryList = constructCountryList(countryListFile, true)


  // Return null if not found.
  def getCountryName(alternateForm: String, lowercase: Boolean = false, useList: Boolean = true): String = {
    val (list, key) = if (lowercase) {
      (lowercaseCountryList, alternateForm.toLowerCase)
    } else {
      (countryList, alternateForm)
    }

    val mappedName = getMappedCountryName(alternateForm, lowercase)

    // If mapped name is found, return.
    // Otherwise, check if the name is in the larger country name list and
    // return if found.
    // If not found, return null.
    if (useList && mappedName == null) {
      if (list.contains(key)) {
        key
      } else {
        null
      }
    } else {
      mappedName
    }
  }

  def getMappedCountryName(alternateForm: String, lowercase: Boolean = false): String = {
    val (map, key) = if (lowercase) {
      (lowercaseCountryMap, alternateForm.toLowerCase)
    } else {
      (countryMap, alternateForm)
    }
    map.getOrElse(key, null)
  }

  private def constructCountryList(filename: String, lowercase: Boolean = false) = {
    val rawlines = Source.fromFile(filename).getLines()
    val lines = if (lowercase) {
      rawlines.map(line => line.toLowerCase)
    } else { rawlines }

    val listbuf = new mutable.ListBuffer[String]()
    lines.filter(line => line.trim() != "").foreach(line => listbuf += line.trim())
    listbuf.toList
  }

  /**
   * Construct a map that maps from alternate forms of country names to the
   * normal country name.  It is constructed from a file with country name to
   * alternate form mapping.
   *
   * The file must be organized so that each line is a country, and the country
   * name is separate from alternate forms by a tab.  The alternate forms are
   * comma separated.
   *
   * NOTE: some alternate names be the same for multiple country names.  In
   *      those instances the one that returns is random.
   */
  private def constructMap(filename: String, lowercase: Boolean = false) = {
    val rawlines = Source.fromFile(filename).getLines()
    val lines = if (lowercase) {
      rawlines.map(line => line.toLowerCase)
    } else { rawlines }

    val countryData = lines.filter(line => line.trim() != "").map(line => {
      val tokens = line.split("\t")

      val (countryName, alternates) = if (tokens.length < 2) {
        (tokens(0), "")
      } else {
        (tokens(0), tokens(1))
      }
      val alternateList = alternates.split(',')
      (countryName, alternateList)
    })

    val map = mutable.Map[String, String]()
    countryData.foreach{ case (countryName, alternateList) => {
      // Remove duplicates.
      val uniqueList = alternateList.toSet.toList.sorted
      uniqueList.foreach(alternate =>
        map.put(alternate, countryName)
      )
      // Also put in the country name to itself for identity.
      map.put(countryName, countryName)
    }}
    // Return an immutable copy.
    map.toMap
  }
}
