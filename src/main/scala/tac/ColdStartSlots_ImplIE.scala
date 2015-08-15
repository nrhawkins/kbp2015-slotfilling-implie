package tac

object ColdStartSlots_ImplIE {
  
  private val coldStartSlotsResourcePath = "/tac/ColdStartSlotTypes_implie.txt"
  
  private val coldStartSlotsURL = getClass().getResource(coldStartSlotsResourcePath)
  require(coldStartSlotsURL != null, "Could not find resource: " + coldStartSlotsResourcePath)

    
  private val slotsSet =  scala.collection.mutable.Set[String]()

  // read in document list lines with latin encoding so as not to get errors.
  scala.io.Source.fromFile(coldStartSlotsURL.getPath())(scala.io.Codec.ISO8859).getLines.foreach(line => {
    
      val name = line.trim
      
      if(!slotsSet.contains(name)) slotsSet.add(name)
        
    })
    
  lazy val slots = slotsSet.toSet
  
}