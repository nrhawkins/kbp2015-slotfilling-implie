package tac

object SelectBestAnswers {
  
  /*def deduplicate(candidates: Seq[Candidate]) = {
    candidates.groupBy(_.deduplicationKey).map {
      case (key, duplicates) =>
        duplicates.maxBy(_.extr.confidence)
    } toSeq
  }
 */    
  
  //var numDupesTrainingSentences = collection.mutable.Map[TrainingSentence, Int]().withDefaultValue(0)
  //  for(ts <- uniqueTrainingSentences){
  //    numDupesTrainingSentences.update(ts, numDupesTrainingSentences(ts))
  //}
  //val relationId = TrainingSentence(sentId,arg1,arg2)
  //if(numDupesTrainingSentences.contains(relationId)){
  //  numDupesTrainingSentences.update(relationId, numDupesTrainingSentences(relationId) + 1)
  //}
  
  
  def reduceToMaxResults(slot :Slot, candidates :Seq[Candidate]):Seq[Candidate] = {
    
    //println("candidates size: " + candidates.size)
    
    if(candidates.size > 1) {

     //---------------------------
     // Make Candidate Map
     //--------------------------- 
     // key = arg2, value = count  
     var slotfillFreq = collection.mutable.Map[String, Int]().withDefaultValue(0)
     for(candidate <- candidates){
       val arg2 = candidate.extr.getArg2().argName
       //update does not require an "if contains"
       slotfillFreq.update(arg2, slotfillFreq(arg2) + 1)          
     }

     //println("candidate map size: " + slotfillFreq.size)

     candidates.foreach(c => c.extr.setScore(slotfillFreq(c.extr.getArg2().argName)))

     //println("sorting candidates")
     
     val sortedCandidates = candidates.sortBy(candidate => candidate.extr.getScore()).reverse      

     //println("slot max results: " + slot.maxResults)
     
     slot.maxResults match {
       
       case 1 => Seq(sortedCandidates.head) 
       //case 1 => Seq(candidates.head) 
       
       case 9 => { 
         
         //Seq(candidates.head)

         //put first candidate in these
         var candidateSeq = Seq(sortedCandidates.head)
         var slotfillSet = Set(sortedCandidates.head.extr.getArg2().argName)
         //add the rest of the candidates
         var i = 1
         while(i < sortedCandidates.size){

           val arg2 = sortedCandidates(i).extr.getArg2().argName
           if(!slotfillSet.contains(arg2)){
             slotfillSet = slotfillSet ++ Set(arg2)
             candidateSeq = candidateSeq ++ Seq(sortedCandidates(i))
           }                     
           i += 1
         } 
         candidateSeq
         //Seq(sortedCandidates.head) 
         /*val candidatesMap = candidates map 
           { candidate => (candidate.extr.getArg2().getArgName() , 
                           candidates.filter(_.extr.getArg2().getArgName()
                        == candidate.extr.getArg2().getArgName()).sortBy(candidate => candidate.extr.getScore()).reverse.head )
           }  toMap	
                        
                           
         // Make a map of unique slot fills from the list of candidates
         //val candidatesMap = candidates map { candidate => (candidate.extr.getArg2().getArgName(), candidate) } toMap 

         // Ensure that of the duplicates, if any, the one with highest confidence score is in the map
         //candidates.foreach(candidate => if(candidate.extr.getScore() > candidatesMap(candidate.extr.getArg2().getArgName()).extr.getScore()) 
         //  candidatesMap + candidate.extr.getArg2().getArgName() -> candidate
         //)
         
         // ToDo: Still need to mergeNames (like VA and Virginia, Bill Clinton and William Jefferson Clinton)
         if(candidatesMap.values.toSeq.size > 9) candidatesMap.values.toSeq.take(9)
         else candidatesMap.values.toSeq
       */
       }
       
       // Could be a case = 0?
       case _ => Seq(sortedCandidates.head) 
       //case _ => Seq(candidates.head) 
       
     }
       
    }
    else{
   
      candidates
       
    }   
    
  }
  
  
}