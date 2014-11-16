package experimenting

import edu.knowitall.repr.sentence.{Chunked, Chunker, Lemmatized, Lemmatizer, Sentence}
import edu.knowitall.taggers.{ParseRule, TaggerCollection}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import extractor.NounToNounRelationExtractor

object Example {
  val pattern = """
Animal := CaseInsensitiveKeywordTagger {
classical singing teacher
classical singing teacher
cat
kitten
dog
red dog
I have a
have a red dog
cliff has a
Cliff has a
puppy
scientology
}
Color := NormalizedKeywordTagger{
blue
red
yellow
green
}
ColorfulAnimal := PatternTagger {
//namedGroup color will yield a Type object
//that is linked to the ColorfulAnimal Type object
(<color>:<type='Color'>) <type='Animal'>
}
ColorfulAnimalAction := TypePatternTagger{
//TypePatternTagger supports @ syntax to capture
// the entire Type
@ColorfulAnimal <pos='VBD'>
}
                """
  val input = """
I have a red dog, a
Cliff has a yellow puppy.
The yellow puppy ran.
Madame Violetta, a classical singing teacher, instructs a student on how to sing "Be-Bop-a-Lula," which is ultimately delivered in comic clucks and barks interspersed with coloratura flashes.
puppy dog cat kitten
"""
  val chunker = new OpenNlpChunker()
  def process(text: String): Sentence with Chunked with Lemmatized = {
    new Sentence(text) with Chunker with Lemmatizer {
      val chunker = Example.this.chunker
      val lemmatizer = MorphaStemmer
    }
  }
  def main(args: Array[String]){
    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(pattern).get
    val t = rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    val lines = input.split("\n").map(f => f.trim()).filter(f => f!= "").toList
    /*
    for (line <- lines){
      val types = t.tag(process(line)).toList
      println("Line: " + line)
      for(typ <- types){
        val interval = typ.tokenInterval
        println("TaggerName: " +typ.name + "\tTypeInterval: " + typ.tokenInterval + s"\tIntervalStart: ${interval.start}\tIntervalEnd: ${interval.end}" + "\t TypeText: " + typ.text)
      }
      //filter out the NamedGroupTypes
      for(typ <- types.filter(p => p.isInstanceOf[NamedGroupType])){
        val namedGroupType = typ.asInstanceOf[NamedGroupType]
        if(namedGroupType.groupName == "color"){
          println("COLOR:\t" + namedGroupType.text)
        }
      }
    }*/
    val extractor = new NounToNounRelationExtractor(t)
    val sentence = "The lawsuits alleged that Scientology churches around the world have " +
      "been bombarded with thousands of harassing phone calls, millions of malicious and obscene e-mails, " +
      "and bomb and death threats by members of Anonymous."
    extractor.extractRelations(sentence)
  }
}