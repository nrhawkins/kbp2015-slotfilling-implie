import java.util
import java.io.StringReader
import edu.stanford.nlp.process.Tokenizer
import edu.stanford.nlp.process.TokenizerFactory
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.process.DocumentPreprocessor
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.ling.{Word, CoreLabel, HasWord, Sentence}
import edu.stanford.nlp.trees._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import scala.collection.JavaConversions._


object JavaParserDemo {
  val PARSER_MODEL = "models/englishPCFG.ser.gz"

  /**
   * The main method demonstrates the easiest way to load a parser.
   * Simply call loadModel and specify the path of a serialized grammar
   * model, which can be a file, a resource on the classpath, or even a URL.
   * For example, this demonstrates loading from the models jar file, which
   * you therefore need to include in the classpath for ParserDemo to work.
   */
  def main(args: Array[String]) {
    val lp: LexicalizedParser = LexicalizedParser.loadModel(PARSER_MODEL)
//      demoDP(lp, "devSet/tagger/sentences.txt")
    demoPrints(lp)
//      demoAPI(lp)
    }

  /**
   * demoDP demonstrates turning a file into tokens and then parse
   * trees.  Note that the trees are printed by calling pennPrint on
   * the Tree object.  It is also possible to pass a PrintWriter to
   * pennPrint if you want to capture the output.
   */
  def demoDP(lp: LexicalizedParser, filename: String) {
    val tlp: TreebankLanguagePack = new PennTreebankLanguagePack
    val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory
    for (sentence <- new DocumentPreprocessor(filename)) {
      val parse: Tree = lp.apply(sentence)
      parse.pennPrint()
      println()
      val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parse)
      val tdl: util.Collection[_] = gs.typedDependenciesCCprocessed
      println(tdl)
      println()
    }
  }

  def demoPrints(lp: LexicalizedParser) {
    val sent: scala.collection.immutable.List[Word] = List("This", "is", "an", "easy", "sentence", ".").toList.map(a => new Word(a))
    val rawWords: util.List[CoreLabel] = Sentence.toCoreLabelList(sent)
    var parse: Tree = lp.apply(rawWords)
    println("Printing pennPrint")
    parse.pennPrint()
    println()

    val tlp: TreebankLanguagePack = new PennTreebankLanguagePack
    val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory
    val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parse)
    val tdl: util.List[TypedDependency] = gs.typedDependenciesCCprocessed
    println("Printing tdl")
    println(tdl)
    println()

    for (td <- tdl) {
      println(s"reln name: ${td.reln().getShortName}")
      println(s"dev: ${td.dep()}, gov: ${td.gov}")
    }

    val tp: TreePrint = new TreePrint("penn,typedDependenciesCollapsed")
    println("Printing tp.printTree")
    tp.printTree(parse)
  }

  /**
   * demoAPI demonstrates other ways of calling the parser with
   * already tokenized text, or in some cases, raw text that needs to
   * be tokenized as a single sentence.  Output is handled with a
   * TreePrint object.  Note that the options used when creating the
   * TreePrint can determine what results to print out.  Once again,
   * one can capture the output by passing a PrintWriter to
   * TreePrint.printTree.
   */
  def demoAPI(lp: LexicalizedParser) {
    val sent: scala.collection.immutable.List[Word] = List("This", "is", "an", "easy", "sentence", ".").toList.map(a => new Word(a))
    val rawWords: util.List[CoreLabel] = Sentence.toCoreLabelList(sent)
    var parse: Tree = lp.apply(rawWords)
    parse.pennPrint()
    println()
//    val sent2: String = "This is another sentence."
/*
    val sent2 =  "Worthy of special mention: At one point, the daughter ventures to Burkina Faso's capital, " +
  "Ouagadougou, and enters a shelter that houses older women suspected of being witches; " +
  "the camera lingers on the discarded women and their environs as haunting music plays in the background -- " +
  "a long and stirring scene that, for sheer length and v    isceral power, is reminiscent of the " +
  "climax in Michelangelo Antonioni's \"The Passenger.\""
*/
  /*  val sent2 = "Jiri Pehe, a prominent Czech sociologist and director of New York " +
  "University in Prague, said the accusations had polarized Czech society into " +
  "two camps: those who believe that Kundera may have been guilty, " +
  "but also see him as a moral scapegoat for their own collaboration with the " +
  "Communists; and those who view the article as an act of Czech treachery motivated by petty jealousy."*/
  /*val sent2 = "It can't be coincidence that \"The Lives of Others,\" Florian Henckel von Donnersmarck's Oscar-nominated " +
  "drama set in the culture of government " +
  "intimidation and social coercion in the former GDR (Communist East Germany), is set in the year 1984."
*/
    val sent2 = "The lawsuits alleged that Scientology churches around the world have " +
    "been bombarded with thousands of harassing phone calls, millions of malicious and obscene e-mails, " +
    "and bomb and death threats by members of Anonymous."
    val tokenizerFactory: TokenizerFactory[CoreLabel] = PTBTokenizer.factory(new CoreLabelTokenFactory, "")
    val tok: Tokenizer[CoreLabel] = tokenizerFactory.getTokenizer(new StringReader(sent2))
    val rawWords2: util.List[CoreLabel] = tok.tokenize
    parse = lp.apply(rawWords2)
    val tlp: TreebankLanguagePack = new PennTreebankLanguagePack
    val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory
    val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parse)
    val tdl: util.List[TypedDependency] = gs.typedDependenciesCCprocessed
    println(tdl)
    println()
    val tp: TreePrint = new TreePrint("penn,typedDependenciesCollapsed")
    tp.printTree(parse)
  }
}


