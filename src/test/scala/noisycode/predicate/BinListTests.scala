package noisycode.predicate

import org.scalatest._

class BinListTests extends FlatSpec with Matchers {
  "simple binary lists" should "parse correctly" in {
    val literal = "<<12, 0, 82, 255, 42>>"
    val shouldFail = "<<0, 5, 81, 256, 50, 400>>"
    val pattern = "<<_/16, 5, 12, Wide/32, Wider/256>>"

    PredicateParser.parseKnowledgeBase(literal) should be
      (List(BinList(List(BinByte(12), BinByte(0), BinByte(82), BinByte(255), BinByte(42)))))

    PredicateParser.parseKnowledgeBase(shouldFail).successful should be (false)

    PredicateParser.parseKnowledgeBase(pattern) should be 
      (List(
        BinList(List(
          BinVar(Variable("_"),2), 
          BinByte(5), 
          BinByte(12), 
          BinVar(Variable("Wide"),4), 
          BinVar(Variable("Wider"),32)))))
  }

  
}
