package noisycode.predicate

import org.scalatest._

class PConsTests extends FlatSpec with Matchers {
  "Simple list literals and patterns" should "be parsed as PCons successfully" in {
    PredicateParser.parseElem("[]").get should be (PNil)
    PredicateParser.parseElem("[1, 2, 3]").get should be 
      PCons(PInteger(1), PCons(PInteger(2), PCons(PInteger(3), PNil)))
    PredicateParser.parseElem("[1 | 2]").get should be (PCons(PInteger(1), PInteger(2)))
    PredicateParser.parseElem("[A | Tail]").get should be
      PCons(Variable("A"), Variable("B"))
  }
}
