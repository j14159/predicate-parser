package noisycode.predicate

import org.scalatest._

class PConsTests extends FlatSpec with Matchers {
  "Simple list literals and patterns" should "be parsed as PCons successfully" in {
    PredicateParser.parseElem("[]").get should be (PNil)
    PredicateParser.parseElem("[1, 2, 3]").get should be 
      PCons(PInteger(1), PCons(PInteger(2), PCons(PInteger(3), PNil)))
    PredicateParser.parseElem("[1 | 2]").get should be (PCons(PInteger(1), PCons(PInteger(2), PNil)))
    PredicateParser.parseElem("[A | Tail]").get should be
      PCons(Variable("A"), PCons(Variable("B"), PNil))
  }

  "List patterns" should "match as expected" in {
    val kb1 = PredicateParser.parseKnowledgeBase("[1, 2, 3]").get

    val expected1 = List(Matched(PCons(PInteger(1), PCons(PInteger(2), PCons(PInteger(3), PNil))), Map()))
    val expected2 = List(Matched(kb1.head, Map(
      "A" -> PInteger(1), 
      "B" -> PCons(PInteger(2), PCons(PInteger(3), PNil)))))

    Matcher.query("[1, 2, 3]", kb1) should be (expected1)
    Matcher.query("[A | B]", kb1) should be (expected2)
  }
}
