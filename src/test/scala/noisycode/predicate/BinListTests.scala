package noisycode.predicate

import org.scalatest._

class BinListTests extends FlatSpec with Matchers {
  "simple binary lists" should "parse correctly" in {
    val literal = "<<12, 0, 82, 255, 42>>"
    val shouldFail = "<<0, 5, 81, 256, 50, 400>>"
    val pattern = "<<_/16, 5, NoWidth, 12, Wide/32, Wider/256>>"

    PredicateParser.parseKnowledgeBase(literal) should be
      (List(BinList(List(BinByte(12), BinByte(0), BinByte(82), BinByte(255), BinByte(42)))))

    PredicateParser.parseKnowledgeBase(shouldFail).successful should be (false)

    PredicateParser.parseKnowledgeBase(pattern) should be 
      (List(
        BinList(List(
          BinVar(Variable("_"), Some(2)), 
          BinByte(5),
          BinVar(Variable("NoWidth"), None),
          BinByte(12), 
          BinVar(Variable("Wide"), Some(4)), 
          BinVar(Variable("Wider"), Some(32))))))
  }

  "simple patter matches against binary lists" should "match as expected" in {
    val list1 = PredicateParser.parseKnowledgeBase("<<10, 12, 255, 18, 42>>").get

    Matcher.query("<<A, B>>", list1) should be (List(Matched(list1.head, 
      Map("A" -> BinByte(10), "B" -> BinList(List(BinByte(12), BinByte(255), BinByte(18), BinByte(42)))))))
    Matcher.query("<<10, 12, A/16, _>>", list1) should be (List(Matched(list1.head,
      Map("A" -> BinList(List(BinByte(255), BinByte(18)))))))
    Matcher.query("<<10, 12, _>>", list1) should be (List(Matched(list1.head, Map())))
    Matcher.query("<<_, 12, _, 18, _>>", list1) should be (List(Matched(list1.head, Map())))
  }
}
