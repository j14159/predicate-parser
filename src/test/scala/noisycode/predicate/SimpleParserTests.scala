package noisycode.predicate

import org.scalatest._

class SimpleParserTests extends FlatSpec with Matchers {
  "the fact parser" should "parse basic facts with atoms and variables" in {
    val code1 = "atom(atom2)"
    val code2 = "atom(atom2, atom3(wat))"
    val code3 = "X(child_fact(hi))"
    val code4 = "like_a_query(X, int(42))"

    val res1 = PredicateParser.parseFact(code1)
    val res2 = PredicateParser.parseFact(code2)
    val res3 = PredicateParser.parseFact(code3)
    val res4 = PredicateParser.parseFact(code4)

    res1.get should be (Fact(Atom("atom"), List(Atom("atom2"))))
    res2.get should be (Fact(Atom("atom"), List(Atom("atom2"), Fact(Atom("atom3"), List(Atom("wat"))))))
    res3.get should be (Fact(Variable("X"), List(Fact(Atom("child_fact"), List(Atom("hi"))))))
    res4.get should be (Fact(Atom("like_a_query"), List(Variable("X"), Fact(Atom("int"), List(PInteger(42))))))
  }
}
