package noisycode.predicate

import org.scalatest._

class SimpleKnowledgeBaseTests extends FlatSpec with ShouldMatchers {
  val simpleKb = """
    fact(hello)
    fact(int(42)) -> 42
    fact(int(X), int(Y)) -> add(X, Y)
    fact2(hello)
    with_let(int(X)) ->
      let
        Y = add(X, 2)
      in mult(Y, X)"""

  "a simple knowledge base" should "be parsed successfully" in {
    val bigFunc =       
      Matched(Func(Fact(Atom("fact"), List(Fact(Atom("int"), List(PInteger(42))))),
        LetRec(List()), 
        PInteger(42)), 
        Map("X" -> Fact(Atom("int"),List(PInteger(42)))))

    val res = PredicateParser.parseKnowledgeBase(simpleKb)
    val q1 = "fact(X)"
    val expected1 = List(
      Matched(Fact(Atom("fact"), List(Atom("hello"))), Map("X" -> Atom("hello"))), bigFunc)

    val q2 = "fact(wat)"
    val expected2 = List()

    val q3 = "fact(int(X))"
    val expected3 = List(bigFunc.copy(bindings = Map("X" -> PInteger(42))))

    val q4 = "X(hello)"
    val expected4 = List(
      Matched(Fact(Atom("fact"), List(Atom("hello"))), Map("X" -> Atom("fact"))),
      Matched(Fact(Atom("fact2"), List(Atom("hello"))), Map("X" -> Atom("fact2"))))
 
    Matcher.query(q1, res.get) should be (expected1)
    Matcher.query(q2, res.get) should be (expected2)
    Matcher.query(q3, res.get) should be (expected3)
    Matcher.query(q4, res.get) should be (expected4)
  }
}
