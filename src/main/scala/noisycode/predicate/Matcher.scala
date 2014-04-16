package noisycode.predicate

case class Matched(theMatch: Syntax, bindings: Map[String, Syntax])

/**
  * Simple pattern matching including function signatures.
  */
object Matcher {
  def query(expr: String, kb: Seq[Syntax]): Seq[Matched] = {
    val q = PredicateParser.parseFact(expr).get
    kb.map(fromKb => check(q, fromKb, fromKb, Map())).flatten
  }

  def check(q: Syntax, fromKb: Syntax, orig: Syntax, bindings: Map[String, Syntax]): Option[Matched] = {
    (q, fromKb) match {
      case (Variable(a), Variable(b)) if a == "_" | b == "_" => Some(Matched(orig, bindings))
      case (Variable(a), Variable(b)) if a == b => Some(Matched(orig, bindings))
      case (Variable(a), syntax) => Some(Matched(orig, bindings + (a -> syntax)))
      case (Atom(a), Atom(b)) if a == b => Some(Matched(orig, bindings))
      case (Fact(a, left), Fact(b, right)) if left.length == right.length =>
        check(a, b, orig, bindings)
          .flatMap {
          case Matched(m, binds) => 
            val matches = left.zip(right).map(pair => check(pair._1, pair._2, orig, bindings))
            if(matches.contains(None)) //sub-optimal, don't care right now
              None
            else
              Some((binds, matches))
        }.map {
          case (binds, matches) => 
            val allBindings = matches.flatten.foldLeft(binds)((acc, next) => acc ++ next.bindings)
            Matched(orig, allBindings)
        }
      case (_, Func(sig, _, _)) => check(q, sig, orig, bindings)

      case _ => None
    }
  }
}
