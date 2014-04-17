package noisycode.predicate

case class Matched(theMatch: Syntax, bindings: Map[String, Syntax])

/**
  * Simple pattern matching including function signatures.
  */
object Matcher {
  def query(expr: String, kb: Seq[Syntax]): Seq[Matched] = {
    val q = PredicateParser.parseElem(expr).get
    kb.map(fromKb => check(q, fromKb, fromKb, Map())).flatten
  }

  def check(q: Syntax, fromKb: Syntax, orig: Syntax, bindings: Map[String, Syntax]): Option[Matched] = {
    (q, fromKb) match {
      case (Variable(a), Variable(b)) if a == "_" | b == "_" => Some(Matched(orig, bindings))
      case (Variable(a), Variable(b)) if a == b => Some(Matched(orig, bindings))
      case (Variable(a), syntax) => Some(Matched(orig, bindings + (a -> syntax)))
      case (Atom(a), Atom(b)) if a == b => Some(Matched(orig, bindings))
      case (a: BinList, b: BinList) => checkBin(a, b)
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

  def checkBin(q: BinList, fromKb: BinList): Option[Matched] = 
    checkBin(q.elems, fromKb.elems, fromKb.elems, Map())

  /**
    * This one is almost twice as long as necessary because I'm allowing variables on both sides.
    * Probably could also handle bindings much better, big TODO.
    */
  def checkBin(q: Seq[BinElem], fromKb: Seq[BinElem], orig: Seq[BinElem], bindings: Map[String, Syntax]): Option[Matched] = {
    //minor helper to handle "_" throaway variable and binding:
    def bind(varName: String, toBind: Syntax, existing: Map[String, Syntax]) = 
      if(varName != "_") existing + (varName -> toBind) else existing

    (q, fromKb) match {
      //base case:
      case (Nil, Nil) => Some(Matched(BinList(orig), bindings))

      //a variable in the final position with no specified width should take everything from the other side:
      case (List(BinVar(Variable(a), None)), right) if !right.isEmpty =>
        Some(Matched(BinList(orig), bind(a, BinList(right), bindings)))
      case (left, List(BinVar(Variable(b), None))) if !left.isEmpty && b != "_" =>
        Some(Matched(BinList(orig), bind(b, BinList(left), bindings)))

      //variables with the same name only match if their widths are the same:
      case (BinVar(Variable(a), wA) :: l, BinVar(Variable(b), wB) :: r) if a == b && wA == wB =>
        checkBin(l, r, orig, bindings)

      //variables with set widths only match if the list on the other side has enough elements:
      case (BinVar(Variable(name), Some(width)) :: left, right) if width <= right.length =>
        checkBin(left, right.drop(width), orig, bind(name, BinList(right.take(width)), bindings))
      case (left, BinVar(Variable(name), Some(width)) :: right) if width <= left.length =>
        checkBin(left.drop(width), right, orig, bind(name, BinList(left.take(width)), bindings))

      //variables with no set width (defaults to a BinByte, not a Seq:
      case (BinVar(Variable(name), _) :: left, x :: right) =>
        checkBin(left, right, orig, bind(name, x, bindings))
      case (x :: left, BinVar(Variable(name), _) :: right) =>
        checkBin(left, right, orig, bind(name, x, bindings))

      //the only other match is equal integers:
      case (BinByte(a) :: left, BinByte(b) :: right) if a == b =>
        checkBin(left, right, orig, bindings)
      case _ => None
    }
  }
}
