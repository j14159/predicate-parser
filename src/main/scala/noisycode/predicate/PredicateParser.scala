package noisycode.predicate

import scala.util.parsing.combinator._

sealed trait Syntax

case class Atom(label: String) extends Syntax
case class Variable(label: String) extends Syntax

case class PInteger(i: Int) extends Syntax
case class PList(elems: Seq[Syntax]) extends Syntax
case class Fact(predicate: Syntax, children: Seq[Syntax]) extends Syntax

case class Binding(v: Variable, s: Syntax)
case class LetRec(bindings: Seq[Binding])
case class Func(sig: Fact, let: LetRec, body: Syntax) extends Syntax

object PredicateParser extends RegexParsers {
  def integer: Parser[PInteger] = """-?[0-9]+""".r ^^ { i => PInteger(i.toInt) }
  def variable: Parser[Variable] = """[A-Z_][a-zA-Z0-9_]*""".r ^^ { v => Variable(v) }
  def atom: Parser[Atom] = """[a-z][a-zA-Z0-9_]*""".r ^^ { a => Atom(a) }

  def list: Parser[PList] = "[" ~ rep(element) ~ "]" ^^ {
    case "[" ~ elems ~ "]" => PList(elems)
  }

  def fact: Parser[Fact] = (atom | variable) ~ "(" ~ repsep(element, ",") ~ ")" ^^ { 
    case pred ~ "(" ~ facts ~ ")" => Fact(pred, facts)
  }

  def binding: Parser[Binding] = variable ~ "=" ~ (func | element) ^^ { case v ~ "=" ~ s => Binding(v, s) }
  def let: Parser[LetRec] = "let" ~ rep(binding) ~ "in" ^^ { case "let" ~ b ~ "in" => LetRec(b) }

  def func: Parser[Func] = fact ~ "->" ~ opt(let) ~ (fact | element) ^^ { 
    case sig ~ "->" ~ None ~ body => Func(sig, LetRec(Nil), body) 
    case sig ~ "->" ~ Some(lr) ~ body => Func(sig, lr, body)
  }

  def element = (fact | atom | variable | integer | list)

  def parseFact(expr: String) = parseAll(fact, expr)
  def parseKnowledgeBase(source: String) = parseAll(rep(func | element), source)
}
