package noisycode.predicate

import scala.util.parsing.combinator._

sealed trait Syntax

case class Atom(label: String) extends Syntax
case class Variable(label: String) extends Syntax

case class PInteger(i: Int) extends Syntax

sealed trait PConsList extends Syntax 

case object PNil extends PConsList
case class PCons(head: Syntax, tail: PConsList) extends PConsList

sealed trait BinElem extends Syntax
/**
  * A binary variable expression with an optional bit width like Erlang.  The width here is
  * the count in bytes whereas in source you would give number of bits (currently must be divisible
  * by 8).  E.g. <<MyVar/32>> is BinVar(Variable("MyVar"), 4)
  */
case class BinVar(v: Variable, width: Option[Int]) extends BinElem
/**
  * This is obviously a bit verbose for just an integer representation of a byte but I wanted to keep
  * binary lists completely isolated in terms of their definition for now (as opposed to having
  * Binlist(elems: Seq[Syntax]) ).
  */
case class BinByte(b: Int) extends BinElem

case class BinList(elems: Seq[BinElem]) extends Syntax

case class Fact(predicate: Syntax, children: Seq[Syntax]) extends Syntax

case class Binding(v: Variable, s: Syntax)
case class LetRec(bindings: Seq[Binding])
case class Func(sig: Fact, let: LetRec, body: Syntax) extends Syntax

object PredicateParser extends RegexParsers {
  def integer: Parser[PInteger] = """-?[0-9]+""".r ^^ { i => PInteger(i.toInt) }
  def variable: Parser[Variable] = """[A-Z_][a-zA-Z0-9_]*""".r ^^ { v => Variable(v) }
  def atom: Parser[Atom] = """[a-z][a-zA-Z0-9_]*""".r ^^ { a => Atom(a) }

  def cons: Parser[PConsList] = "[" ~ element ~ "|" ~ element ~ "]" ^^ {
    case "[" ~ head ~ "|" ~ tail ~ "]" => PCons(head, PCons(tail, PNil))
  }

  def seqToPCons(l: Seq[Syntax], memo: PConsList = PNil): PConsList = l match {
    case Nil => memo
    case h :: t => seqToPCons(t, PCons(h, memo))
  }

  def literalList: Parser[PConsList] = "[" ~ repsep(element, ",") ~ "]" ^^ {
    case "[" ~ elems ~ "]" => seqToPCons(elems.reverse)
  }

  def byte: Parser[BinByte] = """(\d)+""".r flatMap {
    case i if i.toInt < 256 && i.toInt > -1 => success(BinByte(i.toInt))
    case i => failure("Bytes must be between 0 and 255")
  }

  def binVar: Parser[BinVar] = variable ~ opt("/" ~ """(\d)+""".r) ^^ {
    case v ~ Some("/" ~ width) if width.toInt % 8 == 0 => BinVar(v, Some(width.toInt / 8))
    case v ~ None => BinVar(v, None)
  }

  def binList: Parser[BinList] = "<<" ~ repsep((byte | binVar), ",") ~ ">>" ^^ {
    case "<<" ~ binElems ~ ">>" => BinList(binElems)
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

  def element = (fact | atom | variable | integer | literalList | cons | binList)

  def parseFact(expr: String) = parseAll(fact, expr)
  def parseElem(expr: String) = parseAll(element, expr)
  def parseKnowledgeBase(source: String) = parseAll(rep(func | element), source)
}
