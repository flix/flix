package impl.ast

import java.io.File

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Parser {

  object InternalParser extends RegexParsers {
    def int = regex( """[0-9]+""".r) ^^ { (i: String) => Literal.Int(i.toInt)}

    def keyword = "def-type" ^^ Keyword.DefType

    def ident = regex( """[A-Za-z+-/\*]+""".r) ^^ Literal.Str

    def sexp = "(" ~> rep(node) <~ ")" ^^ SExp.Lst

    def node: Parser[SExp] = keyword | int | ident | sexp

    def apply(s: String) = parse(sexp, s)
  }

  def parse(f: File): SExp = {
    val source = Source.fromFile(f).mkString
    InternalParser(source) match {
      case InternalParser.Success(p, _) => p
      case a => println(a); ???
    }
  }

  // TODO: Parser combinator...

  // Q:
  // When to use lists?

}
