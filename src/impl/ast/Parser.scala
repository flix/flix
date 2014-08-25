package impl.ast

import java.io.File

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Parser {

  /**
   * A simple parser for s-expression based on Scala's parser combinators.
   */
  object InternalParser extends RegexParsers {
    def int = regex( """[0-9]+""".r) ^^ { (i: String) => Literal.Int(i.toInt)}

    def keyword = "def-type" ^^ Keyword.DefType

    def ident = regex( """[A-Za-z+-/\*]+""".r) ^^ Literal.Str

    def node: Parser[SExp] = keyword | int | ident | sexp

    def sexp = "(" ~> rep(node) <~ ")" ^^ SExp.Lst

    def decl = rep(sexp)

    def apply(s: String) = parse(decl, s)
  }

  def parse(f: File): List[SExp] = {
    val source = Source.fromFile(f).getLines().mkString("\n")
    InternalParser(source) match {
      case InternalParser.Success(p, _) => p
      case a => println(a); ???
    }
  }

  // TODO: Q:
  // When to use lists?

}
