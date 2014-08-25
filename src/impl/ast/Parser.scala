package impl.ast

import java.io.File

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Parser {

  /**
   * A simple parser for s-expression based on Scala's parser combinators.
   */
  object InternalParser extends RegexParsers {

    /**
     * Keywords.
     */
    def keyword: Parser[SExp.Keyword] =
      ("def-type" | "def-bot" | "def-leq" | "def-lub" | "def-height" | "def-fun" | "rule" | "match" | "case") ^^ SExp.Keyword

    def int = regex( """[0-9]+""".r) ^^ { (i: String) => SExp.Int(i.toInt)}


    /**
     *
     */
    def name = regex( """[A-Z][A-Za-z+-/\*]*""".r) ^^ SExp.Name

    def ident = regex( """[a-z+-/\*]+""".r) ^^ SExp.Str

    def variable = "_".r ^^ SExp.Var

    def node: Parser[SExp] = keyword | int | ident | name | sexp | variable

    def sexp = "(" ~> rep(node) <~ ")" ^^ SExp.Lst

    /**
     * Declarations.
     */
    def declarations: Parser[List[SExp]] = rep(sexp)

    def apply(s: String) = parse(declarations, s)
  }

  def parse(f: File): List[SExp] = {
    val source = Source.fromFile(f).getLines().mkString("\n")
    InternalParser.parseAll(InternalParser.declarations, source) match {
      case InternalParser.Success(p, x) => p
      case a => println(a); ???
    }
  }

}
