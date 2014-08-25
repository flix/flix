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
    def keyword: Parser[SExp.Keyword] = ("def-type" | "def-bot" | "def-leq" | "def-lub" | "def-fun" | "rule" ) ^^ SExp.Keyword


    def int = regex( """[0-9]+""".r) ^^ { (i: String) => Literal.Int(i.toInt)}

    /**
     *
     */
    def name = regex( """[A-Z][A-Za-z+-/\*]*""".r) ^^ Literal.Name

    def ident = regex( """[a-z+-/\*]+""".r) ^^ Literal.Str

    def variable = "_".r ^^ SExp.Variable

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
