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
     * Keyword production.
     */
    def keyword: Parser[SExp.Keyword] =
      ("def-type" | "def-bot" | "def-leq" | "def-lub" | "def-height" | "def-fun" | "rule" | "match" | "case") ^^ SExp.Keyword

    /**
     * Value production.
     */
    // unit literal
    def unit: Parser[SExp.Unit] = "unit" ^^ SExp.Unit

    // boolean literal
    def bool: Parser[SExp.Bool] = ("true" | "false") ^^ SExp.Bool

    // integer literal
    def int: Parser[SExp.Int] = """[0-9]+""".r ^^ SExp.Int

    // string literal
    def str: Parser[SExp.Str] = """"[a-zA-Z]+"""".r ^^ SExp.Str

    // value production
    def value: Parser[SExp] = unit | bool | int | str


    def name = regex( """[A-Z][A-Za-z+-/\*]*""".r) ^^ SExp.Name

    def ident = regex( """[a-z+-/\*]+""".r) ^^ SExp.Str

    def variable = "_".r ^^ SExp.Var

    def node: Parser[SExp] = keyword | value | ident | name | sexp | variable

    /**
     * S-expression production.
     */
    def sexp: Parser[SExp] = "(" ~> rep(node) <~ ")" ^^ SExp.Lst

    /**
     * Declaration production.
     */
    def decl: Parser[List[SExp]] = rep(sexp)
  }

  /**
   * Returns a list of s-expressions parsed from the given file `f`.
   */
  def parse(f: File): List[SExp] = {
    // read all lines into a single string.
    val source = Source.fromFile(f).getLines().mkString("\n")
    // parse the entire file
    val parseResult = InternalParser.parseAll(InternalParser.decl, source)
    // inspect the result
    parseResult match {
      case InternalParser.Success(ast, next) => ast
      case InternalParser.Failure(msg, next) => throw new RuntimeException(s"Parsing Failed!: $msg")
    }
  }

}
