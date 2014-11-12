package impl.ast2

import java.io.File

import impl.ast2.Ast.Calculator
import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}
import scala.collection.immutable.Seq

object Foo {
  def main(args: Array[String]): Unit = {

    val line = Source.fromFile(new File("src/examples/Sign.flix")).getLines().mkString("\n")

    val parser = new Calculator(line)
    parser.TopLevel.run() match {
      case Success(exprAst) => println("Result: " + exprAst)
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => println("Unexpected error during parsing run: " + e)
    }
  }
}

sealed trait Ast

object Ast {

  sealed trait Node

  case class Root(namespaces: Seq[NameSpace])

  case class SimpleName(name: String) extends Node

  case class NameSpace(name: String) extends Node

  import org.parboiled2._

  class Calculator(val input: ParserInput) extends Parser {
    def TopLevel: Rule1[Root] = rule {
      oneOrMore(NameSpaceDeclaration) ~ EOI ~> Root
    }

    def NameSpaceDeclaration: Rule1[Ast.NameSpace] = rule {
      "namespace" ~ WhiteSpace ~ capture(Name) ~ '{' ~ WhiteSpace ~ Declaration ~ '}' ~> Ast.NameSpace
    }

    def Declaration = rule {
      zeroOrMore(TypeDeclaration | ValueDeclaration | FunctionDeclaration)
    }

    def TypeDeclaration = rule {
      "type" ~ WhiteSpace ~ Identifier ~ WhiteSpace ~ "=" ~ WhiteSpace ~ zeroOrMore(Identifier | "|" | WhiteSpace) ~ ";" ~ WhiteSpace
    }

    def ValueDeclaration = rule {
      Keywords.Val ~ Identifier ~ optional(WhiteSpace) ~ ":" ~ WhiteSpace ~ Identifier ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Identifier ~ ";" ~ WhiteSpace
    }

    def FunctionDeclaration = rule {
      "def" ~ WhiteSpace ~ Identifier ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ WhiteSpace ~ Identifier ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression
    }

    def ArgumentList = rule {
      zeroOrMore((Argument ~ "," ~ WhiteSpace) | Argument)
    }

    def Argument = rule {
      Identifier ~ ":" ~ WhiteSpace ~ Identifier
    }

    def Name: Rule0 = rule {
      QualifiedName ~ WhiteSpace
    }

    def SimpleName = rule {
      Identifier
    }

    def QualifiedName: Rule0 = rule {
      SimpleName ~ zeroOrMore(QualifiedName)
    }

    def Expression: Rule0 = rule {
      MatchExpression | TupleExp | LocalVariable
    }

    def MatchExpression: Rule0 = rule {
      "match" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "with" ~ WhiteSpace ~ "{" ~ WhiteSpace ~ MatchBody ~ WhiteSpace ~ "}"
    }

    def MatchBody = rule {
      "case" ~ WhiteSpace ~ Pattern
    }

    def Pattern = rule {
      "("
    }

    def TupleExp = rule {
      "(" ~ Expression ~ ", " ~ Expression ~ ")"
    }

    def LocalVariable = rule {
      Identifier
    }

    def Identifier = rule {
      CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)
    }

    def Digits = rule {
      oneOrMore(CharPredicate.Digit)
    }

    def WhiteSpace = rule {
      oneOrMore(" " | "\t" | "\n")
    }

    object Keywords {
      def Val = rule {
        "val" ~ WhiteSpace
      }
    }

  }

}
