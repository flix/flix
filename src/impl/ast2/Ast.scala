package impl.ast2

import java.io.File

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

  /**
   * The type of all Ast nodes.
   */
  sealed trait Node

  /**
   * The root the Ast. A root consists of a sequence of namespace declarations.
   */
  case class Root(namespaces: Seq[NameSpace])


  case class SimpleName(name: String) extends Node

  case class NameSpace(name: String, body: Seq[Ast.Declaration]) extends Node

  sealed trait Declaration extends Node

  case class TypeDeclaration(x: String, t: Type) extends Declaration

  case class FunctionDeclaration(x: String) extends Declaration

  sealed trait Type extends Node

  case class TypeTag(x: String) extends Type

  case class TypeVariant(xs: Seq[Type])
}

import org.parboiled2._

class Calculator(val input: ParserInput) extends Parser {
  def TopLevel: Rule1[Ast.Root] = rule {
    oneOrMore(NameSpace) ~ EOI ~> Ast.Root
  }

  def NameSpace: Rule1[Ast.NameSpace] = rule {
    "namespace" ~ WhiteSpace ~ capture(Name) ~ WhiteSpace ~ '{' ~ WhiteSpace ~ NameSpaceBody ~ WhiteSpace ~ '}' ~> Ast.NameSpace
  }

  def NameSpaceBody: Rule1[Seq[Ast.Declaration]] = rule {
    zeroOrMore(Declaration)
  }

  def Declaration: Rule1[Ast.Declaration] = rule {
    //TypeDeclaration | ValueDeclaration
    TypeDeclaration | FunctionDeclaration
  }

  def TypeDeclaration: Rule1[Ast.TypeDeclaration] = rule {
    "type" ~ WhiteSpace ~ capture(Identifier) ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Type ~ WhiteSpace ~> Ast.TypeDeclaration
  }

  def ValueDeclaration = rule {
    "val" ~ Identifier ~ optional(WhiteSpace) ~ ":" ~ WhiteSpace ~ Identifier ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Identifier ~ ";" ~ WhiteSpace
  }

  def FunctionDeclaration: Rule1[Ast.FunctionDeclaration] = rule {
    "def" ~ WhiteSpace ~ capture(Identifier) ~ WhiteSpace ~> Ast.FunctionDeclaration
  }

  def ArgumentList = rule {
    zeroOrMore((Argument ~ "," ~ WhiteSpace) | Argument)
  }

  def Argument = rule {
    Identifier ~ ":" ~ WhiteSpace ~ Identifier
  }

  def Type: Rule1[Ast.TypeTag] = rule {
    capture(Identifier) ~> Ast.TypeTag
  }

  def TypeVariant: Rule1[Ast.TypeVariant] = rule {
    oneOrMore(Type) ~> Ast.TypeVariant
  }

  def Name: Rule0 = rule {
    QualifiedName
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

  /**
   * Whitespace is one or more spaces, tabs or newlines.
   */
  def WhiteSpace = rule {
    oneOrMore(" " | "\t" | "\n")
  }

}

