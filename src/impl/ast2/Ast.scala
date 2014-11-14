package impl.ast2

import java.beans.Expression
import java.io.File

import impl.ast2.Ast
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

  sealed trait Name

  case class SimpleName(name: String) extends Name

  case class QualifiedName(prefix: String, rest: Name) extends Name

  case class NameSpace(name: Name, body: Seq[Ast.Declaration]) extends Node

  sealed trait Declaration extends Node

  case class TypeDeclaration(name: String, t: Type) extends Declaration

  case class ValueDeclaration(name: String, t: Type, exp: Expression) extends Declaration

  case class FunctionDeclaration(x: String, arguments: Seq[Argument], returnType: Type, exp: Expression) extends Declaration

  sealed trait Type extends Node

  object Type {

    case class Bool() extends Type

    case class Int() extends Type

    case class Str() extends Type

    case class Tag(x: String) extends Type

    case class Sum(xs: Seq[Type]) extends Type

  }


  case class Argument(name: String, typ: Type) extends Node


  case class MatchRule(p: Pattern, e: Expression) extends Node


  sealed trait Expression extends Node

  object Expression {

    case class Literal(s: String) extends Expression

    case class Variable(name: Name) extends Expression

    case class IfThenElse(cond: Expression, e2: Expression, e3: Expression) extends Expression

    case class Match(matchValue: Expression, rules: Seq[Ast.MatchRule]) extends Expression

    case class Tuple(xs: Seq[Expression]) extends Expression

    case class NotImplemented() extends Expression

  }

  sealed trait Pattern extends Node

  object Pattern {

    case class Wildcard() extends Pattern

    case class Var(name: Name) extends Pattern

    case class Tuple(ps: Seq[Pattern]) extends Pattern

  }

}

import org.parboiled2._

class Calculator(val input: ParserInput) extends Parser {
  def TopLevel: Rule1[Ast.Root] = rule {
    oneOrMore(NameSpace) ~ EOI ~> Ast.Root
  }

  def NameSpace: Rule1[Ast.NameSpace] = rule {
    "namespace" ~ WhiteSpace ~ Name ~ WhiteSpace ~ '{' ~ WhiteSpace ~ NameSpaceBody ~ WhiteSpace ~ '}' ~> Ast.NameSpace
  }

  def NameSpaceBody: Rule1[Seq[Ast.Declaration]] = rule {
    zeroOrMore(Declaration)
  }

  def Declaration: Rule1[Ast.Declaration] = rule {
    TypeDeclaration | ValueDeclaration | FunctionDeclaration
  }

  def TypeDeclaration: Rule1[Ast.TypeDeclaration] = rule {
    "type" ~ WhiteSpace ~ capture(Identifier) ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Type ~ ";" ~ WhiteSpace ~> Ast.TypeDeclaration
  }

  def ValueDeclaration: Rule1[Ast.ValueDeclaration] = rule {
    "val" ~ WhiteSpace ~ capture(Identifier) ~ ":" ~ WhiteSpace ~ Type ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ ";" ~ WhiteSpace ~> Ast.ValueDeclaration
  }

  def FunctionDeclaration: Rule1[Ast.FunctionDeclaration] = rule {
    "def" ~ WhiteSpace ~ capture(Identifier) ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ WhiteSpace ~ Type ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ ";" ~ WhiteSpace ~> Ast.FunctionDeclaration
  }

  def ArgumentList: Rule1[Seq[Ast.Argument]] = rule {
    zeroOrMore(Argument).separatedBy("," ~ optional(WhiteSpace))
  }

  def Argument: Rule1[Ast.Argument] = rule {
    capture(Identifier) ~ ":" ~ WhiteSpace ~ Type ~> Ast.Argument
  }

  def Type: Rule1[Ast.Type] = rule {
    BoolType | IntType | StrType | SumType | TagType
  }

  def BoolType: Rule1[Ast.Type.Bool] = rule {
    str("Bool") ~> Ast.Type.Bool
  }

  def IntType: Rule1[Ast.Type.Int] = rule {
    str("Int") ~> Ast.Type.Int
  }

  def StrType: Rule1[Ast.Type.Str] = rule {
    str("Str") ~> Ast.Type.Str
  }

  def TagType: Rule1[Ast.Type.Tag] = rule {
    capture(Identifier) ~> Ast.Type.Tag
  }

  def SumType: Rule1[Ast.Type.Sum] = rule {
    oneOrMore(TagType).separatedBy(optional(WhiteSpace) ~ "|" ~ optional(WhiteSpace)) ~> Ast.Type.Sum
  }

  def Name: Rule1[Ast.Name] = rule {
    QualifiedName | SimpleName
  }

  // TODO: Use separated by.

  def SimpleName: Rule1[Ast.SimpleName] = rule {
    capture(Identifier) ~> Ast.SimpleName
  }

  def QualifiedName: Rule1[Ast.QualifiedName] = rule {
    capture(Identifier) ~ "." ~ Name ~> Ast.QualifiedName
  }

  def Expression: Rule1[Ast.Expression] = rule {
    LiteralExpression | IfThenElseExp | MatchExpression | TupleExpression | VariableExpression | NotImplementedExpression
  }

  def LiteralExpression: Rule1[Ast.Expression.Literal] = rule {
    capture(Digits) ~> Ast.Expression.Literal
  }

  def IfThenElseExp: Rule1[Ast.Expression.IfThenElse] = rule {
    "if" ~ WhiteSpace ~ "(" ~ Expression ~ ")" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "else" ~ WhiteSpace ~ Expression ~> Ast.Expression.IfThenElse
  }

  def MatchExpression: Rule1[Ast.Expression.Match] = rule {
    "match" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "with" ~ WhiteSpace ~ "{" ~ WhiteSpace ~ oneOrMore(MatchRule) ~ "}" ~> Ast.Expression.Match
  }

  def MatchRule: Rule1[Ast.MatchRule] = rule {
    "case" ~ WhiteSpace ~ Pattern ~ WhiteSpace ~ "=>" ~ WhiteSpace ~ Expression ~ ";" ~ WhiteSpace ~> Ast.MatchRule
  }

  def NotImplementedExpression: Rule1[Ast.Expression.NotImplemented] = rule {
    str("???") ~> Ast.Expression.NotImplemented
  }

  def Pattern: Rule1[Ast.Pattern] = rule {
    WildcardPattern | VariablePattern | TuplePattern
  }

  def VariablePattern: Rule1[Ast.Pattern.Var] = rule {
    Name ~> Ast.Pattern.Var
  }

  def WildcardPattern: Rule1[Ast.Pattern.Wildcard] = rule {
    str("_") ~> Ast.Pattern.Wildcard
  }

  def TuplePattern: Rule1[Ast.Pattern.Tuple] = rule {
    "(" ~ oneOrMore(Pattern).separatedBy("," ~ optional(WhiteSpace)) ~ ")" ~> Ast.Pattern.Tuple
  }

  def TupleExpression: Rule1[Ast.Expression.Tuple] = rule {
    "(" ~ oneOrMore(Expression).separatedBy("," ~ optional(WhiteSpace)) ~ ")" ~> Ast.Expression.Tuple
  }

  def VariableExpression: Rule1[Ast.Expression.Variable] = rule {
    Name ~> Ast.Expression.Variable
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
  def WhiteSpace: Rule0 = rule {
    oneOrMore(" " | "\t" | "\n")
  }

}

