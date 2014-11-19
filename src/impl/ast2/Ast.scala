package impl.ast2

import java.io.File

import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}
import scala.collection.immutable.Seq

object Foo {
  def main(args: Array[String]): Unit = {

    val line = Source.fromFile(new File("src/examples/Sign.flix")).getLines().mkString("\n")

    val parser = new Parsing(line)
    parser.Root.run() match {
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
   * The Ast root node.
   *
   * At the highest level an Ast is a sequence of declarations.
   */
  case class Root(decls: Seq[Declaration]) extends Node

  /**
   * AST nodes which represent declarations.
   */
  sealed trait Declaration extends Node

  object Declaration {

    /**
     * A type declaration.
     */
    // TODO: Naming issue? Why can't this be called type?
    case class TypeDecl(name: String, typ: Type) extends Declaration

  }

  case class NameSpace(name: Name, body: Seq[Ast.Declaration]) extends Declaration


  case class ValueDeclaration(name: String, t: Type, exp: Expression) extends Declaration

  case class VariableDeclaration(name: String, t: Type) extends Declaration

  case class FunctionDeclaration(an: Seq[Annotation], x: String, arguments: Seq[Argument], returnType: Type, exp: Expression) extends Declaration

  case class FactDeclaration(name: String, p: Predicate) extends Declaration

  case class RuleDeclaration(name: String) extends Declaration


  case class Argument(name: String, typ: Type) extends Node


  case class MatchRule(p: Pattern, e: Expression) extends Node

  case class Annotation(s: String) extends Node

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

  case class Predicate(t1: Term, t2: Term) extends Pattern

  sealed trait Term extends Node

  object Term {

    case class NameRef(n: Name) extends Term

  }

  sealed trait Name

  case class SimpleName(name: String) extends Name

  case class QualifiedName(prefix: String, rest: Name) extends Name


  /**
   * AST nodes which represents types.
   */
  sealed trait Type extends Node

  object Type {

    /**
     * An AST node which represents the unit type.
     */
    case object Unit extends Type

    /**
     * An AST node which represents the boolean type.
     */
    case object Bool extends Type

    /**
     * An AST node which represents the int type.
     */
    case object Int extends Type

    /**
     * An AST node which represents the string type.
     */
    case object Str extends Type

    /**
     * An AST node which represents a tagged type.
     */
    case class Tag(name: String) extends Type

    /**
     * An AST node which represents a tuple type.
     */
    case class Tuple(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a set type.
     */
    case class Set(elms: Type) extends Type

    /**
     * An AST node which represents a map type.
     */
    case class Map(keys: Type, values: Type) extends Type

    /**
     * An AST node which represents an enumeration type.
     */
    case class Enum(elms: Seq[Type.Tag]) extends Type

    /**
     * An AST node which represents a function type.
     *
     * A function type is a sequence of types t1 -> t2 -> ... -> tn
     * to avoid left-recursion in the grammar.
     */
    case class Function(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a reference to a named type.
     *
     * The compilation replaces all named refs by their actual types.
     */
    case class NameRef(name: Name) extends Type

  }

}
