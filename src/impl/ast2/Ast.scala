package impl.ast2

import impl.logic.BinaryOperator
import impl.logic.UnaryOperator

import scala.collection.immutable.Seq

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
   * A common super-type for AST nodes which represent declarations.
   */
  sealed trait Declaration extends Node

  object Declaration {

    /**
     * An AST node which represents a namespace declaration.
     */
    case class NameSpace(name: Name, body: Seq[Ast.Declaration]) extends Declaration

    /**
     * An AST node which represents a type declaration.
     */
    // TODO: Naming issue? Why can't this be called type?
    case class TypeDecl(name: String, typ: Type) extends Declaration

    /**
     * An AST node which represents a value declaration.
     */
    case class Val(name: String, t: Type, exp: Expression) extends Declaration

    /**
     * An AST node which represents a variable declaration.
     */
    case class Var(name: String, typ: Type) extends Declaration

    /**
     * An AST node which represents a fact declaration.
     */
    case class Fact(name: String, head: Predicate) extends Declaration

    /**
     * An AST node which represent a rule declaration.
     */
    case class Rule(name: String, head: Predicate, body: Seq[Predicate]) extends Declaration

  }


  case class FunctionDeclaration(an: Seq[Annotation], x: String, arguments: Seq[Argument], returnType: Type, exp: Expression) extends Declaration

  case class Argument(name: String, typ: Type) extends Node

  case class MatchRule(p: Pattern, e: Expression) extends Node

  case class Annotation(s: String) extends Node


  sealed trait Pattern extends Node

  object Pattern {

    case object Wildcard extends Pattern

    case class Var(name: Name) extends Pattern

    case class Tuple(elms: Seq[Pattern]) extends Pattern

  }

  case class Predicate(t1: Term, t2: Term) extends Pattern

  sealed trait Term extends Node

  object Term {

    case class Call(n: Name, args: Seq[Term]) extends Term

    case class Int(i: String) extends Term

    case class NameRef(n: Name) extends Term

  }

  /**
   * A common super-type for AST nodes which represent expressions.
   */
  sealed trait Expression extends Node

  object Expression {

    /**
     * An AST node which represents a boolean literal.
     */
    case class BoolLit(literal: Boolean) extends Expression

    /**
     * An AST node which represents an integer literal.
     */
    case class IntLit(literal: Int) extends Expression

    /**
     * An AST node which represents a string literal.
     */
    case class StrLit(literal: String) extends Expression

    /**
     * An AST node which represents unary expressions.
     */
    case class Unary(op: UnaryOperator, e: Expression) extends Expression

    /**
     * An AST node which represents binary expressions.
     */
    case class Binary(op: BinaryOperator, e1: Expression, e2: Expression) extends Expression

    /**
     * An AST node which represents an if-then-else expression.
     */
    case class IfThenElse(e1: Expression, e2: Expression, e3: Expression) extends Expression

    /**
     * An AST node which represents a match expression.
     */
    case class Match(values: Expression, rules: Seq[Ast.MatchRule]) extends Expression


    // TODO: Introduce let.

    case class Tuple(xs: Seq[Expression]) extends Expression


    // TODO: Could be a let or lambda bound thing, or a reference to a global thing.
    // Elimintated by the compiler.
    case class UnresolvedName(name: Name) extends Expression

    // Introduced by the compiler.

    /**
     * An AST node which represents a "missing" (i.e. not-yet-implemented) expression.
     *
     * Evaluating a missing expression always results in a runtime error.
     *
     * The semantic difference between missing and impossible is that missing represents
     * some missing code whereas impossible represent a situation which is guaranteed,
     * by the programmer, not to occur.
     */
    case object Missing extends Expression

    /**
     * An AST node which represents an "impossible" (i.e. unreachable code) expression.
     *
     * Evaluating an impossible expression always results in a runtime error.
     *
     * The semantic difference between missing and impossible is that missing represents
     * some missing code whereas impossible represent a situation which is guaranteed,
     * by the programmer, not to occur.
     */
    case object Impossible extends Expression

  }

  /**
   * A common super-type for AST nodes which represent names.
   */
  sealed trait Name

  object Name {

    /**
     * An AST node which represents a simple name, i.e. an identifier.
     */
    case class Simple(name: String) extends Name

    /**
     * An AST node which represents a qualified name.
     *
     * A name such as a.b.c is represented as Qual(a, Qual(b, Simple(c)).
     */
    case class Qualified(prefix: String, suffix: Name) extends Name

  }

  /**
   * A common super-type for AST nodes which represents types.
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
     * Eliminated by the compiler.
     */
    case class NameRef(name: Name) extends Type

  }

}
