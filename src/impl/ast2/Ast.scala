package impl.ast2

import impl.logic.BinaryOperator
import impl.logic.UnaryOperator

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

/**
 * The type of all Ast nodes.
 */
sealed trait Ast

object Ast {

  /**
   * The Ast root node.
   *
   * At the highest level an Ast is a sequence of declarations.
   */
  case class Root(decls: Seq[Declaration]) extends Ast

  /**
   * A common super-type for AST nodes which represent declarations.
   */
  sealed trait Declaration extends Ast

  object Declaration {

    /**
     * An AST node which represents a namespace declaration.
     */
    case class NameSpace(name: Seq[String], body: Seq[Declaration]) extends Declaration

    /**
     * An AST node which represents a type declaration.
     */
    case class Tpe(name: String, typ: Type) extends Declaration

    /**
     * An AST node which represents a value declaration.
     */
    case class Val(name: String, t: Type, exp: Expression) extends Declaration

    /**
     * An AST node which represents a variable declaration.
     */
    case class Var(name: String, typ: Type) extends Declaration

    /**
     * An AST node which represents a function declaration.
     */
    case class Fun(annotations: Seq[String], name: String, arguments: Seq[(String, Type)], typ: Type, body: Expression) extends Declaration

    /**
     * An AST node which represents a lattice declaration.
     */
    case class Lattice(name: String, record: Expression) extends Declaration

    /**
     * An AST node which represents a fact declaration.
     */
    case class Fact(head: Predicate) extends Declaration

    /**
     * An AST node which represent a rule declaration.
     */
    case class Rule(head: Predicate, body: Seq[Predicate]) extends Declaration

    /**
     * An AST node which represents a enum declaration.
     */
    case class Enum(name: String, tpe: Type.Enum) extends Declaration
  }

  /**
   * A common super-type for AST node which represent literals.
   */
  sealed trait Literal

  object Literal {

    /**
     * An AST node which represents a boolean literal.
     */
    case class Bool(literal: scala.Boolean) extends Literal

    /**
     * An AST node which represents an integer literal.
     */
    case class Int(literal: scala.Int) extends Literal

    /**
     * An AST node which represents a string literal.
     */
    case class Str(literal: java.lang.String) extends Literal

  }

  /**
   * A common super-type for AST nodes which represent expressions.
   */
  sealed trait Expression extends Ast

  object Expression {

    /**
     * An AST node which represents either a variable or a reference to a named value.
     */
    @Eliminated
    case class AmbiguousName(name: Seq[String]) extends Expression

    /**
     * An AST node which represents a reference to a variable.
     *
     * Introduced by the compiler.
     */
    @Introduced
    case class Var(name: String) extends Expression

    /**
     * An AST node which represents a literal.
     */
    case class Lit(literal: Literal) extends Expression

    /**
     * An AST node which represents unary expressions.
     */
    case class Unary(op: UnaryOperator, e: Expression) extends Expression

    /**
     * An AST node which represents binary expressions.
     */
    case class Binary(e1: Expression, op: BinaryOperator, e2: Expression) extends Expression

    /**
     * An AST node which represents an infix expression.
     */
    case class Infix(e1: Expression, name: Seq[String], e2: Expression) extends Expression

    /**
     * An AST node which represents a let-binding.
     */
    case class Let(name: String, value: Expression, body: Expression) extends Expression

    /**
     * An AST node which represents an if-then-else expression.
     */
    case class IfThenElse(e1: Expression, e2: Expression, e3: Expression) extends Expression

    /**
     * An AST node which represents a match expression.
     */
    case class Match(values: Expression, rules: Seq[(Pattern, Expression)]) extends Expression

    /**
     * An AST node which represents a function call.
     */
    case class Call(function: Expression, arguments: Seq[Expression]) extends Expression

    /**
     * An AST node which represents a tagged expression.
     */
    case class Tag(name: String, e: Expression) extends Expression

    /**
     * An AST node which represents a set expression.
     */
    case class Set(elms: Seq[Expression]) extends Expression

    /**
     * An AST node which represents a tuple expression.
     */
    case class Tuple(elms: Seq[Expression]) extends Expression

    /**
     * An AST node which represents a record expression.
     */
    case class Record(elms: Seq[(String, Expression)]) extends Expression

    /**
     * An AST node which represents an error expression.
     *
     * Evaluating an error expression always results in a runtime error.
     */
    case object Error extends Expression

  }

  /**
   * A common super-type for AST nodes which represent patterns.
   */
  sealed trait Pattern extends Ast

  object Pattern {

    /**
     * An AST node which represents a wildcard (the underscore _) pattern.
     */
    case object Wildcard extends Pattern

    /**
     * An AST node which represents a variable pattern.
     */
    case class Var(name: String) extends Pattern

    /**
     * An AST node which represents a literal boolean pattern.
     */
    case class Bool(literal: scala.Boolean) extends Pattern

    /**
     * An AST node which represents a literal integer pattern.
     */
    case class Int(literal: scala.Int) extends Pattern

    /**
     * An AST node which represents a literal string pattern.
     */
    case class Str(literal: String) extends Pattern

    /**
     * An AST node which represents a tagged pattern.
     */
    case class Tag(name: Seq[String], pattern: Pattern) extends Pattern

    /**
     * An AST node which represents a tuples pattern.
     */
    case class Tuple(elms: Seq[Pattern]) extends Pattern

  }


  // TODO: Parse Predicates!
  case class Predicate(name: String, t2: Term) extends Pattern

  // TODO: Refactor?

  sealed trait Term extends Ast

  object Term {

    case class Lit(literal: Literal) extends Term

    case class Apply(name: Seq[String], arguments: Seq[Term]) extends Term

    case class Name(name: Seq[String]) extends Term

    case class Tuple(elms: Seq[Term]) extends Term

    case class Set(elms: Seq[Term]) extends Term

    case class Map(key: Term, value: Term) extends Term

  }


  /**
   * A common super-type for AST nodes which represent types.
   */
  sealed trait Type extends Ast

  object Type {

    /**
     * An AST node which represents a reference to a named type.
     *
     * Eliminated by the compiler.
     */
    @Eliminated
    case class AmbiguousName(name: Seq[String]) extends Type

    /**
     * An AST node which represents the unit type.
     */
    @Introduced
    case object Unit extends Type

    /**
     * An AST node which represents the boolean type.
     */
    @Introduced
    case object Bool extends Type

    /**
     * An AST node which represents the int type.
     */
    @Introduced
    case object Int extends Type

    /**
     * An AST node which represents the string type.
     */
    @Introduced
    case object Str extends Type

    /**
     * An AST node which represents a tuple type.
     */
    case class Tuple(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a set type.
     */
    case class Set(elms: Type) extends Type

    /**
     * An AST node which represents a relation type.
     */
    @Eliminated
    case class Rel(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a map type.
     */
    case class Map(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a tagged type.
     */
    case class Tag(name: String) extends Type

    /**
     * An AST node which represents an enumeration type.
     */
    case class Enum(elms: Seq[Type.Tag]) extends Type

    /**
     * An AST node which represents a function type.
     */
    case class Function(t1: Type, t2: Type) extends Type

    /**
     * A common super-type for AST nodes which represent types.
     */
    sealed trait LatticeType extends Type

    /**
     * An AST node which represents a set lattice type.
     */
    case class SetLattice(elms: Type) extends LatticeType

    /**
     * An AST node which represents a product lattice type.
     */
    // TODO: Do we actually need this? We have tuples!
    case class ProductLattice(t1: Type, t2: Type) extends LatticeType

    /**
     * An AST node which represents a map lattice type.
     */
    case class MapLattice(t1: Type, t2: Type) extends LatticeType

  }

  /**
   * An AST node annotation which documents that the AST node is introduced by the compiler.
   *
   * That is, the AST node is *not* generated by the parser.
   */
  final class Introduced extends StaticAnnotation

  /**
   * An AST node annotation which documents that the AST node is eliminated by the compiler.
   *
   * That is, the AST not is generated by the parser and then later desugared.
   */
  final class Eliminated extends StaticAnnotation

}
