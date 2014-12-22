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

  // TODO: Introduce "Name".

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
     * An AST node which represents a enum declaration.
     */
    case class Enum(name: String, tpe: Type.Enum) extends Declaration

    /**
     * An AST node which represents a value declaration.
     */
    case class Val(name: String, tpe: Type, exp: Expression) extends Declaration

    /**
     * An AST node which represents a variable declaration.
     */
    case class Var(name: String, lat: Ast.Lattice) extends Declaration

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

  }

  /**
   * A common super-type for AST node which represent literals.
   */
  sealed trait Literal

  object Literal {

    /**
     * An AST node which represents the unit literal.
     */
    case object Unit extends Literal

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
     * An AST node which represents a (generalized) lambda expression.
     */
    case class Lambda(formals: Seq[(String, Type)], tpe: Ast.Type, e: Expression) extends Expression

    /**
     * An AST node which represents unary expressions.
     */
    case class Unary(op: UnaryOperator, e: Expression) extends Expression

    /**
     * An AST node which represents binary expressions.
     */
    case class Binary(e1: Expression, op: BinaryOperator, e2: Expression) extends Expression

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
    case class Match(exp: Expression, rules: Seq[(MatchPattern, Expression)]) extends Expression

    /**
     * An AST node which represents a function call.
     */
    case class Call(name: Seq[String], arguments: Seq[Expression]) extends Expression

    /**
     * An AST node which represents an infix function call expression.
     */
    case class Infix(e1: Expression, name: Seq[String], e2: Expression) extends Expression

    /**
     * An AST node which represents a tagged expression.
     */
    case class Tag(name: String, e: Expression) extends Expression

    /**
     * An AST node which represents a tuple expression.
     */
    case class Tuple(elms: Seq[Expression]) extends Expression

    /**
     * An AST node which represents a record expression.
     */
    case class Record(elms: Seq[(String, Expression)]) extends Expression

    /**
     * An AST node which represents a set expression.
     */
    case class Set(elms: Seq[Expression]) extends Expression

    /**
     * An AST node which represents a map expression.
     */
    case class Map(elms: Seq[(Expression, Expression)]) extends Expression

    /**
     * An AST node which represents an error expression.
     *
     * Evaluating an error expression always results in a runtime error.
     */
    case object Error extends Expression

  }

  /**
   * A common super-type for AST nodes which represent match patterns.
   */
  sealed trait MatchPattern extends Ast

  object MatchPattern {

    /**
     * An AST node which represents a wildcard pattern.
     */
    case object Wildcard extends MatchPattern

    /**
     * An AST node which represents a variable pattern.
     */
    @Eliminated
    case class AmbigiousName(name: String) extends MatchPattern

    /**
     * An AST node which represents a pattern match literal
     */
    case class Lit(literal: Literal) extends MatchPattern

    /**
     * An AST node which represents a tagged pattern.
     */
    case class Tag(name: Seq[String], pattern: MatchPattern) extends MatchPattern

    /**
     * An AST node which represents a tuples pattern.
     */
    case class Tuple(elms: Seq[MatchPattern]) extends MatchPattern

  }


  // TODO: Parse Predicates!
  // todo: Should be seq String
  case class Predicate(name: String, t: Term) extends MatchPattern

  // TODO: Refactor?

  sealed trait Term extends Ast

  object Term {

    // TODO: Var

    // TODO: Ambigious name
    case class Name(name: Seq[String]) extends Term

    case class Lit(literal: Literal) extends Term

    // TODO: Rename to Call
    case class Apply(name: Seq[String], arguments: Seq[Term]) extends Term

    // TODO: Tag?

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
     * An AST node which represents a tagged type.
     */
    case class Tag(name: String) extends Type

    /**
     * An AST node which represents an enumeration type.
     */
    case class Enum(elms: Seq[Type.Tag]) extends Type

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
    case class Map(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a function type.
     */
    case class Function(t1: Type, t2: Type) extends Type

  }

  /**
   * A common super-type for AST nodes which represent lattices.
   */
  sealed trait Lattice

  object Lattice {

    /**
     * An AST node which represents a reference to a lattice.
     */
    case class Name(name: Seq[String]) extends Lattice

    /**
     * An AST node which represents a set lattice.
     */
    case class Set(elms: Type) extends Lattice

    /**
     * An AST node which represents a map lattice.
     */
    case class Map(keys: Seq[Type], value: Lattice) extends Lattice

    /**
     * An AST node which represents a product lattice.
     */
    case class Product(elms: Seq[Lattice]) extends Lattice

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
