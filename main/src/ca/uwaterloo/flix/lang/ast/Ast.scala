package ca.uwaterloo.flix.lang.ast

import ca.uwaterloo.flix.lang.SourceLocation

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

/**
 * The type of all Ast nodes.
 */
sealed trait Ast

// TODO: Consider renaming to ParsedAst
// TODO: Ensure that every reference is prefixed with ParsedAst.XYZ
// TODO: that vs. which
// TODO: Ensure that @param is documented.


object Ast {

  /**
   * The Ast root node.
   *
   * At the highest level an Ast is a sequence of declarations.
   */
  case class Root(declarations: Seq[Ast.Declaration]) extends Ast

  /**
   * An AST node that represent an identifier.
   *
   * @param name the identifier.
   * @param location the source location of the identifier.
   */
  case class Ident(name: String, location: SourceLocation) extends Ast

  /**
   * An AST node that represents a qualified name.
   *
   * @param parts the name parts.
   * @param location the source location of the first name part.
   */
  case class QName(parts: Seq[String], location: SourceLocation) extends Ast

  /**
   * A common super-type for AST nodes which represent declarations.
   */
  sealed trait Declaration extends Ast

  object Declaration {

    /**
     * An AST node which represents a namespace declaration.
     */
    case class Namespace(name: QName, body: Seq[Ast.Declaration]) extends Ast.Declaration

    /**
     * An AST node which represents a type declaration.
     */
    case class Tpe(ident: Ast.Ident, typ: Type) extends Ast.Declaration

    /**
     * An AST node which represents a enum declaration.
     */
    case class Enum(ident: Ast.Ident, body: Seq[Ast.Type.Tag]) extends Ast.Declaration

    /**
     * An AST node which represents a value declaration.
     */
    case class Val(ident: Ast.Ident, tpe: Ast.Type, exp: Ast.Expression) extends Ast.Declaration

    /**
     * An AST node which represents a variable declaration.
     */
    case class Var(ident: Ast.Ident, tpe: Ast.Type) extends Ast.Declaration

    /**
     * An AST node which represents a function declaration.
     */
    case class Fun(annotations: Seq[Ast.Ident], ident: Ast.Ident, arguments: Seq[(Ast.Ident, Ast.Type)], typ: Ast.Type, body: Ast.Expression) extends Ast.Declaration

    /**
     * An AST node which represents a lattice declaration.
     */
    case class Lattice(ident: Ast.Ident, record: Ast.Expression) extends Ast.Declaration

    /**
     * An AST node that represents a fact declaration.
     */
    case class Fact(head: Ast.AmbiguousPredicate) extends Ast.Declaration

    /**
     * An AST node that represent a rule declaration.
     */
    case class Rule(head: Ast.AmbiguousPredicate, body: Seq[Ast.AmbiguousPredicate]) extends Ast.Declaration

  }

  /**
   * A common super-type for AST node which represent literals.
   */
  sealed trait Literal

  object Literal {

    /**
     * An AST node which represents the unit literal.
     */
    case object Unit extends Ast.Literal

    /**
     * An AST node which represents a boolean literal.
     */
    case class Bool(literal: scala.Boolean) extends Ast.Literal

    /**
     * An AST node which represents an integer literal.
     */
    case class Int(literal: scala.Int) extends Ast.Literal

    /**
     * An AST node which represents a string literal.
     */
    case class Str(literal: java.lang.String) extends Ast.Literal

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
    case class AmbiguousName(name: Ast.QName) extends Ast.Expression

    /**
     * An AST node that represents a function call.
     *
     * @param name the ambiguous name of the function.
     * @param arguments the arguments to the function.
     */
    case class AmbiguousApply(name: Ast.QName, arguments: Seq[Ast.Expression]) extends Ast.Expression

    /**
     * An AST node which represents a literal.
     */
    case class Lit(literal: Ast.Literal) extends Ast.Expression

    /**
     * An AST node which represents a (generalized) lambda expression.
     */
    case class Lambda(formals: Seq[(Ast.Ident, Type)], tpe: Ast.Type, e: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents unary expressions.
     */
    case class Unary(op: UnaryOperator, e: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents binary expressions.
     */
    case class Binary(e1: Ast.Expression, op: BinaryOperator, e2: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents a let-binding.
     *
     * @param ident the identifier to be bound.
     * @param value the expression whose value the identifier should be bound to.
     * @param body the expression in which the bound variable is visible.
     */
    case class Let(ident: Ast.Ident, value: Ast.Expression, body: Ast.Expression) extends Ast.Expression

    /**
     * An AST node that represents an if-then-else expression.
     *
     * @param e1 the conditional expression.
     * @param e2 the consequence expression.
     * @param e3 the alternative expression.
     */
    case class IfThenElse(e1: Ast.Expression, e2: Ast.Expression, e3: Ast.Expression) extends Ast.Expression

    /**
     * An AST node that represents a match expression.
     *
     * @param exp the match expression.
     * @param rules the match rules and their bodies.
     */
    case class Match(exp: Ast.Expression, rules: Seq[(Ast.Pattern, Ast.Expression)]) extends Ast.Expression

    /**
     * An AST node which represents an infix function call expression.
     */
    case class Infix(e1: Ast.Expression, name: Ast.QName, e2: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents a tagged expression.
     */
    case class Tag(ident: Ast.Ident, e: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents a tuple expression.
     */
    case class Tuple(elms: Seq[Ast.Expression]) extends Ast.Expression

    /**
     * An AST node which represents a record expression.
     */
    // TODO: Needed?
    case class Record(elms: Seq[(Ast.Ident, Ast.Expression)]) extends Ast.Expression

    /**
     * An AST node that represents an error expression.
     *
     * @param location the source location where the error expression occurs.
     */
    case class Error(location: SourceLocation) extends Ast.Expression

  }

  /**
   * A common super-type for AST nodes which represent patterns.
   */
  sealed trait Pattern extends Ast

  object Pattern {

    /**
     * An AST node that represents a wildcard pattern.
     */
    case class Wildcard(location: SourceLocation) extends Ast.Pattern

    /**
     * An AST node that represents a variable pattern.
     */
    case class Var(ident: Ast.Ident) extends Ast.Pattern

    /**
     * An AST node that represents a literal pattern.
     */
    case class Lit(literal: Ast.Literal) extends Ast.Pattern

    /**
     * An AST node that represents a tuple pattern.
     */
    case class Tuple(elms: Seq[Ast.Pattern]) extends Ast.Pattern

  }

  /**
   * An AST node that represent either a proposition or a predicate.
   */
  case class AmbiguousPredicate(name: Ast.QName, terms: Seq[Ast.Term]) extends Pattern

  /**
   * A common super-type for AST that represent terms.
   *
   * A term is either a wildcard, variable, literal or a function call.
   */
  sealed trait Term extends Ast

  object Term {

    /**
     * An AST node that represent a wildcard variable term.
     */
    case class Wildcard(location: SourceLocation) extends Ast.Term

    /**
     * An AST node that represent a variable term.
     */
    case class Var(ident: Ast.Ident) extends Ast.Term

    /**
     * An AST node that represent a literal term.
     */
    case class Lit(literal: Ast.Literal) extends Ast.Term

    /**
     * An AST node that represent a function call term.
     */
    case class Apply(name: QName, arguments: Seq[Ast.Term]) extends Ast.Term

  }

  /**
   * A common super-type for AST nodes that represent types.
   */
  sealed trait Type extends Ast

  object Type {

    /**
     * An AST node that represent the unit type.
     */
    case object Unit extends Ast.Type

    /**
     * An AST node that represent a reference to a type.
     *
     * @param name the ambiguous name.
     */
    case class Ambiguous(name: Ast.QName) extends Ast.Type

    /**
     * An AST node that represent a function type.
     *
     * @param t1 the type of the domain.
     * @param t2 the type of the range.
     */
    case class Function(t1: Ast.Type, t2: Ast.Type) extends Ast.Type

    /**
     * An AST node that represent a tuple type.
     *
     * @param elms the type of the individual elements.
     */
    case class Tuple(elms: Seq[Ast.Type]) extends Ast.Type

    /**
     * An AST node that represent a parametric type.
     *
     * @param name the ambiguous name.
     * @param elms the type of the type parameters.
     */
    case class Parametric(name: Ast.QName, elms: Seq[Ast.Type]) extends Ast.Type


    /**
     * An AST node which represents a tagged type.
     */
    // TODO: Needed in this phase?
    case class Tag(ident: Ast.Ident) extends Ast.Type

  }

  /**
   * An AST node annotation which documents that the AST node is introduced by the compiler.
   *
   * That is, the AST node is *not* generated by the parser.
   */
  // TODO: Do we really need this?
  final class Introduced extends StaticAnnotation

  /**
   * An AST node annotation which documents that the AST node is eliminated by the compiler.
   *
   * That is, the AST not is generated by the parser and then later desugared.
   */
  // TODO: Do we really need this?
  final class Eliminated extends StaticAnnotation

}
