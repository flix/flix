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
     * An AST node which represents an ambiguous function call.
     */
    @Eliminated
    case class AmbiguousCall(name: Ast.QName, arguments: Seq[Ast.Expression]) extends Ast.Expression

    /**
     * An AST node which represents a reference to a variable.
     *
     * Introduced by the compiler.
     */
    @Introduced
    case class Var(name: String) extends Ast.Expression

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
     */
    case class Let(ident: Ast.Ident, value: Ast.Expression, body: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents an if-then-else expression.
     */
    case class IfThenElse(e1: Ast.Expression, e2: Ast.Expression, e3: Ast.Expression) extends Ast.Expression

    /**
     * An AST node which represents a match expression.
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
    case class Record(elms: Seq[(Ast.Ident, Ast.Expression)]) extends Ast.Expression

    /**
     * An AST node which represents a set expression.
     */
    case class Set(elms: Seq[Ast.Expression]) extends Ast.Expression

    /**
     * An AST node which represents a map expression.
     */
    case class Map(elms: Seq[(Ast.Expression, Ast.Expression)]) extends Ast.Expression

    /**
     * An AST node which represents an error expression.
     *
     * Evaluating an error expression always results in a runtime error.
     */
    case object Error extends Ast.Expression

  }

  /**
   * A common super-type for AST nodes which represent patterns.
   */
  sealed trait Pattern extends Ast

  object Pattern {

    /**
     * An AST node which represents a variable or tagged pattern.
     */
    @Eliminated
    case class Ambiguous(name: Ast.QName, pattern: Option[Pattern]) extends Ast.Pattern

    /**
     * An AST node which represents a wildcard pattern.
     */
    case object Wildcard extends Ast.Pattern

    /**
     * An AST node which represents a tagged pattern.
     */
    @Introduced
    case class Tag(name: Seq[String], pattern: Ast.Pattern) extends Ast.Pattern

    /**
     * An AST node which represents a pattern match literal
     */
    case class Lit(literal: Ast.Literal) extends Ast.Pattern

    /**
     * An AST node which represents a tuples pattern.
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
   * A common super-type for AST nodes which represent types.
   */
  sealed trait Type extends Ast

  object Type {

    /**
     * An AST node which represents a reference to a named type.
     */
    @Eliminated
    case class Ambiguous(name: QName) extends Type

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
    @Introduced
    case class Tag(ident: Ast.Ident) extends Type

    /**
     * An AST node which represents an enumeration type.
     */
    @Introduced
    case class Enum(elms: Seq[Type.Tag]) extends Type

    /**
     * An AST node which represents a tuple type.
     */
    case class Tuple(elms: Seq[Type]) extends Type

    /**
     * An AST node which represents a list type.
     */
    case class List(elms: Type) extends Type

    /**
     * An AST node which represents a set type.
     */
    case class Set(elms: Type) extends Type

    /**
     * An AST node which represents a map type.
     */
    @Right2Left
    case class Map(t1: Type, t2: Type) extends Type

    /**
     * An AST node which represents a function type.
     */
    @Right2Left
    case class Function(t1: Type, t2: Type) extends Type

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

  /**
   * An AST node annotation which documents that the children of the AST node were parsed
   * from right-to-left, but they should be interpreted from left-to-right.
   */
  // TODO: Do we really need this?
  final class Right2Left extends StaticAnnotation

}
