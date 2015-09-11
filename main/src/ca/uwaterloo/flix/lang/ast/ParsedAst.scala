package ca.uwaterloo.flix.lang.ast

import scala.collection.immutable.Seq

/**
 * The type of all Ast nodes.
 */
sealed trait ParsedAst

// TODO: that vs. which
// TODO: Ensure that @param is documented.
// TODO: Every where QName appears, should it be call ambigi?
// TODO: Consider order

object ParsedAst {

  /**
   * The Ast root node.
   *
   * At the highest level an Ast is a sequence of declarations.
   */
  case class Root(declarations: Seq[ParsedAst.Declaration]) extends ParsedAst

  /**
   * An AST node that represent an identifier.
   *
   * @param name the identifier.
   * @param location the source location of the identifier.
   */
  case class Ident(name: String, location: SourceLocation) extends ParsedAst

  /**
   * An AST node that represents a qualified name.
   *
   * @param parts the name parts.
   * @param location the source location of the first name part.
   */
  case class QName(parts: Seq[String], location: SourceLocation) extends ParsedAst

  /**
   * A common super-type for AST nodes which represent declarations.
   */
  sealed trait Declaration extends ParsedAst

  object Declaration {

    /**
     * An AST node which represents a namespace declaration.
     */
    case class Namespace(name: QName, body: Seq[ParsedAst.Declaration]) extends ParsedAst.Declaration

    /**
     * An AST node which represents a type declaration.
     */
    case class Tpe(ident: ParsedAst.Ident, typ: Type) extends ParsedAst.Declaration

    /**
     * An AST node which represents a enum declaration.
     */
    case class Enum(ident: ParsedAst.Ident, body: Seq[ParsedAst.Type.Tag]) extends ParsedAst.Declaration

    /**
     * An AST node which represents a value declaration.
     */
    case class Val(ident: ParsedAst.Ident, tpe: ParsedAst.Type, exp: ParsedAst.Expression) extends ParsedAst.Declaration

    /**
     * An AST node which represents a variable declaration.
     */
    case class Var(ident: ParsedAst.Ident, tpe: ParsedAst.Type) extends ParsedAst.Declaration

    /**
     * An AST node which represents a function declaration.
     */
    case class Fun(annotations: Seq[ParsedAst.Ident], ident: ParsedAst.Ident, arguments: Seq[(ParsedAst.Ident, ParsedAst.Type)], typ: ParsedAst.Type, body: ParsedAst.Expression) extends ParsedAst.Declaration

    /**
     * An AST node which represents a lattice declaration.
     */
    case class Lattice(ident: ParsedAst.Ident, record: ParsedAst.Expression) extends ParsedAst.Declaration

    /**
     * An AST node that represents a fact declaration.
     */
    case class Fact(head: ParsedAst.AmbiguousPredicate) extends ParsedAst.Declaration

    /**
     * An AST node that represent a rule declaration.
     */
    case class Rule(head: ParsedAst.AmbiguousPredicate, body: Seq[ParsedAst.AmbiguousPredicate]) extends ParsedAst.Declaration

  }

  /**
   * A common super-type for AST node which represent literals.
   */
  sealed trait Literal

  object Literal {

    /**
     * An AST node which represents the unit literal.
     */
    case object Unit extends ParsedAst.Literal

    /**
     * An AST node which represents a boolean literal.
     */
    case class Bool(literal: scala.Boolean) extends ParsedAst.Literal

    /**
     * An AST node which represents an integer literal.
     */
    case class Int(literal: scala.Int) extends ParsedAst.Literal

    /**
     * An AST node which represents a string literal.
     */
    case class Str(literal: java.lang.String) extends ParsedAst.Literal

  }

  /**
   * AST nodes for expressions.
   */
  sealed trait Expression extends ParsedAst

  object Expression {

    /**
     * An AST node that represents an unresolved variable.
     *
     * @param name the ambiguous name.
     */
    case class AmbiguousVar(name: ParsedAst.QName) extends ParsedAst.Expression

    /**
     * An AST node that represents a function application.
     *
     * @param name the unresolved name of the function.
     * @param arguments the arguments to the function.
     */
    case class AmbiguousApply(name: ParsedAst.QName, arguments: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
     * An AST node that represents a literal.
     *
     * @param literal the literal.
     */
    case class Lit(literal: ParsedAst.Literal) extends ParsedAst.Expression

    /**
     * An AST node that represents a lambda expression.
     *
     * @param formals the formals (i.e. parameters and their types).
     * @param tpe the return type.
     * @param body the body expression of the lambda.
     */
    case class Lambda(formals: Seq[(ParsedAst.Ident, Type)], tpe: ParsedAst.Type, body: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents unary expressions.
     *
     * @param op the unary operator.
     * @param e the expression.   
     */
    case class Unary(op: UnaryOperator, e: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents binary expressions.
     *
     * @param e1 the left expression.
     * @param op the binary operator.
     * @param e2 the right expression.
     */
    case class Binary(e1: ParsedAst.Expression, op: BinaryOperator, e2: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a let-binding.
     *
     * @param ident the identifier to be bound.
     * @param value the expression whose value the identifier should be bound to.
     * @param body the expression in which the bound variable is visible.
     */
    case class Let(ident: ParsedAst.Ident, value: ParsedAst.Expression, body: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents an if-then-else expression.
     *
     * @param e1 the conditional expression.
     * @param e2 the consequence expression.
     * @param e3 the alternative expression.
     */
    case class IfThenElse(e1: ParsedAst.Expression, e2: ParsedAst.Expression, e3: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a match expression.
     *
     * @param exp the match expression.
     * @param rules the match rules and their bodies.
     */
    case class Match(exp: ParsedAst.Expression, rules: Seq[(ParsedAst.Pattern, ParsedAst.Expression)]) extends ParsedAst.Expression

    /**
     * An AST node that represents an infix function call.
     *
     * @param e1 the first argument expression.
     * @param name the ambiguous name of the function.
     * @param e2 the second argument expression.
     */
    case class Infix(e1: ParsedAst.Expression, name: ParsedAst.QName, e2: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a tagged expression.
     *
     * @param name the unresolved name of the tag.
     * @param e the nested expression.
     */
    case class Tag(name: ParsedAst.QName, e: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a tuple expression.
     *
     * @param elms the elements of the tuple.
     */
    case class Tuple(elms: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
     * An AST node that ascribe a type to an expression.
     *
     * @param e the expression.
     * @param tpe the ascribed type.
     */
    case class Ascribe(e: ParsedAst.Expression, tpe: ParsedAst.Type) extends ParsedAst.Expression

    /**
     * An AST node that represents an error expression.
     *
     * @param location the source location where the error expression occurs.
     */
    case class Error(location: SourceLocation) extends ParsedAst.Expression

  }

  /**
   * A common super-type for AST nodes which represent patterns.
   */
  sealed trait Pattern extends ParsedAst

  object Pattern {

    /**
     * An AST node that represents a wildcard pattern.
     */
    case class Wildcard(location: SourceLocation) extends ParsedAst.Pattern

    /**
     * An AST node that represents a variable pattern.
     */
    case class Var(ident: ParsedAst.Ident) extends ParsedAst.Pattern

    /**
     * An AST node that represents a literal pattern.
     */
    case class Lit(literal: ParsedAst.Literal) extends ParsedAst.Pattern

    /**
     * An AST node that represents a tuple pattern.
     */
    case class Tuple(elms: Seq[ParsedAst.Pattern]) extends ParsedAst.Pattern

  }

  /**
   * An AST node that represent either a proposition or a predicate.
   */
  case class AmbiguousPredicate(name: ParsedAst.QName, terms: Seq[ParsedAst.Term]) extends Pattern

  /**
   * A common super-type for AST that represent terms.
   *
   * A term is either a wildcard, variable, literal or a function call.
   */
  sealed trait Term extends ParsedAst

  object Term {

    /**
     * An AST node that represent a wildcard variable term.
     */
    case class Wildcard(location: SourceLocation) extends ParsedAst.Term

    /**
     * An AST node that represent a variable term.
     */
    case class Var(ident: ParsedAst.Ident) extends ParsedAst.Term

    /**
     * An AST node that represent a literal term.
     */
    case class Lit(literal: ParsedAst.Literal) extends ParsedAst.Term

    /**
     * An AST node that represent a function call term.
     */
    case class Apply(name: QName, arguments: Seq[ParsedAst.Term]) extends ParsedAst.Term

  }

  /**
   * A common super-type for AST nodes that represent types.
   */
  sealed trait Type extends ParsedAst

  object Type {

    /**
     * An AST node that represent the unit type.
     */
    case object Unit extends ParsedAst.Type

    /**
     * An AST node that represent a reference to a type.
     *
     * @param name the ambiguous name.
     */
    case class Ambiguous(name: ParsedAst.QName) extends ParsedAst.Type

    /**
     * An AST node that represent a function type.
     *
     * @param t1 the type of the domain.
     * @param t2 the type of the range.
     */
    case class Function(t1: ParsedAst.Type, t2: ParsedAst.Type) extends ParsedAst.Type

    /**
     * An AST node that represent a tuple type.
     *
     * @param elms the type of the individual elements.
     */
    case class Tuple(elms: Seq[ParsedAst.Type]) extends ParsedAst.Type

    /**
     * An AST node that represent a parametric type.
     *
     * @param name the ambiguous name.
     * @param elms the type of the type parameters.
     */
    case class Parametric(name: ParsedAst.QName, elms: Seq[ParsedAst.Type]) extends ParsedAst.Type


    /**
     * An AST node which represents a tagged type.
     */
    // TODO: Needed in this phase?
    case class Tag(ident: ParsedAst.Ident) extends ParsedAst.Type

  }

}
