package ca.uwaterloo.flix.lang.ast

import scala.collection.immutable.Seq

/**
 * The type of all Ast nodes.
 */
sealed trait ParsedAst

// TODO: that vs. which
// TODO: Ensure that @param is documented.
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
     * An AST node that represents a namespace declaration.
     *
     * @param name the name of the namespace.
     * @param body the nested declarations.
     */
    case class Namespace(name: ParsedAst.QName, body: Seq[ParsedAst.Declaration]) extends ParsedAst.Declaration

    /**
     * An AST node that represent a type alias.
     *
     * @param ident the name of the alias.
     * @param tpe the type of the alias.
     */
    // NB: This class is called `Tpe` since the name `Type` causes problems with the Scala compiler/shapeless.
    case class Tpe(ident: ParsedAst.Ident, tpe: ParsedAst.Type) extends ParsedAst.Declaration

    /**
     * An AST node that represents a (constant) value declaration.
     *
     * @param ident the name of the value.
     * @param tpe the declared type of the value.
     * @param e the expression.
     */
    case class Val(ident: ParsedAst.Ident, tpe: ParsedAst.Type, e: ParsedAst.Expression) extends ParsedAst.Declaration

    /**
     * An AST node that represents a function declaration.
     *
     * @param ident the name of the function.
     * @param formals the formals (i.e. parameters and their types).
     * @param tpe the return type.
     * @param body the body expression of the function.
     */
    case class Fun(ident: ParsedAst.Ident, formals: Seq[(ParsedAst.Ident, ParsedAst.Type)], tpe: ParsedAst.Type, body: ParsedAst.Expression) extends ParsedAst.Declaration

    /**
     * An AST node that represents a enum declaration.
     *
     * @param ident the name of the enum.
     * @param body the variants of the enum.
     */
    case class Enum(ident: ParsedAst.Ident, body: Seq[ParsedAst.Type.Tag]) extends ParsedAst.Declaration

    /**
     * An AST node which represents a variable declaration.
     */
    case class Var(ident: ParsedAst.Ident, tpe: ParsedAst.Type) extends ParsedAst.Declaration

    /**
     * An AST node which represents a lattice declaration.
     */
    case class Lattice(ident: ParsedAst.Ident, record: ParsedAst.Expression) extends ParsedAst.Declaration

    /**
     * An AST node that represents a fact declaration.
     *
     * @param head the head predicate.
     */
    case class Fact(head: ParsedAst.AmbiguousPredicate) extends ParsedAst.Declaration

    /**
     * An AST node that represent a rule declaration.
     *
     * @param head the head predicate.
     * @param body the body predicates.
     */
    case class Rule(head: ParsedAst.AmbiguousPredicate, body: Seq[ParsedAst.AmbiguousPredicate]) extends ParsedAst.Declaration

  }

  /**
   * AST nodes for Literals.
   */
  sealed trait Literal

  object Literal {

    /**
     * An AST node that represents the Unit literal.
     */
    case object Unit extends ParsedAst.Literal

    /**
     * An AST node that represents a boolean literal.
     *
     * @param literal the boolean literal.
     */
    case class Bool(literal: scala.Boolean) extends ParsedAst.Literal

    /**
     * An AST node that represents an integer literal.
     *
     * @param literal the integer literal.
     */
    case class Int(literal: scala.Int) extends ParsedAst.Literal

    /**
     * An AST node that represents a string literal.
     *
     * @param literal the string literal.
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
   * AST nodes for Patterns.
   */
  sealed trait Pattern extends ParsedAst

  object Pattern {

    /**
     * An AST node that represents a wildcard pattern.
     *
     * @param location the source location of the wildcard.
     */
    case class Wildcard(location: SourceLocation) extends ParsedAst.Pattern

    /**
     * An AST node that represents a variable pattern.
     *
     * @param ident the variable identifier.
     */
    case class Var(ident: ParsedAst.Ident) extends ParsedAst.Pattern

    /**
     * An AST node that represents a literal pattern.
     *
     * @param literal the literal.
     */
    case class Lit(literal: ParsedAst.Literal) extends ParsedAst.Pattern

    /**
     * An AST node that represents a tuple pattern.
     *
     * @param elms the elements of the tuple.
     */
    case class Tuple(elms: Seq[ParsedAst.Pattern]) extends ParsedAst.Pattern

  }

  /**
   * An AST node that represent an unresolved predicate.
   *
   * @param name the unresolved name of the predicate.
   * @param terms the terms of the predicate.
   */
  case class AmbiguousPredicate(name: ParsedAst.QName, terms: Seq[ParsedAst.Term]) extends Pattern

  /**
   * AST nodes for Terms.
   */
  sealed trait Term extends ParsedAst

  object Term {

    /**
     * An AST node that represent a wildcard variable term.
     *
     * @param location the source location of the wildcard.
     */
    case class Wildcard(location: SourceLocation) extends ParsedAst.Term

    /**
     * An AST node that represent a variable term.
     *
     * @param ident the variable identifier.
     */
    case class Var(ident: ParsedAst.Ident) extends ParsedAst.Term

    /**
     * An AST node that represent a literal term.
     *
     * @param literal the literal.
     */
    case class Lit(literal: ParsedAst.Literal) extends ParsedAst.Term

    /**
     * An AST node that represent a function application term
     *
     * @param name the unresolved name of the function.
     * @param args the arguments to the function.
     */
    case class Apply(name: ParsedAst.QName, args: Seq[ParsedAst.Term]) extends ParsedAst.Term

  }

  /**
   * AST node for Types.
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
     * An AST node that represents a tagged type.
     *
     * @param ident the tag name.
     * @param tpe the type of nested components.
     */
    case class Tag(ident: ParsedAst.Ident, tpe: ParsedAst.Type) extends ParsedAst.Type

    /**
     * An AST node that represent a parametric type.
     *
     * @param name the ambiguous name.
     * @param elms the type of the type parameters.
     */
    case class Parametric(name: ParsedAst.QName, elms: Seq[ParsedAst.Type]) extends ParsedAst.Type

  }

}
