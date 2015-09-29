package ca.uwaterloo.flix.language.ast

import scala.collection.immutable.Seq

/**
 * A common-super type for all parsed AST nodes.
 */
sealed trait ParsedAst

// TODO: Add source locations everywhere.

object ParsedAst {

  /**
   * The Ast root node. 
   */
  case class Root(declarations: Seq[ParsedAst.Declaration]) extends ParsedAst

  /**
   * A common super-type for AST nodes that represent declarations.
   */
  sealed trait Declaration extends ParsedAst

  object Declaration {

    /**
     * An AST node that represents a namespace declaration.
     *
     * @param name the name of the namespace.
     * @param body the nested declarations.
     */
    case class Namespace(name: Name.Unresolved, body: Seq[ParsedAst.Declaration]) extends ParsedAst.Declaration

    /**
     * An AST node that represents a fact declaration.
     *
     * @param head the head predicate.
     */
    case class Fact(head: ParsedAst.Predicate) extends ParsedAst.Declaration

    /**
     * An AST node that represent a rule declaration.
     *
     * @param head the head predicate.
     * @param body the body predicates.
     */
    case class Rule(head: ParsedAst.Predicate, body: Seq[ParsedAst.Predicate]) extends ParsedAst.Declaration

    // TODO: Add integrity constraints
  }

  /**
   * A common super-type for AST nodes that represent definitions.
   */
  sealed trait Definition extends Declaration

  object Definition {

    /**
     * An AST node that represents a value definition.
     *
     * @param loc the location.
     * @param ident the name of the value.
     * @param tpe the declared type of the value.
     * @param e the expression.
     */
    case class Value(loc: SourceLocation, ident: Name.Ident, tpe: ParsedAst.Type, e: ParsedAst.Expression) extends ParsedAst.Definition

    /**
     * An AST node that represents a function definition.
     *
     * @param loc the location.
     * @param ident the name of the function.
     * @param formals the formals (i.e. parameters and their types).
     * @param tpe the return type.
     * @param body the body expression of the function.
     */
    case class Function(loc: SourceLocation, ident: Name.Ident, formals: Seq[FormalArg], tpe: ParsedAst.Type, body: ParsedAst.Expression) extends ParsedAst.Definition

    /**
     * An AST node that represents a enum definition.
     *
     * @param loc the location.
     * @param ident the name of the enum.
     * @param cases the variants of the enum.
     */
    case class Enum(loc: SourceLocation, ident: Name.Ident, cases: Seq[ParsedAst.Type.Tag]) extends ParsedAst.Definition

    /**
     * An AST node that represents a lattice definition.
     *
     * @param loc the location.
     * @param ident the name of the lattice.
     * @param elms the components of the lattice (e.g. bot, leq, lub).
     * @param traits the traits of the lattice (e.g. Norm and Widening).
     */
    case class Lattice(loc: SourceLocation, ident: Name.Ident, elms: Seq[ParsedAst.Expression], traits: Seq[ParsedAst.Trait]) extends ParsedAst.Definition

    /**
     * An AST that represent a relation definition.
     *
     * @param loc the location.
     * @param ident the name of the relation.
     * @param attributes the name and type of the attributes.
     */
    case class Relation(loc: SourceLocation, ident: Name.Ident, attributes: Seq[ParsedAst.Attribute]) extends ParsedAst.Definition

  }

  /**
   * AST nodes for Literals.
   */
  sealed trait Literal {

  }

  object Literal {

    /**
     * An AST node that represents the Unit literal.
     *
     * @param loc the source location.
     */
    case class Unit(loc: SourceLocation) extends ParsedAst.Literal

    /**
     * An AST node that represents a boolean literal.
     *
     * @param loc the source location.
     * @param lit the boolean literal.
     */
    case class Bool(loc: SourceLocation, lit: scala.Boolean) extends ParsedAst.Literal

    /**
     * An AST node that represents an integer literal.
     *
     * @param loc the source location.
     * @param lit the integer literal.
     */
    case class Int(loc: SourceLocation, lit: scala.Int) extends ParsedAst.Literal

    /**
     * An AST node that represents a string literal.
     *
     * @param loc the source location.
     * @param lit the string literal.
     */
    case class Str(loc: SourceLocation, lit: java.lang.String) extends ParsedAst.Literal

    /**
     * An AST node that represents a tagged literal.
     *
     * @param loc the source location.
     * @param enum the name of the enum.
     * @param tag the name of the tag.
     * @param lit the nested literal.
     */
    case class Tag(loc: SourceLocation, enum: Name.Unresolved, tag: Name.Ident, lit: ParsedAst.Literal) extends ParsedAst.Literal

    /**
     * An AST node that represents a tuple literal.
     *
     * @param loc the source location.
     * @param elms the elements of the tuple.
     */
    case class Tuple(loc: SourceLocation, elms: Seq[ParsedAst.Literal]) extends ParsedAst.Literal

  }

  /**
   * AST nodes for expressions.
   */
  sealed trait Expression extends ParsedAst {
    /**
     * Returns the source location of `this` expression.
     */
    def loc: SourceLocation
  }

  object Expression {

    /**
     * An AST node that represents a literal.
     *
     * @param loc the location.
     * @param lit the literal.
     */
    case class Lit(loc: SourceLocation, lit: ParsedAst.Literal) extends ParsedAst.Expression

    /**
     * An AST node that represents an unresolved variable.
     *
     * @param loc the location.
     * @param name the ambiguous name.
     */
    case class Var(loc: SourceLocation, name: Name.Unresolved) extends ParsedAst.Expression

    /**
     * An AST node that represents a function application.
     *
     * @param loc the location.
     * @param lambda the lambda expression.
     * @param actuals the arguments.
     */
    case class Apply(loc: SourceLocation, lambda: ParsedAst.Expression, actuals: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
     * An AST node that represents a lambda expression.
     *
     * @param loc the location.
     * @param formals the formals (i.e. parameters and their types).
     * @param tpe the return type.
     * @param body the body expression of the lambda.
     */
    case class Lambda(loc: SourceLocation, formals: Seq[FormalArg], tpe: ParsedAst.Type, body: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents unary expressions.
     *
     * @param loc the location.
     * @param op the unary operator.
     * @param e the expression.
     */
    case class Unary(loc: SourceLocation, op: UnaryOperator, e: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents binary expressions.
     *
     * @param e1 the left expression.
     * @param loc the location.
     * @param op the binary operator.
     * @param e2 the right expression.
     */
    case class Binary(e1: ParsedAst.Expression, loc: SourceLocation, op: BinaryOperator, e2: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents an if-then-else expression.
     *
     * @param loc the location.
     * @param e1 the conditional expression.
     * @param e2 the consequence expression.
     * @param e3 the alternative expression.
     */
    case class IfThenElse(loc: SourceLocation, e1: ParsedAst.Expression, e2: ParsedAst.Expression, e3: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a let-binding.
     *
     * @param loc the location.
     * @param ident the identifier to be bound.
     * @param value the expression whose value the identifier should be bound to.
     * @param body the expression in which the bound variable is visible.
     */
    case class Let(loc: SourceLocation, ident: Name.Ident, value: ParsedAst.Expression, body: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a match expression.
     *
     * @param loc the location.
     * @param e the match expression.
     * @param rules the match rules and their bodies.
     */
    case class Match(loc: SourceLocation, e: ParsedAst.Expression, rules: Seq[(ParsedAst.Pattern, ParsedAst.Expression)]) extends ParsedAst.Expression

    /**
     * An AST node that represents an infix function call.
     *
     * @param e1 the first argument expression.
     * @param loc the location.
     * @param name the ambiguous name of the function.
     * @param e2 the second argument expression.
     */
    case class Infix(e1: ParsedAst.Expression, loc: SourceLocation, name: Name.Unresolved, e2: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a tagged expression.
     *
     * @param loc the location.
     * @param name the namespace of the enum.
     * @param ident the tag name.
     * @param e the nested expression.
     */
    case class Tag(loc: SourceLocation, name: Name.Unresolved, ident: Name.Ident, e: ParsedAst.Expression) extends ParsedAst.Expression

    /**
     * An AST node that represents a tuple expression.
     *
     * @param loc the location.
     * @param elms the elements of the tuple.
     */
    case class Tuple(loc: SourceLocation, elms: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
     * An AST node that ascribes a type to an expression.
     *
     * @param loc the location.
     * @param e the expression.
     * @param tpe the ascribed type.
     */
    case class Ascribe(loc: SourceLocation, e: ParsedAst.Expression, tpe: ParsedAst.Type) extends ParsedAst.Expression

    /**
     * An AST node that represents an error expression.
     *
     * @param loc the source location of the error expression.
     * @param tpe the type of the error expression.
     */
    case class Error(loc: SourceLocation, tpe: ParsedAst.Type) extends ParsedAst.Expression

  }

  /**
   * AST nodes for Patterns.
   *
   * A pattern is like a literal except it may contain variables and wildcards.
   */
  sealed trait Pattern extends ParsedAst

  object Pattern {

    /**
     * An AST node that represents a wildcard pattern.
     *
     * @param loc the source location of the wildcard.
     */
    case class Wildcard(loc: SourceLocation) extends ParsedAst.Pattern

    /**
     * An AST node that represents a variable pattern.
     *
     * @param loc the location.
     * @param ident the variable identifier.
     */
    case class Var(loc: SourceLocation, ident: Name.Ident) extends ParsedAst.Pattern

    /**
     * An AST node that represents a literal pattern.
     *
     * @param loc the location.
     * @param lit the literal.
     */
    case class Lit(loc: SourceLocation, lit: ParsedAst.Literal) extends ParsedAst.Pattern

    /**
     * An AST node that represents a tagged pattern.
     *
     * @param loc the location.
     * @param name the namespace of the enum.
     * @param ident the tag name.
     * @param p the nested pattern.
     */
    case class Tag(loc: SourceLocation, name: Name.Unresolved, ident: Name.Ident, p: ParsedAst.Pattern) extends ParsedAst.Pattern

    /**
     * An AST node that represents a tuple pattern.
     *
     * @param loc the location.
     * @param elms the elements of the tuple.
     */
    case class Tuple(loc: SourceLocation, elms: Seq[ParsedAst.Pattern]) extends ParsedAst.Pattern

  }

  /**
   * An AST node that represent an unresolved predicate.
   *
   * @param loc the location.
   * @param name the unresolved name of the predicate.
   * @param terms the terms of the predicate.
   */
  case class Predicate(loc: SourceLocation, name: Name.Unresolved, terms: Seq[ParsedAst.Term]) extends ParsedAst

  /**
   * AST nodes for Terms.
   */
  sealed trait Term extends ParsedAst

  object Term {

    /**
     * An AST node that represent a wildcard variable term.
     *
     * @param loc the source location of the wildcard.
     */
    case class Wildcard(loc: SourceLocation) extends ParsedAst.Term

    /**
     * An AST node that represent a variable term.
     *
     * @param loc the location.
     * @param ident the variable identifier.
     */
    case class Var(loc: SourceLocation, ident: Name.Ident) extends ParsedAst.Term

    /**
     * An AST node that represent a literal term.
     *
     * @param loc the location.
     * @param literal the literal.
     */
    case class Lit(loc: SourceLocation, literal: ParsedAst.Literal) extends ParsedAst.Term

    /**
     * An AST node that represents an ascribed term.
     *
     * @param loc the location.
     * @param term the term.
     * @param tpe the type.
     */
    case class Ascribe(loc: SourceLocation, term: ParsedAst.Term, tpe: ParsedAst.Type) extends ParsedAst.Term

    /**
     * An AST node that represent a function application term
     *
     * @param loc the location.
     * @param name the unresolved name of the function.
     * @param args the arguments to the function.
     */
    case class Apply(loc: SourceLocation, name: Name.Unresolved, args: Seq[ParsedAst.Term]) extends ParsedAst.Term

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
     * An AST node that represent an unresolved type.
     *
     * @param name the ambiguous name.
     */
    case class Var(name: Name.Unresolved) extends ParsedAst.Type

    /**
     * An AST node that represent a function type.
     *
     * @param formals the type of the arguments.
     * @param retTpe the return type.
     */
    case class Function(formals: List[ParsedAst.Type], retTpe: ParsedAst.Type) extends ParsedAst.Type

    /**
     * An AST node that represents a tagged type.
     *
     * @param ident the tag name.
     * @param tpe the type of nested components.
     */
    case class Tag(ident: Name.Ident, tpe: ParsedAst.Type) extends ParsedAst.Type

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
    case class Parametric(name: Name.Unresolved, elms: Seq[ParsedAst.Type]) extends ParsedAst.Type

  }

  /**
   * An AST node that represents an attribute.
   *
   * @param ident the name of the attribute.
   * @param tpe the type of the attribute.
   */
  // TODO: Lattice notion
  case class Attribute(ident: Name.Ident, tpe: ParsedAst.Type) extends ParsedAst

  /**
   * An AST node representing a formal argument of a function.
   *
   * @param ident the name of the argument.
   * @param tpe the type of the argument.
   */
  case class FormalArg(ident: Name.Ident, tpe: ParsedAst.Type) extends ParsedAst

  /**
   * An AST node that represent a trait.
   *
   * @param ident the name of the trait.
   * @param name the value passed to the trait.
   */
  // TODO: Find a better name or eliminate entirely...
  case class Trait(ident: Name.Ident, name: Name.Unresolved) extends ParsedAst

}
