package ca.uwaterloo.flix.language.ast

import scala.collection.immutable.Seq

/**
 * A common-super type for all parsed AST nodes.
 */
sealed trait ParsedAst

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

  }

  /**
   * A common super-type for AST nodes that represent definitions.
   */
  sealed trait Definition extends Declaration {
    /**
     * Returns the source location of `this` definition.
     */
    def loc: SourceLocation
  }

  object Definition {

    /**
     * An AST node that represents a value definition.
     *
     * @param sp1 the position of the first character in the definition.
     * @param ident the name of the value.
     * @param tpe the declared type of the value.
     * @param e the expression.
     * @param sp2 the position of the last character in the definition.
     */
    case class Value(sp1: SourcePosition, ident: Name.Ident, tpe: ParsedAst.Type, e: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Definition {
      /**
       * Returns the source location of `this` definition.
       */
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a function definition.
     *
     * @param sp1 the position of the first character in the definition.
     * @param ident the name of the function.
     * @param formals the formals (i.e. parameters and their types).
     * @param tpe the return type.
     * @param body the body expression of the function.
     * @param sp2 the position of the last character in the definition.
     */
    case class Function(sp1: SourcePosition, ident: Name.Ident, formals: Seq[FormalArg], tpe: ParsedAst.Type, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Definition {
      /**
       * Returns the source location of `this` definition.
       */
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a enum definition.
     *
     * @param sp1 the position of the first character in the definition.
     * @param ident the name of the enum.
     * @param cases the variants of the enum.
     * @param sp2 the position of the last character in the definition.
     */
    case class Enum(sp1: SourcePosition, ident: Name.Ident, cases: Seq[ParsedAst.Type.Tag], sp2: SourcePosition) extends ParsedAst.Definition {
      /**
       * Returns the source location of `this` definition.
       */
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a lattice definition.
     *
     * @param sp1 the position of the first character in the definition.
     * @param tpe the type of the lattice elements.
     * @param elms the components of the lattice (e.g. bot, leq, lub).
     * @param sp2 the position of the last character in the definition.
     */
    case class Lattice(sp1: SourcePosition, tpe: ParsedAst.Type, elms: Seq[ParsedAst.Expression], traits: Seq[ParsedAst.Trait], sp2: SourcePosition) extends ParsedAst.Definition {
      /**
       * Returns the source location of `this` definition.
       */
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST that represent a relation definition.
     *
     * @param sp1 the position of the first character in the definition.
     * @param ident the name of the relation.
     * @param attributes the name and type of the attributes.
     * @param sp2 the position of the last character in the definition.
     */
    case class Relation(sp1: SourcePosition, ident: Name.Ident, attributes: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Definition {
      /**
       * Returns the source location of `this` definition.
       */
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  object Directive {
    // TODO
  }

  /**
   * AST nodes for Literals.
   */
  sealed trait Literal {
    /**
     * Returns the source location of `this` literal.
     */
    def loc: SourceLocation
  }

  object Literal {

    /**
     * An AST node that represents the Unit literal.
     *
     * @param sp1 the position of the first character in the literal.
     * @param sp2 the position of the last character in the literal.
     */
    case class Unit(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a boolean literal.
     *
     * @param sp1 the position of the first character in the literal.
     * @param lit the boolean literal.
     * @param sp2 the position of the last character in the literal.
     */
    case class Bool(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents an integer literal.
     *
     * @param sp1 the position of the first character in the literal.
     * @param lit the integer literal.
     * @param sp2 the position of the last character in the literal.
     */
    case class Int(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a string literal.
     *
     * @param sp1 the position of the first character in the literal.
     * @param lit the string literal.
     * @param sp2 the position of the last character in the literal.
     */
    case class Str(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a tagged literal.
     *
     * @param sp1 the position of the first character in the literal.
     * @param enum the name of the enum.
     * @param tag the name of the tag.
     * @param lit the nested literal.
     * @param sp2 the position of the last character in the literal.
     */
    case class Tag(sp1: SourcePosition, enum: Name.Unresolved, tag: Name.Ident, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Literal {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a tuple literal.
     *
     * @param sp1 the position of the first character in the literal.
     * @param elms the elements of the tuple.
     * @param sp2 the position of the last character in the literal.
     */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Literal], sp2: SourcePosition) extends ParsedAst.Literal {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

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
     * @param sp1 the position of the first character in the expression.
     * @param lit the literal.
     * @param sp2 the position of the last character in the expression.
     */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents an unresolved variable.
     *
     * @param sp1 the position of the first character in the expression.
     * @param name the ambiguous name.
     * @param sp2 the position of the last character in the expression.
     */
    case class Var(sp1: SourcePosition, name: Name.Unresolved, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a function application.
     *
     * @param sp1 the position of the first character in the expression.
     * @param lambda the lambda expression.
     * @param actuals the arguments.
     * @param sp2 the position of the last character in the expression.
     */
    case class Apply(sp1: SourcePosition, lambda: ParsedAst.Expression, actuals: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a lambda expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param formals the formals (i.e. parameters and their types).
     * @param tpe the return type.
     * @param body the body expression of the lambda.
     * @param sp2 the position of the last character in the expression.
     */
    case class Lambda(sp1: SourcePosition, formals: Seq[FormalArg], tpe: ParsedAst.Type, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents unary expressions.
     *
     * @param sp1 the position of the first character in the expression.
     * @param op the unary operator.
     * @param e the expression.
     * @param sp2 the position of the last character in the expression.
     */
    case class Unary(sp1: SourcePosition, op: UnaryOperator, e: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents binary expressions.
     *
     * @param e1 the left expression.
     * @param sp1 the position of the first character in the expression.
     * @param op the binary operator.
     * @param e2 the right expression.
     * @param sp2 the position of the last character in the expression.
     */
    case class Binary(e1: ParsedAst.Expression, sp1: SourcePosition, op: BinaryOperator, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents an if-then-else expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param e1 the conditional expression.
     * @param e2 the consequence expression.
     * @param e3 the alternative expression.
     * @param sp2 the position of the last character in the expression.
     */
    case class IfThenElse(sp1: SourcePosition, e1: ParsedAst.Expression, e2: ParsedAst.Expression, e3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a let-binding.
     *
     * @param sp1 the position of the first character in the expression.
     * @param ident the identifier to be bound.
     * @param value the expression whose value the identifier should be bound to.
     * @param body the expression in which the bound variable is visible.
     * @param sp2 the position of the last character in the expression.
     */
    case class Let(sp1: SourcePosition, ident: Name.Ident, value: ParsedAst.Expression, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a match expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param e the match expression.
     * @param rules the match rules and their bodies.
     * @param sp2 the position of the last character in the expression.
     */
    case class Match(sp1: SourcePosition, e: ParsedAst.Expression, rules: Seq[(ParsedAst.Pattern, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents an infix function call.
     *
     * @param e1 the first argument expression.
     * @param sp1 the position of the first character in the expression.
     * @param name the ambiguous name of the function.
     * @param e2 the second argument expression.
     * @param sp2 the position of the last character in the expression.
     */
    case class Infix(e1: ParsedAst.Expression, sp1: SourcePosition, name: Name.Unresolved, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a tagged expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param enumName the namespace of the enum.
     * @param tagName the tag name.
     * @param e the nested expression.
     * @param sp2 the position of the last character in the expression.
     */
    case class Tag(sp1: SourcePosition, enumName: Name.Unresolved, tagName: Name.Ident, e: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a tuple expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param elms the elements of the tuple.
     * @param sp2 the position of the last character in the expression.
     */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that ascribes a type to an expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param e the expression.
     * @param tpe the ascribed type.
     * @param sp2 the position of the last character in the expression.
     */
    case class Ascribe(sp1: SourcePosition, e: ParsedAst.Expression, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents an error expression.
     *
     * @param sp1 the position of the first character in the expression.
     * @param tpe the type of the error expression.
     * @param sp2 the position of the last character in the expression.
     */
    case class Error(sp1: SourcePosition, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Expression {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
   * AST nodes for Patterns.
   *
   * A pattern is like a literal except it may contain variables and wildcards.
   */
  sealed trait Pattern extends ParsedAst {
    /**
     * The source location of `this` pattern.
     */
    def loc: SourceLocation
  }

  object Pattern {

    /**
     * An AST node that represents a wildcard pattern.
     *
     * @param sp1 the position of the first character in the pattern.
     * @param sp2 the position of the last character in the pattern.
     */
    case class Wildcard(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a variable pattern.
     *
     * @param sp1 the position of the first character in the pattern.
     * @param ident the variable identifier.
     * @param sp2 the position of the last character in the pattern.
     */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Pattern {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a literal pattern.
     *
     * @param sp1 the position of the first character in the pattern.
     * @param lit the literal.
     * @param sp2 the position of the last character in the pattern.
     */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Pattern {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a tagged pattern.
     *
     * @param sp1 the position of the first character in the pattern.
     * @param enumName the enum name.
     * @param tagName the tag name.
     * @param p the nested pattern.
     * @param sp2 the position of the last character in the pattern.
     */
    case class Tag(sp1: SourcePosition, enumName: Name.Unresolved, tagName: Name.Ident, p: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents a tuple pattern.
     *
     * @param sp1 the position of the first character in the pattern.
     * @param elms the elements of the tuple.
     * @param sp2 the position of the last character in the pattern.
     */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
   * A common super-type for predicates.
   */
  sealed trait Predicate extends ParsedAst

  object Predicate {

    /**
     * An AST node that represent an unresolved predicate.
     *
     * @param sp1 the position of the first character in the term.
     * @param name the unresolved name of the predicate.
     * @param terms the terms of the predicate.
     * @param sp2 the position of the last character in the term.
     */
    case class Unresolved(sp1: SourcePosition, name: Name.Unresolved, terms: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Predicate {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    // TODO
    case class Alias() extends ParsedAst.Predicate

  }

  /**
   * AST nodes for Terms.
   */
  sealed trait Term extends ParsedAst {
    /**
     * Returns the source location of `this` term.
     */
    def loc: SourceLocation
  }

  object Term {

    /**
     * An AST node that represent a wildcard variable term.
     *
     * @param sp1 the position of the first character in the term.
     * @param sp2 the position of the last character in the term.
     */
    case class Wildcard(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Term {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represent a variable term.
     *
     * @param sp1 the position of the first character in the term.
     * @param ident the variable identifier.
     * @param sp2 the position of the last character in the term.
     */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Term {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represent a literal term.
     *
     * @param sp1 the position of the first character in the term.
     * @param lit the literal.
     * @param sp2 the position of the last character in the term.
     */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Term {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represents an ascribed term.
     *
     * @param sp1 the position of the first character in the term.
     * @param term the term.
     * @param tpe the type.
     * @param sp2 the position of the last character in the term.
     */
    case class Ascribe(sp1: SourcePosition, term: ParsedAst.Term, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Term {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
     * An AST node that represent a function application term
     *
     * @param sp1 the position of the first character in the term.
     * @param name the unresolved name of the function.
     * @param args the arguments to the function.
     * @param sp2 the position of the last character in the term.
     */
    case class Apply(sp1: SourcePosition, name: Name.Unresolved, args: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Term {
      val loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

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
   * @param interp the interpretation of the attribute.
   */
  case class Attribute(ident: Name.Ident, interp: Interpretation) extends ParsedAst

  /**
   * A common super-type for attribute interpretations.
   */
  sealed trait Interpretation {
    /**
     * The type of elements in `this` interpretation.
     */
    def tpe: ParsedAst.Type
  }

  object Interpretation {

    /**
     * An AST node representing the standard set-based interpretation of an attribute in a relation.
     *
     * @param tpe the type of the attribute.
     */
    case class Set(tpe: ParsedAst.Type) extends ParsedAst.Interpretation

    /**
     * An AST node representing a lattice-based interpretation of an attribute in a relation.
     *
     * @param tpe the type of the attribute.
     */
    case class Lattice(tpe: ParsedAst.Type) extends ParsedAst.Interpretation

  }

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
