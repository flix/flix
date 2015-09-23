package ca.uwaterloo.flix.lang.ast

import ca.uwaterloo.flix.lang.Compiler

/**
 * A common super-type for typed AST nodes.
 */
sealed trait TypedAst

object TypedAst {

  /**
   * A typed AST node representing the root of the entire AST.
   *
   * @param defns a map from resolved names to definitions.
   * @param facts a list of facts.
   * @param rules a list of rules.
   *              // TODO: What exactly should go here?
   */
  case class Root(defns: Map[Name.Resolved, TypedAst.Definition],
                  facts: List[TypedAst.Constraint.Fact],
                  rules: List[TypedAst.Constraint.Rule]) extends TypedAst {

  }

  /**
   * A common super-type for typed definitions.
   */
  sealed trait Definition

  object Definition {

    /**
     * A typed AST node representing a constant definition.
     *
     * @param name the name of the constant.
     * @param exp the constant expression.
     * @param tpe the type of the constant.
     */
    case class Constant(name: Name.Resolved, exp: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Definition

    /**
     * A typed AST node representing an enum definition.
     *
     * @param name the name of the enum.
     * @param cases the tags of the enum.
     * @param tpe the type of the enum.
     */
    case class Enum(name: Name.Resolved, cases: Map[String, ParsedAst.Type.Tag], tpe: TypedAst.Type.Enum) extends TypedAst.Definition

    /**
     * A typed AST node representing a join semi lattice definition.
     *
     * @param tpe the type of elements.
     * @param bot the bottom element.
     * @param leq the partial order.
     * @param lub the least-upper-bound.
     */
    // TODO: Do we need a global type class namespace? Type -> JoinSemiLattice?
    // TODO: These should have a different type. Probably Exp.
    case class JoinSemiLattice(tpe: TypedAst.Type, bot: Name.Resolved, leq: Name.Resolved, lub: Name.Resolved) extends TypedAst.Definition

    /**
     * A typed AST node representing a relation definition.
     *
     * @param name the name of the relation.
     * @param attributes the attributes (columns) of the relation.
     */
    case class Relation(name: Name.Resolved, attributes: List[TypedAst.Attribute]) extends TypedAst.Definition {
      /**
       * Returns the attribute with the given `name`.
       */
      def attribute(attribute: String): TypedAst.Attribute = attributes find {
        case TypedAst.Attribute(ident, tpe) => ident.name == attribute
      } getOrElse {
        throw Compiler.InternalCompilerError(s"Attribute '$name' does not exist.")
      }
    }

  }

  /**
   * A common super-type for typed facts and rules.
   */
  sealed trait Constraint extends TypedAst

  object Constraint {

    /**
     * A typed AST node representing a fact declaration.
     *
     * @param head the head predicate.
     */
    case class Fact(head: TypedAst.Predicate.Head) extends TypedAst.Constraint

    /**
     * A typed AST node representing a rule declaration.
     *
     * @param head the head predicate.
     * @param body the body predicates.
     */
    case class Rule(head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body]) extends TypedAst.Constraint

  }

  /**
   * A common super-type for typed literals.
   */
  sealed trait Literal extends TypedAst {
    /**
     * The type of the literal.
     */
    def tpe: TypedAst.Type
  }

  object Literal {

    /**
     * A typed AST node representing the unit literal.
     */
    case object Unit extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Unit
    }

    /**
     * A typed AST node representing a boolean literal.
     */
    case class Bool(literal: scala.Boolean) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Bool
    }

    /**
     * A typed AST node representing an integer literal.
     */
    case class Int(literal: scala.Int) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Int
    }

    /**
     * A typed AST node representing a string literal.
     */
    case class Str(literal: java.lang.String) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Str
    }

    /**
     * A typed AST node representing a tagged literal.
     *
     * @param name the enum name.
     * @param ident the tag name.
     * @param literal the nested literal.
     * @param tpe the type of the tag.
     */
    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, literal: TypedAst.Literal, tpe: TypedAst.Type.Enum) extends TypedAst.Literal

    /**
     * A typed AST node representing a tuple literal.
     * @param elms the elements of the tuple.
     * @param tpe the typed of the tuple.
     */
    case class Tuple(elms: List[TypedAst.Literal], tpe: TypedAst.Type.Tuple) extends TypedAst.Literal

  }

  sealed trait Expression extends TypedAst {
    /**
     * The type of the expression.
     */
    def tpe: Type
  }

  object Expression {

    /**
     * A typed AST node representing a literal expression.
     *
     * @param literal the literal.
     * @param tpe the type of the literal.
     */
    case class Lit(literal: TypedAst.Literal, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
     *
     * @param name the name of the variable.
     * @param tpe the type of the variable.
     */
    case class Var(name: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a reference to a definition (i.e. a value or function).
     *
     * @param name the name of the definition.
     * @param tpe the type of the definition.
     */
    case class Ref(name: Name.Resolved, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a lambda abstraction.
     *
     * @param formals the formal arguments.
     * @param retTpe the declared return type.
     * @param body the body expression of the lambda.
     * @param tpe the type of the entire function.
     */
    case class Lambda(formals: List[TypedAst.FormalArg], retTpe: TypedAst.Type, body: TypedAst.Expression, tpe: TypedAst.Type.Function) extends TypedAst.Expression

    /**
     * A typed AST node representing a function call.
     *
     * @param exp the lambda/function expression.
     * @param args the function arguments.
     * @param tpe the return type of the function.
     */
    case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a unary expression.
     *
     * @param op the unary operator.
     * @param exp the expression.
     * @param tpe the type
     */
    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a binary expression.
     *
     * @param op the binary operator.
     * @param exp1 the lhs expression.
     * @param exp2 the rhs expression.
     * @param tpe the type of the expression.
     */
    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing an if-then-else expression.
     *
     * @param exp1 the conditional expression.
     * @param exp2 the consequent expression.
     * @param e3 the alternative expression.
     * @param tpe the type of the consequent and alternative expressions.
     */
    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, e3: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a let expression.
     *
     * @param ident the name of the bound variable.
     * @param value the value of the bound variable.
     * @param body the body expression in which the bound variable is visible.
     * @param tpe the type of the expression (which is equivalent to the type of the body expression).
     */
    case class Let(ident: ParsedAst.Ident, value: TypedAst.Expression, body: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a match expression.
     *
     * @param exp the match expression.
     * @param rules the match rules.
     * @param tpe the type of the match expression (which is equivalent to the type of each rule).
     */
    case class Match(exp: TypedAst.Expression, rules: List[(TypedAst.Pattern, TypedAst.Expression)], tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing a tagged expression.
     *
     * @param name the name of the enum.
     * @param ident the name of the tag.
     * @param exp the expression.
     * @param tpe the type of the expression.
     */
    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, exp: TypedAst.Expression, tpe: TypedAst.Type.Enum) extends TypedAst.Expression

    /**
     * A typed AST node representing a tuple expression.
     *
     * @param elms the elements of the tuple.
     * @param tpe the type of the tuple.
     */
    case class Tuple(elms: List[TypedAst.Expression], tpe: TypedAst.Type) extends TypedAst.Expression

    /**
     * A typed AST node representing an error expression.
     *
     * @param location the location of the error expression.
     * @param tpe the type of the error expression.
     */
    case class Error(location: SourceLocation, tpe: TypedAst.Type) extends TypedAst.Expression

  }

  /**
   * A common-super type for typed patterns.
   */
  sealed trait Pattern extends TypedAst {
    /**
     * The type of the pattern.
     */
    def tpe: TypedAst.Type

    /**
     * Returns the bound variables (and their types).
     */
    def bound: Map[ParsedAst.Ident, TypedAst.Type] = {
      def visit(pat: TypedAst.Pattern, m: Map[ParsedAst.Ident, TypedAst.Type]): Map[ParsedAst.Ident, TypedAst.Type] =
        pat match {
          case TypedAst.Pattern.Wildcard(_) => m
          case TypedAst.Pattern.Var(ident, tpe) => m + (ident -> tpe)
          case TypedAst.Pattern.Lit(_, _) => m
          case TypedAst.Pattern.Tag(_, _, pat2, _) => visit(pat2, m)
          case TypedAst.Pattern.Tuple(elms, _) => elms.foldLeft(m) {
            case (macc, elm) => visit(elm, macc)
          }
        }

      visit(this, Map.empty)
    }
  }

  object Pattern {

    /**
     * A typed AST node representing a wildcard pattern.
     *
     * @param tpe the type of the wildcard variable.
     */
    case class Wildcard(tpe: TypedAst.Type) extends TypedAst.Pattern

    /**
     * A typed AST node representing a variable pattern.
     *
     * @param ident the name of the variable.
     * @param tpe the type of the variable.
     */
    case class Var(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Pattern

    /**
     * A typed AST node representing a literal pattern.
     *
     * @param lit the literal.
     * @param tpe the type of the literal.
     */
    case class Lit(lit: ResolvedAst.Literal, tpe: TypedAst.Type) extends TypedAst.Pattern

    /**
     * A typed AST node representing a tagged pattern.
     *
     * @param name the namespace of the tag.
     * @param ident the tag name.
     * @param pat the nested pattern.
     * @param tpe the type of the tag.
     */
    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, pat: TypedAst.Pattern, tpe: TypedAst.Type.Tag) extends TypedAst.Pattern

    /**
     * A typed AST node representing a tuple pattern.
     *
     * @param elms the elements of the tuple.
     * @param tpe the type of the tuple.
     */
    case class Tuple(elms: List[TypedAst.Pattern], tpe: TypedAst.Type.Tuple) extends TypedAst.Pattern

  }

  /**
   * A common super-type for typed predicates.
   */
  sealed trait Predicate extends TypedAst

  object Predicate {

    /**
     * A predicate that is allowed to occur in the head of a rule.
     *
     * @param name the name of the predicate.
     * @param terms the terms of the predicate.
     * @param tpe the type of the predicate.
     */
    case class Head(name: Name.Resolved, terms: List[TypedAst.Term.Head], tpe: TypedAst.Type) extends TypedAst.Predicate

    /**
     * A predicate that is allowed to occur in the body of a rule.
     *
     * @param name the name of the predicate.
     * @param terms the terms of the predicate.
     * @param tpe the type of the predicate.
     */
    case class Body(name: Name.Resolved, terms: List[TypedAst.Term.Body], tpe: TypedAst.Type) extends TypedAst.Predicate

  }

  object Term {

    /**
     * A common super-type for terms that are allowed appear in a head predicate.
     */
    sealed trait Head extends TypedAst

    object Head {

      /**
       * An AST node representing a variable term.
       *
       * @param ident the variable name.
       * @param tpe the type of the term.
       */
      case class Var(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Term.Head

      /**
       * An AST node representing a literal term.
       *
       * @param literal the literal.
       * @param tpe the type of the term.
       */
      case class Lit(literal: TypedAst.Literal, tpe: TypedAst.Type) extends TypedAst.Term.Head

      /**
       * An AST node representing a function call term.
       *
       * @param name the name of the called function.
       * @param args the arguments to the function.
       * @param tpe the type of the term.
       */
      case class Apply(name: Name.Resolved, args: List[TypedAst.Term.Head], tpe: TypedAst.Type) extends TypedAst.Term.Head

    }

    /**
     * A common super-type for terms that are allowed to appear in a body predicate.
     */
    sealed trait Body extends TypedAst

    object Body {

      /**
       * An AST node representing a wildcard term.
       *
       * @param location the location of the wildcard.
       * @param tpe the type of the term.
       */
      case class Wildcard(location: SourceLocation, tpe: TypedAst.Type) extends TypedAst.Term.Body

      /**
       * An AST node representing a variable term.
       *
       * @param ident the variable name.
       * @param tpe the type of the term.
       */
      case class Var(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Term.Body

      /**
       * An AST node representing a literal term.
       *
       * @param literal the literal.
       * @param tpe the type of the term.
       */
      case class Lit(literal: TypedAst.Literal, tpe: TypedAst.Type) extends TypedAst.Term.Body

    }

  }

  /**
   * A common super-type for types.
   */
  sealed trait Type extends TypedAst

  object Type {

    /**
     * An AST node representing the Unit type.
     */
    case object Unit extends TypedAst.Type

    /**
     * An AST node representing the Boolean type.
     */
    case object Bool extends TypedAst.Type

    /**
     * An AST node representing the Integer type.
     */
    case object Int extends TypedAst.Type

    /**
     * An AST node representing the String type.
     */
    case object Str extends TypedAst.Type

    /**
     * An AST node representing the type of a tag.
     *
     * @param name the namespace of the tag.
     * @param ident the name of the tag.
     * @param tpe the type of the nested value.
     */
    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Type

    /**
     * An AST node representing an enum type (a set of tags).
     *
     * @param variants a map from tag names to tag types.
     */
    case class Enum(variants: Map[String, TypedAst.Type.Tag]) extends TypedAst.Type

    /**
     * An AST node representing a tuple type.
     *
     * @param elms the types of the elements.
     */
    case class Tuple(elms: List[TypedAst.Type]) extends TypedAst.Type

    /**
     * An AST node representing a function type.
     *
     * @param args the type of the arguments.
     * @param retTpe the type of the return type.
     */
    case class Function(args: List[TypedAst.Type], retTpe: TypedAst.Type) extends TypedAst.Type

  }

  /**
   * A typed AST node representing an attribute in a relation.
   *
   * @param ident the name of the attribute.
   * @param tpe  the type of the attribute.
   */
  case class Attribute(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst

  /**
   * A typed AST node representing a formal argument in a function.
   *
   * @param ident the name of the argument.
   * @param tpe the type of the argument.
   */
  case class FormalArg(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst

}
