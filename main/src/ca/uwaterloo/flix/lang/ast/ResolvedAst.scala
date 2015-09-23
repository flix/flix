package ca.uwaterloo.flix.lang.ast

trait ResolvedAst

object ResolvedAst {

  case class Root(
                   // TODO: value environment
                   // todo: relation environment
                   // todo: type environment
                   // todo: lattice environment
                   // todo: enum environment... sigh so many
                   enums: Map[Name.Resolved, ResolvedAst.Definition.Enum],
                   // relations: Map[Name.Resolved, ResolvedAst.Definition.Relation],
                   facts: List[ResolvedAst.Constraint.Fact],
                   rules: List[ResolvedAst.Constraint.Rule]) extends ResolvedAst

  sealed trait Constraint extends ResolvedAst

  object Constraint {

    /**
     * A resolved AST node representing a fact declaration.
     *
     * @param head the head predicate.
     */
    case class Fact(head: ResolvedAst.Predicate.Head) extends ResolvedAst.Constraint

    /**
     * A resolved AST node representing a rule declaration.
     *
     * @param head the head predicate.
     * @param body the body predicates.
     */
    case class Rule(head: ResolvedAst.Predicate.Head, body: List[ResolvedAst.Predicate.Body]) extends ResolvedAst.Constraint

  }

  sealed trait Definition extends ResolvedAst.Constraint

  object Definition {

    case class Value(name: Name.Resolved, exp: ResolvedAst.Expression, tpe: ResolvedAst.Type) extends ResolvedAst.Definition

    case class Enum(name: Name.Resolved, cases: Map[String, ResolvedAst.Type.Tag]) extends ResolvedAst.Definition

    case class Relation() extends ResolvedAst.Definition

  }

  sealed trait Literal

  object Literal {

    case object Unit extends ResolvedAst.Literal

    case class Bool(literal: scala.Boolean) extends ResolvedAst.Literal

    case class Int(literal: scala.Int) extends ResolvedAst.Literal

    case class Str(literal: java.lang.String) extends ResolvedAst.Literal

    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, literal: ResolvedAst.Literal) extends ResolvedAst.Literal

    case class Tuple(elms: List[ResolvedAst.Literal]) extends ResolvedAst.Literal

  }

  sealed trait Expression extends Definition

  object Expression {

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Expression

    case class Ref(name: Name.Resolved) extends ResolvedAst.Expression

    case class Lit(literal: ResolvedAst.Literal) extends ResolvedAst.Expression

    case class Lambda(formals: Seq[(ParsedAst.Ident, ResolvedAst.Type)], returnType: ResolvedAst.Type, body: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Apply(lambda: ResolvedAst.Expression, args: Seq[ResolvedAst.Expression]) extends ResolvedAst.Expression

    case class Unary(op: UnaryOperator, e: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Binary(op: BinaryOperator, e1: ResolvedAst.Expression, e2: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class IfThenElse(e1: ResolvedAst.Expression, e2: ResolvedAst.Expression, e3: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Let(ident: ParsedAst.Ident, value: ResolvedAst.Expression, body: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Match(e: ResolvedAst.Expression, rules: Seq[(ResolvedAst.Pattern, ResolvedAst.Expression)]) extends ResolvedAst.Expression

    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, e: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Tuple(elms: List[ResolvedAst.Expression]) extends ResolvedAst.Expression

    case class Ascribe(e: ResolvedAst.Expression, tpe: ResolvedAst.Type) extends ResolvedAst.Expression

    case class Error(location: SourceLocation) extends ResolvedAst.Expression

  }

  sealed trait Pattern extends ResolvedAst

  object Pattern {

    case class Wildcard(location: SourceLocation) extends ResolvedAst.Pattern

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Pattern

    case class Lit(literal: ResolvedAst.Literal) extends ResolvedAst.Pattern

    case class Tag(name: Name.Resolved, ident: ParsedAst.Ident, pat: ResolvedAst.Pattern) extends ResolvedAst.Pattern

    case class Tuple(elms: Seq[ResolvedAst.Pattern]) extends ResolvedAst.Pattern

  }


  // TODO: Filters

  object Predicate {

    /**
     * A predicate that is allowed to occur in the head of a rule.
     *
     * @param name the name of the predicate.
     * @param terms the terms of the predicate.
     */
    case class Head(name: Name.Resolved, terms: List[ResolvedAst.Term.Head])

    /**
     * A predicate that is allowed to occur in the body of a rule.
     *
     * @param name the name of the predicate.
     * @param terms the terms of the predicate.
     */
    case class Body(name: Name.Resolved, terms: List[ResolvedAst.Term.Body])

  }

  object Term {

    /**
     * A common super-type for terms that are allowed appear in a head predicate.
     */
    sealed trait Head extends ResolvedAst

    object Head {

      /**
       * An AST node representing a variable term.
       *
       * @param ident the variable name.
       */
      case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Term.Head

      /**
       * An AST node representing a literal term.
       *
       * @param literal the literal.
       */
      case class Lit(literal: ResolvedAst.Literal) extends ResolvedAst.Term.Head

      /**
       * An AST node representing a function call term.
       *
       * @param name the name of the called function.
       * @param args the arguments to the function.
       */
      case class Apply(name: Name.Resolved, args: List[ResolvedAst.Term.Head]) extends ResolvedAst.Term.Head

    }

    /**
     * A common super-type for terms that are allowed to appear in a body predicate.
     */
    sealed trait Body extends ResolvedAst

    object Body {

      /**
       * An AST node representing a wildcard term.
       *
       * @param location the location of the wildcard.
       */
      case class Wildcard(location: SourceLocation) extends ResolvedAst.Term.Body

      /**
       * An AST node representing a variable term.
       *
       * @param ident the variable name.
       */
      case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Term.Body

      /**
       * An AST node representing a literal term.
       *
       * @param literal the literal.
       */
      case class Lit(literal: ResolvedAst.Literal) extends ResolvedAst.Term.Body

    }

  }

  sealed trait Type extends ResolvedAst

  object Type {

    case object Unit extends ResolvedAst.Type

    case object Bool extends ResolvedAst.Type

    case object Int extends ResolvedAst.Type

    case object Str extends ResolvedAst.Type

    case class Tag(ident: ParsedAst.Ident, tpe: ResolvedAst.Type) extends ResolvedAst.Type

    case class Tuple(elms: List[ResolvedAst.Type]) extends ResolvedAst.Type

    case class Function(t1: ResolvedAst.Type, t2: ResolvedAst.Type) extends ResolvedAst.Type

    case class Parametric(name: Name.Resolved, elms: List[ResolvedAst.Type]) extends ResolvedAst.Type

    case class Lattice(tpe: ResolvedAst.Type) extends ResolvedAst.Type

  }

}