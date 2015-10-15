package ca.uwaterloo.flix.language.ast

trait ResolvedAst

// TODO: DOC

object ResolvedAst {

  // TODO: Add directives.
  case class Root(constants: Map[Name.Resolved, ResolvedAst.Definition.Constant],
                  directives: List[ResolvedAst.Directive],
                  enums: Map[Name.Resolved, ResolvedAst.Definition.Enum],
                  lattices: Map[ResolvedAst.Type, ResolvedAst.Definition.BoundedLattice],
                  collections: Map[Name.Resolved, ResolvedAst.Collection],
                  facts: List[ResolvedAst.Constraint.Fact],
                  rules: List[ResolvedAst.Constraint.Rule]) extends ResolvedAst

  sealed trait Definition

  object Definition {

    /**
     * A resolved AST node representing a constant definition.
     *
     * @param name the name of the constant.
     * @param exp the constant expression.
     * @param tpe the (declared) type of the constant.
     * @param loc the location.
     */
    case class Constant(name: Name.Resolved, exp: ResolvedAst.Expression, tpe: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Definition

    //  TODO: DOC
    case class Enum(name: Name.Resolved, cases: Map[String, ResolvedAst.Type.Tag], loc: SourceLocation) extends ResolvedAst.Definition

    /**
     * A resolved AST node that represents a bounded lattice definition.
     *
     * @param tpe the type of the lattice elements.
     * @param bot the bot element.
     * @param top the top element.
     * @param leq the partial order.
     * @param lub the least upper bound.
     * @param glb the greatest lower bound.
     * @param loc the source location.
     */
    case class BoundedLattice(tpe: ResolvedAst.Type, bot: ResolvedAst.Expression, top: ResolvedAst.Expression, leq: ResolvedAst.Expression,
                              lub: ResolvedAst.Expression, glb: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Definition

  }

  /**
   * A common super-type for collections that are either relations or lattices.
   */
  sealed trait Collection extends ResolvedAst.Definition

  object Collection {

    /**
     * A resolved AST node representing a relation definition.
     *
     * @param name the name of the relation.
     * @param attributes the attributes of the relation.
     * @param loc the location.
     */
    case class Relation(name: Name.Resolved, attributes: List[ResolvedAst.Attribute], loc: SourceLocation) extends ResolvedAst.Collection

    /**
     * A resolved AST node representing a lattice definition.
     *
     * @param name the name of the relation.
     * @param keys the keys of the lattice.
     * @param values the values of the lattice.
     * @param loc the location.
     */
    case class Lattice(name: Name.Resolved, keys: List[ResolvedAst.Attribute], values: List[ResolvedAst.Attribute], loc: SourceLocation) extends ResolvedAst.Collection

  }

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

  sealed trait Directive

  object Directive {

    case class AssertFact(fact: ResolvedAst.Constraint.Fact, loc: SourceLocation) extends ResolvedAst.Directive

    case class AssertRule(rule: ResolvedAst.Constraint.Rule, loc: SourceLocation) extends ResolvedAst.Directive

    case class Print(name: Name.Resolved, loc: SourceLocation) extends ResolvedAst.Directive

  }

  sealed trait Literal

  object Literal {

    case class Unit(loc: SourceLocation) extends ResolvedAst.Literal

    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends ResolvedAst.Literal

    case class Int(lit: scala.Int, loc: SourceLocation) extends ResolvedAst.Literal

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ResolvedAst.Literal

    case class Tag(name: Name.Resolved, ident: Name.Ident, lit: ResolvedAst.Literal, loc: SourceLocation) extends ResolvedAst.Literal

    case class Tuple(elms: List[ResolvedAst.Literal], loc: SourceLocation) extends ResolvedAst.Literal

  }

  sealed trait Expression extends Definition {
    def loc: SourceLocation
  }

  object Expression {

    case class Var(ident: Name.Ident, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ref(name: Name.Resolved, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lit(literal: ResolvedAst.Literal, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lambda(formals: List[FormalArg], retTpe: ResolvedAst.Type, body: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Apply(lambda: ResolvedAst.Expression, args: Seq[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Expression

    case class Unary(op: UnaryOperator, e: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Binary(op: BinaryOperator, e1: ResolvedAst.Expression, e2: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class IfThenElse(e1: ResolvedAst.Expression, e2: ResolvedAst.Expression, e3: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Let(ident: Name.Ident, value: ResolvedAst.Expression, body: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Match(e: ResolvedAst.Expression, rules: List[(ResolvedAst.Pattern, ResolvedAst.Expression)], loc: SourceLocation) extends ResolvedAst.Expression

    case class Tag(name: Name.Resolved, ident: Name.Ident, e: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tuple(elms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Expression

    case class Ascribe(e: ResolvedAst.Expression, tpe: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Expression

    case class Error(tpe: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Expression

  }

  /**
   * A common super-type for resolved patterns.
   */
  sealed trait Pattern extends ResolvedAst {

  }

  object Pattern {

    /**
     * An AST node representing a wildcard pattern.
     *
     * @param loc the source location.
     */
    case class Wildcard(loc: SourceLocation) extends ResolvedAst.Pattern

    /**
     * An AST node representing a variable pattern.
     *
     * @param ident the variable name.
     * @param loc the location.
     */
    case class Var(ident: Name.Ident, loc: SourceLocation) extends ResolvedAst.Pattern

    /**
     * An AST node representing a literal pattern.
     *
     * @param lit the literal.
     * @param loc the location.
     */
    case class Lit(lit: ResolvedAst.Literal, loc: SourceLocation) extends ResolvedAst.Pattern

    /**
     * An AST node representing a tagged pattern.
     *
     * @param name the name of the enum.
     * @param ident the name of the tag.
     * @param pat the nested pattern.
     * @param loc the location.
     */
    case class Tag(name: Name.Resolved, ident: Name.Ident, pat: ResolvedAst.Pattern, loc: SourceLocation) extends ResolvedAst.Pattern

    /**
     * An AST node representing a tuple pattern.
     *
     * @param elms the elements of the tuple.
     * @param loc the location.
     */
    case class Tuple(elms: List[ResolvedAst.Pattern], loc: SourceLocation) extends ResolvedAst.Pattern

  }

  sealed trait Predicate

  object Predicate {

    /**
     * A common super-type for head predicates.
     */
    sealed trait Head extends ResolvedAst.Predicate

    object Head {

      /**
       * A relational predicate that occurs in the head of a fact/rule.
       *
       * @param name the name of the relation.
       * @param terms the terms of the predicate.
       * @param loc the source location.
       */
      case class Relation(name: Name.Resolved, terms: List[ResolvedAst.Term.Head], loc: SourceLocation) extends ResolvedAst.Predicate.Head

      /**
       * A special trace predicate that occurs in the head of a rule.
       *
       * @param terms the terms of the predicate.
       * @param loc the source location.
       */
      case class Trace(terms: List[ResolvedAst.Term.Head], loc: SourceLocation) extends ResolvedAst.Predicate.Head

      /**
       * A special write predicate that occurs in the head of a fact/rule.
       *
       * @param terms the terms of the predicate.
       * @param path the path to write to.
       * @param loc the source location.
       */
      case class Write(terms: List[ResolvedAst.Term.Head], path: ResolvedAst.Term.Head, loc: SourceLocation) extends ResolvedAst.Predicate.Head

      /**
       * A special error predicate that occurs in the head of a fact/rule.
       *
       * @param terms the terms of the predicate.
       * @param loc the source location.
       */
      case class Error(terms: List[ResolvedAst.Term.Head], loc: SourceLocation) extends ResolvedAst.Predicate.Head

    }

    /**
     * A common super-type for body predicates.
     */
    sealed trait Body extends ResolvedAst.Predicate

    object Body {

      /**
       * A relational predicate that occurs in the body of a rule.
       *
       * @param name the name of the relation.
       * @param terms the terms of the predicate.
       * @param loc the source location.
       */
      case class Relation(name: Name.Resolved, terms: List[ResolvedAst.Term.Body], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      /**
       * A functional predicate that occurs in the body of a rule.
       *
       * @param name the name of the function.
       * @param terms the terms of the predicate.
       * @param loc the source location.
       */
      case class Function(name: Name.Resolved, terms: List[ResolvedAst.Term.Body], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      /**
       * A not equal predicate that occurs in the body of a rule.
       *
       * @param ident1 the name of the first variable.
       * @param ident2 the name of the second variable.
       * @param loc the source location.
       */
      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends ResolvedAst.Predicate.Body

      /**
       * A special read predicate that occurs in the body of a rule.
       */
      case class Read(terms: List[ResolvedAst.Term.Body], path: ResolvedAst.Term.Body, loc: SourceLocation) extends ResolvedAst.Predicate.Body

    }

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
       * @param loc the location.
       */
      case class Var(ident: Name.Ident, loc: SourceLocation) extends ResolvedAst.Term.Head

      /**
       * An AST node representing a literal term.
       *
       * @param literal the literal.
       * @param loc the location.
       */
      case class Lit(literal: ResolvedAst.Literal, loc: SourceLocation) extends ResolvedAst.Term.Head

      /**
       * An AST node representing an ascribed term.
       *
       * @param term the ascribed term.
       * @param tpe the ascribed type.
       * @param loc the location.
       */
      case class Ascribe(term: ResolvedAst.Term.Head, tpe: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Term.Head

      /**
       * An AST node representing a function call term.
       *
       * @param name the name of the called function.
       * @param args the arguments to the function.
       * @param loc the location.
       */
      case class Apply(name: Name.Resolved, args: List[ResolvedAst.Term.Head], loc: SourceLocation) extends ResolvedAst.Term.Head

    }

    /**
     * A common super-type for terms that are allowed to appear in a body predicate.
     */
    sealed trait Body extends ResolvedAst

    object Body {

      /**
       * An AST node representing a wildcard term.
       *
       * @param loc the location of the wildcard.
       */
      case class Wildcard(loc: SourceLocation) extends ResolvedAst.Term.Body

      /**
       * An AST node representing a variable term.
       *
       * @param ident the variable name.
       * @param loc the location.
       */
      case class Var(ident: Name.Ident, loc: SourceLocation) extends ResolvedAst.Term.Body

      /**
       * An AST node representing a literal term.
       *
       * @param literal the literal.
       * @param loc the location.
       */
      case class Lit(literal: ResolvedAst.Literal, loc: SourceLocation) extends ResolvedAst.Term.Body

      /**
       * An AST node representing an ascribed term.
       *
       * @param term the ascribed term.
       * @param tpe the ascribed type.
       * @param loc the location.
       */
      case class Ascribe(term: ResolvedAst.Term.Body, tpe: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Term.Body

    }

  }

  /**
   * A common super-type for resolved types.
   */
  sealed trait Type extends ResolvedAst

  object Type {

    /**
     * An AST node representing the Unit type.
     */
    case object Unit extends ResolvedAst.Type

    /**
     * An AST node representing the Boolean type.
     */
    case object Bool extends ResolvedAst.Type

    /**
     * An AST node representing the Integer type.
     */
    case object Int extends ResolvedAst.Type

    /**
     * An AST node representing the String type.
     */
    case object Str extends ResolvedAst.Type

    /**
     * An AST node representing a type tag.
     *
     * @param name the name of the enum.
     * @param ident the name of the tag.
     * @param tpe the nested type.
     */
    case class Tag(name: Name.Resolved, ident: Name.Ident, tpe: ResolvedAst.Type) extends ResolvedAst.Type

    /**
     * An AST node representing an enum type (a set of tags).
     *
     * @param cases a map from tag names to tag types.
     */
    case class Enum(cases: Map[String, ResolvedAst.Type.Tag]) extends ResolvedAst.Type

    /**
     * An AST node representing a tuple type.
     *
     * @param elms the type of the elements.
     */
    case class Tuple(elms: List[ResolvedAst.Type]) extends ResolvedAst.Type

    /**
     * An AST node representing a function type.
     *
     * @param args the argument types.
     * @param retTpe the return type.
     */
    case class Function(args: List[ResolvedAst.Type], retTpe: ResolvedAst.Type) extends ResolvedAst.Type

  }

  /**
   * A typed AST node representing an attribute in a relation.
   *
   * @param ident the name of the attribute.
   * @param tpe the (declared) type of the attribute.
   */
  case class Attribute(ident: Name.Ident, tpe: ResolvedAst.Type) extends ResolvedAst

  /**
   * A resolved AST node representing a formal argument in a function.
   *
   * @param ident the name of the argument.
   * @param tpe the type of the argument.
   */
  case class FormalArg(ident: Name.Ident, tpe: ResolvedAst.Type) extends ResolvedAst

}