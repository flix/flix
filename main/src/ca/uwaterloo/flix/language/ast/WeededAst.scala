package ca.uwaterloo.flix.language.ast

/**
 * A common super-type for weeded AST nodes.
 */
trait WeededAst

object WeededAst {

  /**
   * The AST root node.
   *
   * @param declarations the declarations in the AST.
   */
  case class Root(declarations: List[WeededAst.Declaration]) extends WeededAst

  /**
   * A common super-type for AST nodes that represents declarations.
   */
  sealed trait Declaration extends WeededAst {
    /**
     * Returns the source location of `this` declaration.
     */
    def loc: SourceLocation
  }

  object Declaration {

    /**
     * An AST node that represents a namespace declaration.
     *
     * @param name the name of the namespace.
     * @param body the nested declarations.
     * @param loc the source location.
     */
    case class Namespace(name: Name.Unresolved, body: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    /**
     * An AST node that a fact declaration.
     *
     * @param head the head predicate.
     * @param loc the source location.
     */
    case class Fact(head: WeededAst.Predicate.Head, loc: SourceLocation) extends WeededAst.Declaration

    /**
     * An AST node that represents a rule declaration.
     *
     * @param head the head predicate.
     * @param body the body predicate.
     * @param loc the source location.
     */
    case class Rule(head: WeededAst.Predicate.Head, body: List[WeededAst.Predicate.Body], loc: SourceLocation) extends WeededAst.Declaration

  }

  /**
   * A common super-type for AST nodes that represents definitions.
   */
  sealed trait Definition extends WeededAst.Declaration

  object Definition {

    /**
     * An AST node that represent a constant definition.
     *
     * @param ident the name of the constant.
     * @param e the named expression.
     * @param tpe the declared type of the expression.
     * @param loc the source location.
     */
    case class Constant(ident: Name.Ident, e: WeededAst.Expression, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Definition

    /**
     * An AST node that represents an enum definition.
     *
     * @param ident the name of the enum.
     * @param cases the cases of the enum.
     * @param loc the source location.
     */
    case class Enum(ident: Name.Ident, cases: Map[String, WeededAst.Type.Tag], loc: SourceLocation) extends WeededAst.Definition

    /**
     * An AST node that represents a lattice definition.
     *
     * @param tpe the type of the lattice elements.
     * @param bot the bottom element.
     * @param leq the less-than-equal function.
     * @param lub the least-upper-bound function.
     * @param loc the source location.
     */
    case class Lattice(tpe: WeededAst.Type, bot: WeededAst.Expression, leq: WeededAst.Expression, lub: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Definition

    /**
     * An AST node that represents a relation definition.
     *
     * @param ident the name of the relation.
     * @param attributes the attributes of the relation.
     * @param loc the source location of the relation.
     */
    case class Relation(ident: Name.Ident, attributes: List[WeededAst.Attribute], loc: SourceLocation) extends WeededAst.Definition

  }

  /**
   * A common super-type for AST nodes that represent directives.
   */
  sealed trait Directive extends WeededAst.Declaration {
    /**
     * Returns the source location of `this` directive.
     */
    def loc: SourceLocation
  }

  object Directive {

    /**
     * An AST node that represents an asserted fact.
     *
     * @param fact the asserted fact.
     * @param loc the source location.
     */
    case class AssertFact(fact: WeededAst.Declaration.Fact, loc: SourceLocation) extends WeededAst.Directive

    /**
     * An AST node that represents an asserted rule.
     *
     * @param rule the asserted rule.
     * @param loc the source location.
     */
    case class AssertRule(rule: WeededAst.Declaration.Rule, loc: SourceLocation) extends WeededAst.Directive

    /**
     * An AST node that represents a directive to print a relation.
     *
     * @param name the name of the relation.
     * @param loc the source location.
     */
    case class Print(name: Name.Unresolved, loc: SourceLocation) extends WeededAst.Directive

  }

  /**
   * A common super-type for AST node that represents literals.
   */
  sealed trait Literal extends WeededAst {
    /**
     * Returns the source location of `this` literal.
     */
    def loc: SourceLocation
  }

  object Literal {

    /**
     * An AST node that represents a unit literal
     *
     * @param loc the source location of the literal.
     */
    case class Unit(loc: SourceLocation) extends WeededAst.Literal

    /**
     * An AST node that represents a boolean literal.
     *
     * @param lit the boolean literal.
     * @param loc the source location of the literal.
     */
    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends WeededAst.Literal

    /**
     * An AST node that represents an integer literal.
     *
     * @param lit the integer literal.
     * @param loc the source location of the literal
     */
    case class Int(lit: scala.Int, loc: SourceLocation) extends WeededAst.Literal

    /**
     * An AST node that represents a string literal.
     *
     * @param lit the string literal.
     * @param loc the source location of the literal.
     */
    case class Str(lit: java.lang.String, loc: SourceLocation) extends WeededAst.Literal

    /**
     * An AST node that represents a tagged literal.
     *
     * @param enum the enum name.
     * @param tag the tag name.
     * @param literal the literal
     * @param loc the source location of the literal.
     */
    case class Tag(enum: Name.Unresolved, tag: Name.Ident, literal: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Literal

    /**
     * An AST node that represents a tuple literal.
     *
     * @param elms the elements of the tuple.
     * @param loc the source location of the literal.
     */
    case class Tuple(elms: List[WeededAst.Literal], loc: SourceLocation) extends WeededAst.Literal

  }

  sealed trait Expression extends WeededAst

  object Expression {

    case class Lit(literal: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Expression

    case class Var(name: Name.Unresolved, loc: SourceLocation) extends WeededAst.Expression

    case class Apply(lambda: WeededAst.Expression, arguments: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Lambda(formals: List[WeededAst.FormalArg], body: WeededAst.Expression, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class Unary(op: UnaryOperator, e: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Binary(op: BinaryOperator, e1: WeededAst.Expression, e2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class IfThenElse(e1: WeededAst.Expression, e2: WeededAst.Expression, e3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Let(ident: Name.Ident, value: WeededAst.Expression, body: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Match(e: WeededAst.Expression, rs: List[(WeededAst.Pattern, WeededAst.Expression)], loc: SourceLocation) extends WeededAst.Expression

    case class Tag(name: Name.Unresolved, ident: Name.Ident, e: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Tuple(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Ascribe(e: WeededAst.Expression, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class Error(tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

  }

  sealed trait Pattern extends WeededAst {

    def loc: SourceLocation

    /**
     * Returns the set of free variables in `this` pattern.
     */
    final def freeVars: Set[String] = this match {
      case WeededAst.Pattern.Wildcard(_) => Set.empty
      case WeededAst.Pattern.Var(ident, loc) => Set(ident.name)
      case WeededAst.Pattern.Lit(lit, loc) => Set.empty
      case WeededAst.Pattern.Tag(_name, ident, p, loc) => p.freeVars
      case WeededAst.Pattern.Tuple(elms, loc) => elms.foldLeft(Set.empty[String]) {
        case (acc, pat) => acc ++ pat.freeVars
      }
    }
  }

  object Pattern {

    case class Wildcard(loc: SourceLocation) extends WeededAst.Pattern

    case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Pattern

    case class Lit(literal: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Pattern

    case class Tag(name: Name.Unresolved, ident: Name.Ident, p: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

    case class Tuple(elms: List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

  }

  sealed trait Predicate extends WeededAst

  object Predicate {

    sealed trait Head extends WeededAst.Predicate

    object Head {

      case class Relation(name: Name.Unresolved, terms: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Predicate.Head

      case class Print(terms: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Predicate.Head

      case class Write(terms: List[WeededAst.Term.Head], path: WeededAst.Term.Head, loc: SourceLocation) extends WeededAst.Predicate.Head

      case class Error(terms: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Predicate.Head

    }


    sealed trait Body extends WeededAst.Predicate

    object Body {

      case class FunctionOrRelation(name: Name.Unresolved, terms: List[WeededAst.Term.Body], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Read(terms: List[WeededAst.Term.Body], path: WeededAst.Term.Body, loc: SourceLocation) extends WeededAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends WeededAst {
      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Term.Head

      case class Lit(literal: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Term.Head

      case class Apply(name: Name.Unresolved, args: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Term.Head

      case class Ascribe(term: Head, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Term.Head

    }

    sealed trait Body extends WeededAst {
      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(loc: SourceLocation) extends WeededAst.Term.Body

      case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Term.Body

      case class Lit(literal: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Term.Body

      case class Ascribe(term: Body, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Term.Body

    }

  }


  sealed trait Type

  object Type {

    case object Unit extends WeededAst.Type

    case class Ambiguous(name: Name.Unresolved) extends WeededAst.Type

    case class Tag(tagName: Name.Ident, tpe: WeededAst.Type) extends WeededAst.Type

    case class Enum(name: Name.Resolved, cases: Map[String, WeededAst.Type.Tag]) extends WeededAst.Type

    case class Tuple(elms: List[WeededAst.Type]) extends WeededAst.Type

    case class Function(args: List[WeededAst.Type], retType: WeededAst.Type) extends WeededAst.Type

  }

  case class Attribute(ident: Name.Ident, tpe: WeededAst.Type, interp: WeededAst.Interpretation) extends WeededAst

  sealed trait Interpretation

  object Interpretation {

    case object Set extends WeededAst.Interpretation

    case object Lattice extends WeededAst.Interpretation

  }

  /**
   * An AST node representing a formal argument of a function.
   *
   * @param ident the name of the argument.
   * @param tpe the type of the argument.
   */
  case class FormalArg(ident: Name.Ident, tpe: WeededAst.Type) extends WeededAst


}

// TODO: DOC
