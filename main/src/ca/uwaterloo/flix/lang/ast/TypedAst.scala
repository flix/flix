package ca.uwaterloo.flix.lang.ast

import ca.uwaterloo.flix.lang.Compiler

// TODO: if there is going to be an optimized IR then all these helper methods such be moved to that IR.

/**
 * A common super-type for typed AST nodes.
 */
sealed trait TypedAst

object TypedAst {

  /**
   * A typed AST node representing the root of the entire AST.
   *
   * @param declarations the top-level declarations.
   */
  case class Root(declarations: List[TypedAst.Declaration]) extends TypedAst {

    /**
     * Returns all facts declared anywhere in the AST.
     */
    lazy val facts: List[TypedAst.Declaration.Fact] = {
      def visit(d: TypedAst.Declaration): List[TypedAst.Declaration.Fact] = d match {
        case TypedAst.Declaration.Namespace(_, body) => body flatMap visit
        case f: TypedAst.Declaration.Fact => List(f)
        case _ => List.empty
      }
      declarations flatMap visit
    }

    /**
     * Returns all rules declared anywhere in the AST.
     */
    lazy val rules: List[TypedAst.Declaration.Rule] = {
      def visit(d: TypedAst.Declaration): List[TypedAst.Declaration.Rule] = d match {
        case TypedAst.Declaration.Namespace(_, body) => body flatMap visit
        case r: TypedAst.Declaration.Rule => List(r)
        case _ => List.empty
      }
      declarations flatMap visit
    }

    /**
     * Returns all relations declared anywhere in the AST.
     */
    lazy val relations: List[TypedAst.Definition.Relation] = {
      def visit(d: TypedAst.Declaration): List[TypedAst.Definition.Relation] = d match {
        case TypedAst.Declaration.Namespace(_, body) => body flatMap visit
        case r: TypedAst.Definition.Relation => List(r)
        case _ => List.empty
      }
      declarations flatMap visit
    }

    // TODO: Need this?
    // TODO: Build mutable symbol table upon construction of the root?
    // and then pass the root around?
    // def lookup(name: ResolvedAst.RName): TypedAst.Definition = ???
  }

  /**
   * A common super-type for typed declarations.
   */
  sealed trait Declaration extends TypedAst

  object Declaration {

    /**
     * A typed AST node representing a namespace declaration.
     *
     * @param name the name of the namespace.
     * @param body the nested declarations.
     */
    case class Namespace(name: ResolvedAst.RName, body: List[TypedAst.Declaration]) extends TypedAst.Definition

    /**
     * A typed AST node representing a fact declaration.
     *
     * @param head the head predicate.
     */
    case class Fact(head: TypedAst.Predicate.WithApply) extends TypedAst.Declaration

    /**
     * A typed AST node representing a rule declaration.
     *
     * @param head the head predicate.
     * @param body the body predicates.
     */
    case class Rule(head: TypedAst.Predicate.WithApply, body: List[WeededAst.PredicateNoApply]) extends TypedAst.Declaration

  }

  /**
   * A common super-type for typed definitions.
   *
   * A definition is a kind-of declaration.
   */
  sealed trait Definition extends TypedAst.Declaration

  object Definition {

    //
    //    case class TypeAlias(ident: ParsedAst.Ident, tpe: WeededAst.Type) extends TypedAst.Definition
    //
    //    case class Value(ident: ParsedAst.Ident, tpe: WeededAst.Type, e: WeededAst.Expression) extends TypedAst.Definition
    //
    //    case class Function(ident: ParsedAst.Ident, formals: List[(ParsedAst.Ident, WeededAst.Type)], tpe: WeededAst.Type, body: WeededAst.Expression) extends TypedAst.Definition
    //
    //    case class Enum(ident: ParsedAst.Ident, cases: Map[String, ParsedAst.Type.Tag]) extends TypedAst.Definition
    //
    //    case class Lattice(ident: ParsedAst.Ident, elms: List[ParsedAst.QName], traits: List[ParsedAst.Trait]) extends TypedAst.Definition
    //
    //    case class JoinSemiLattice(ident: ParsedAst.Ident,
    //                               bot: ParsedAst.QName,
    //                               leq: ParsedAst.QName,
    //                               lub: ParsedAst.QName,
    //                               norm: Option[ParsedAst.QName],
    //                               widen: Option[ParsedAst.QName]) extends TypedAst.Definition
    //

    /**
     * A typed AST node representing a relation definition.
     *
     * @param ident the name of the relation.
     * @param attributes the attributes (columns) of the relation.
     */
    case class Relation(ident: ParsedAst.Ident, attributes: List[TypedAst.Attribute]) extends TypedAst.Definition {
      /**
       * Returns the attribute with the given `name`.
       */
      def attribute(name: String): TypedAst.Attribute = attributes find {
        case TypedAst.Attribute(attributeIdent) => attributeIdent.name == name
      } getOrElse {
        throw Compiler.InternalCompilerError(s"Attribute '$name' does not exist.", ident.location)
      }
    }

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
     * @param ident the tag name.
     * @param literal the nested literal.
     * @param tpe the type of the tag.
     * @param defn the definition of the enum.
     */
    case class Tag(ident: ParsedAst.Ident, literal: TypedAst.Literal, tpe: TypedAst.Type.Enum, defn: WeededAst.Definition.Enum) extends TypedAst.Literal

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

    case class Var(name: String, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Ref(name: ResolvedAst.RName, decl: WeededAst.Definition, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Apply(name: ResolvedAst.RName, arguments: Seq[TypedAst.Expression], tpe: TypedAst.Type) extends TypedAst.Expression

    case class Lit(literal: TypedAst.Literal, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Lambda(formals: Seq[(ParsedAst.Ident, TypedAst.Type)], returnType: TypedAst.Type, body: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Unary(op: UnaryOperator, e: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Binary(e1: TypedAst.Expression, op: BinaryOperator, e2: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class IfThenElse(e1: TypedAst.Expression, e2: TypedAst.Expression, e3: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Let(ident: ParsedAst.Ident, value: TypedAst.Expression, body: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Match(e: TypedAst.Expression, rules: Seq[(TypedAst.Pattern, TypedAst.Expression)], tpe: TypedAst.Type) extends TypedAst.Expression

    case class Tag(name: ResolvedAst.RName, ident: ParsedAst.Ident, e: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Tuple(elms: Seq[TypedAst.Expression], tpe: TypedAst.Type) extends TypedAst.Expression

    case class Ascribe(e: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression

    case class Error(location: SourceLocation, tpe: TypedAst.Type) extends TypedAst.Expression

  }

  /**
   * A common-super type for typed patterns.
   */
  sealed trait Pattern extends TypedAst

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
     * @param name the resolved name of the enum.
     * @param ident the tag name.
     * @param pat the nested pattern.
     * @param tpe the type of the tag.
     */
    case class Tag(name: ResolvedAst.RName, ident: ParsedAst.Ident, pat: TypedAst.Pattern, tpe: TypedAst.Type.Tag) extends TypedAst.Pattern

    /**
     * A typed AST node representing a tuple pattern.
     *
     * @param elms the elements of the tuple.
     * @param tpe the type of the tuple.
     */
    case class Tuple(elms: List[TypedAst.Pattern], tpe: TypedAst.Type.Tuple) extends TypedAst.Pattern

  }

  /**
   *
   */
  sealed trait Predicate

  object Predicate {

    case class NoApply()

    case class WithApply()

  }

  sealed trait Term {

  }


  /**
   * A common super-type for types.
   */
  sealed trait Type extends TypedAst

  object Type {

    /**
     * An AST node that represents the Unit type.
     */
    case object Unit extends TypedAst.Type

    /**
     * An AST node that represents the Boolean type.
     */
    case object Bool extends TypedAst.Type

    /**
     * An AST node that represents the Integer type.
     */
    case object Int extends TypedAst.Type

    /**
     * An AST node that represents the String type.
     */
    case object Str extends TypedAst.Type


    case class Tag(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Type


    case class Enum(variants: Map[String, TypedAst.Type.Tag]) extends TypedAst.Type

    /**
     * An AST node that represents a tuple type.
     *
     * @param elms the types of the elements.
     */
    case class Tuple(elms: Seq[TypedAst.Type]) extends TypedAst.Type

    case class Parametric(name: ResolvedAst.RName, elms: Seq[TypedAst.Type]) extends TypedAst.Type

  }

  /**
   * A typed AST node representing an attribute in a relation.
   */
  case class Attribute(ident: ParsedAst.Ident) extends TypedAst

}
