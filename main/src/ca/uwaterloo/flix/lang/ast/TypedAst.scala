package ca.uwaterloo.flix.lang.ast

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

  }

  /**
   * A common super-type for typed AST declarations.
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

  sealed trait Definition extends TypedAst.Declaration

  object Definition {

    case class Relation()

  }


  sealed trait Literal extends TypedAst {
    def tpe: TypedAst.Type
  }

  object Literal {

    case object Unit extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Unit
    }

    case class Bool(literal: scala.Boolean) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Bool
    }

    case class Int(literal: scala.Int) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Int
    }

    case class Str(literal: java.lang.String) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Str
    }

    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, literal: TypedAst.Literal, tpe: TypedAst.Type) extends TypedAst.Literal

    case class Tuple(elms: Seq[TypedAst.Literal], tpe: TypedAst.Type) extends TypedAst.Literal

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

  sealed trait Pattern extends TypedAst

  object Pattern {

    case class Wildcard(location: SourceLocation) extends TypedAst.Pattern

    case class Var(ident: ParsedAst.Ident) extends TypedAst.Pattern

    case class Lit(literal: ResolvedAst.Literal) extends TypedAst.Pattern

    case class Tag(name: ResolvedAst.RName, ident: ParsedAst.Ident, p: TypedAst.Pattern) extends TypedAst.Pattern

    case class Tuple(elms: Seq[TypedAst.Pattern]) extends TypedAst.Pattern

  }

  sealed trait Predicate

  object Predicate {

    case class NoApply()

    case class WithApply()

  }

  sealed trait Type extends TypedAst

  object Type {

    case object Unit extends TypedAst.Type

    case object Bool extends TypedAst.Type

    case object Int extends TypedAst.Type

    case object Str extends TypedAst.Type

    case class Tag(ident: ParsedAst.Ident, tpe: TypedAst.Type) extends TypedAst.Type

    case class Enum(variants: Map[String, TypedAst.Type.Tag]) extends TypedAst.Type

    case class Tuple(elms: Seq[TypedAst.Type]) extends TypedAst.Type

    case class Parametric(name: ResolvedAst.RName, elms: Seq[TypedAst.Type]) extends TypedAst.Type


  }


}
