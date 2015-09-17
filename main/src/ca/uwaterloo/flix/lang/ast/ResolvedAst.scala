package ca.uwaterloo.flix.lang.ast

trait ResolvedAst

object ResolvedAst {

  // TODO Replace QName by RName.

  sealed trait Declaration extends ResolvedAst

  object Declaration {

    case class Fact(head: ResolvedAst.Predicate) extends Declaration


  }

  sealed trait Definition extends ResolvedAst.Declaration

  object Definition {

    case class Relation() extends ResolvedAst.Definition

  }

  sealed trait Literal

  object Literal {

    case object Unit extends ResolvedAst.Literal

    case class Bool(literal: scala.Boolean) extends ResolvedAst.Literal

    case class Int(literal: scala.Int) extends ResolvedAst.Literal

    case class Str(literal: java.lang.String) extends ResolvedAst.Literal

    // TODO: Need access to the enum declaration.
    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, literal: ResolvedAst.Literal, defn: WeededAst.Definition.Enum) extends ResolvedAst.Literal

    case class Tuple(elms: Seq[ResolvedAst.Literal]) extends ResolvedAst.Literal

  }

  sealed trait Expression extends Definition

  object Expression {

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Expression

    case class Ref(name: ParsedAst.QName, decl: WeededAst.Declaration) extends ResolvedAst.Expression

    // TODO
    case class Apply(ident: ParsedAst.Ident) extends ResolvedAst.Expression

    // TODO
    case class ApplyRef(ident: ParsedAst.Ident) extends ResolvedAst.Expression


    case class IfThenElse(e1: ResolvedAst.Expression, e2: ResolvedAst.Expression, e3: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Let(ident: ParsedAst.Ident, value: ResolvedAst.Expression, body: ResolvedAst.Expression) extends ResolvedAst.Expression


    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, e: ResolvedAst.Expression, decl: WeededAst.Definition.Enum) extends ResolvedAst.Expression

  }

  sealed trait Predicate

  object Predicate {

    case class Relational(/* todo: what */) extends ResolvedAst.Predicate

    case class Functional(/*  todo: what */) extends ResolvedAst.Predicate

  }

  sealed trait Ref

  object Ref {

    // TODO: Need this or not?
    case class FormalArg(location: SourceLocation)

  }


}