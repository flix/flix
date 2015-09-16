package ca.uwaterloo.flix.lang.ast

trait ResolvedAst

object ResolvedAst {

  // TODO: Split these up or not?
  case class SymbolTable(
                          m: Map[Ref, ResolvedAst.Declaration],
                          relations: Map[Ref, ResolvedAst.Declaration.Relation]
                          )

  sealed trait Declaration

  object Declaration {

    case class Fact(head: ResolvedAst.Predicate) extends Declaration

    case class Relation() extends Declaration

  }

  sealed trait Literal

  sealed trait Expression extends WeededAst

  object Expression {

    case class LocalVar(name: String, location: SourceLocation) extends ResolvedAst.Expression

    case class GlobalVar(ref: Ref, location: SourceLocation) extends ResolvedAst.Expression

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