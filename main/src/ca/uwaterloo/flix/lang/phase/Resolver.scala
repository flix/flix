package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{WeededAst, ResolvedAst, ParsedAst}
import util.Validation
import util.Validation._

object Resolver {

  import ResolverError._

  sealed trait ResolverError

  object ResolverError {

    //case class UnknownPredicate(name: ParsedAst.QName) extends ResolverError

    //case class IllegalReference(expect: String, actual: String) extends ResolverError

  }

  def resolve(wast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {
    wast.declarations.map {
      case wd: WeededAst.Declaration => Declaration.symbols(wd, Nil)
    }

    ???
  }

  object Declaration {

    def symbols(wd: WeededAst.Declaration, namespace: List[String]): Validation[Map[ResolvedAst.RName, WeededAst.Definition], ResolverError] = wd match {
      case WeededAst.Declaration.Namespace(ParsedAst.QName(parts, location), body) =>

        ???

      case d: WeededAst.Declaration.Fact => Map.empty[ResolvedAst.RName, WeededAst.Definition].toSuccess // nop
      case d: WeededAst.Declaration.Rule => Map.empty[ResolvedAst.RName, WeededAst.Definition].toSuccess // nop
      case d: WeededAst.Definition => Definition.symbols(d)
    }


    def link(p: WeededAst.PredicateNoApply, globals: Map[ResolvedAst.RName, WeededAst.Definition]): ResolvedAst.Predicate =
      ???

    //      globals.get(p.name) match {
    //        case None => ??? //UnknownPredicate(p.name)
    //        case Some(d: ParsedAst.Definition.Function) => ResolvedAst.Predicate.Functional()
    //        case Some(d: ParsedAst.Definition.Relation) => ResolvedAst.Predicate.Relational()
    //        case Some(otherDecl) => ??? // IllegalReference("Relation", otherDecl)
    //      }
  }

  object Definition {

    def symbols(wd: WeededAst.Definition): Validation[Map[ResolvedAst.RName, WeededAst.Definition], ResolverError] = wd match {
      case WeededAst.Definition.Function(ident, formals, tpe, body) => ???
      case _ => ???
    }

  }

  object Literal {

    def link(wl: WeededAst.Literal): Validation[ResolvedAst.Literal, ResolverError] = wl match {
      case WeededAst.Literal.Unit => ResolvedAst.Literal.Unit.toSuccess
      case WeededAst.Literal.Bool(b) => ResolvedAst.Literal.Bool(b).toSuccess
      case WeededAst.Literal.Int(i) => ResolvedAst.Literal.Int(i).toSuccess
      case WeededAst.Literal.Str(s) => ResolvedAst.Literal.Str(s).toSuccess
      case WeededAst.Literal.Tag(name, ident, literal) => ??? // TODO: Need access to defn
      case WeededAst.Literal.Tuple(welms) => @@(welms map link) map {
        case elms => ResolvedAst.Literal.Tuple(elms)
      }
    }

  }

  object Expression {


    def link(we: WeededAst.Expression, globals: Map[ResolvedAst.RName, WeededAst.Definition]): Validation[ResolvedAst.Expression, ResolverError] = we match {
      case WeededAst.Expression.AmbiguousVar(name) => ???
      case WeededAst.Expression.AmbiguousApply(name, args) => ???
      case WeededAst.Expression.Lit(wlit) => ???
      case WeededAst.Expression.Lambda(formals, tpe, body) => ???
      case WeededAst.Expression.Unary(op, e) => ???
      case WeededAst.Expression.Binary(e1, op, e2) => ???
      case WeededAst.Expression.IfThenElse(e1, e2, e3) => ???
      //case WeededAst.Expression.Let

      //      case class IfThenElse(e1: WeededAst.Expression, e2: WeededAst.Expression, e3: WeededAst.Expression) extends WeededAst.Expression
      //
      //      case class Let(ident: ParsedAst.Ident, value: WeededAst.Expression, body: WeededAst.Expression) extends WeededAst.Expression
      //
      //      case class Match(e: WeededAst.Expression, rules: Seq[(WeededAst.Pattern, WeededAst.Expression)]) extends WeededAst.Expression
      //
      //      case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, e: WeededAst.Expression) extends WeededAst.Expression
      //
      //      case class Tuple(elms: Seq[WeededAst.Expression]) extends WeededAst.Expression
      //
      //      case class Ascribe(e: WeededAst.Expression, tpe: WeededAst.Type) extends WeededAst.Expression
      //
      //      case class Error(location: SourceLocation) extends WeededAst.Expression
    }

  }


}
