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

  def resolve(ast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {


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


    def link(p: ParsedAst.Predicate, globals: Map[ParsedAst.QName, ParsedAst]): ResolvedAst.Predicate =
      globals.get(p.name) match {
        case None => ??? //UnknownPredicate(p.name)
        case Some(d: ParsedAst.Definition.Function) => ResolvedAst.Predicate.Functional()
        case Some(d: ParsedAst.Definition.Relation) => ResolvedAst.Predicate.Relational()
        case Some(otherDecl) => ??? // IllegalReference("Relation", otherDecl)
      }
  }

  object Definition {

    def symbols(d: WeededAst.Definition): Validation[Map[ResolvedAst.RName, WeededAst.Definition], ResolverError] = d match {
      case _ => ???
    }

  }

  object Literal {

  }

  object Expression {

  }


}
