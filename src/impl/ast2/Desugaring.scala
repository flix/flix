package impl.ast2

import Ast._

/**
 * A compiler-phase which performs desugaring.
 */
object Desugaring {

  /**
   * Desugars the entire ast.
   */
  def desugar(ast: Root): Root = ast match {
    case Root(decls) => Root(decls map desugar)
  }

  /**
   * Desugars the given ast declaration.
   */
  def desugar(ast: Declaration): Declaration = ast match {
    case Declaration.NameSpace(name, decls) => Declaration.NameSpace(name, decls map desugar)
    case Declaration.Tpe(name, tpe) => Declaration.Tpe(name, desugar(tpe))
    case Declaration.Val(name, tpe, exp) => ???

    case _ => ast; // TODO

    // TODO: Introduce synthetic Tag declaraction? Declaraction.Tag(name, tpe, tpe)
  }

  /**
   * Desugars the given ast type.
   */
  def desugar(ast: Type): Type = ast match {
    case Type.AmbiguousName(Seq("Unit")) => Type.Unit
    case Type.AmbiguousName(Seq("Bool")) => Type.Bool
    case Type.AmbiguousName(Seq("Int")) => Type.Int
    case Type.AmbiguousName(Seq("Str")) => Type.Str

    case Type.Tuple(elms) => Type.Tuple(elms map desugar)
    case Type.Set(elms) => Type.Set(desugar(elms))
    case Type.Rel(elms) => ???
    case Type.Map(elms) => Type.Map(elms map desugar)

    case Type.Function(typ1, typ2) => Type.Function(desugar(typ1), desugar(typ2))

    case _ => ast // TODO
  }

  /**
   * returns the tags...
   */
  def tagsOf(tpe: Type): Set[Declaration] = tpe match {
    case x@Type.Enum(tags) => (tags map (t => Declaration.Tag(t.name, t, x))).toSet
    case _ => Set.empty // TODO should recurse
  }

}

