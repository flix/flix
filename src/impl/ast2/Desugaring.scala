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
    case Declaration.NameSpace(name, decls)   => Declaration.NameSpace(name, decls map desugar)
    case Declaration.Tpe(name, tpe)           => Declaration.Tpe(name, desugar(tpe))
   // case Declaration.Enum(name, tpe)          => Declaration.Enum(name, desugar(tpe))
    case Declaration.Val(name, tpe, exp)      => Declaration.Val(name, desugar(tpe), desugar(exp))
    case Declaration.Var(name, tpe)           => Declaration.Var(name, desugar(tpe))
    case Declaration.Fun(annotations, name, args, tpe, exp) =>
      val args2 = args.map(a => (a._1, desugar(a._2)))
      Declaration.Fun(annotations, name, args2, desugar(tpe), desugar(exp))

    case _ => ast
  }

  /**
   * Desugars the given ast type.
   */
  def desugar(ast: Type): Type = ast match {
    case Type.Tuple(elms) => Type.Tuple(elms map desugar)
    case Type.Set(elms) => Type.Set(desugar(elms))
    case Type.Map(elms) => Type.Map(elms map desugar)
    case Type.Function(typ1, typ2) => Type.Function(desugar(typ1), desugar(typ2))
    case _ => ast
  }

  def desugar(ast: Expression) = ast

}

