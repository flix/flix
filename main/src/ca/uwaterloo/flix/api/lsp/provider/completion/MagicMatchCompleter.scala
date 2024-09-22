package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.{MatchCompletion, SnippetCompletion}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypeConstructor, TypedAst}

object MagicMatchCompleter {

  def getCompletions(ident: Name.Ident, tpe: Type)(implicit root: TypedAst.Root): Iterable[Completion] = {
    println("magic match")
    getEnumSym(tpe) match {
      case Some(sym) => matchCompletion(root.enums(sym)) :: Nil
      case None => Nil
    }
  }

  private def getEnumSym(tpe: Type): Option[Symbol.EnumSym] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) => Some(sym)
    case _ => None
  }

  // TODO: This code copy and pasted. It should be cleaned up.
  private def matchCompletion(enm: TypedAst.Enum): Completion = {
    val (completion, _) = enm.cases.toList.sortBy(_._1.loc).foldLeft(("", 1))({
      case ((acc, z), (sym, cas)) =>
        val enumName = enm.sym.toString
        val caseName = sym.name
        val (str, k) = cas.tpe.typeConstructor match {
          case Some(TypeConstructor.Unit) => (s"$enumName.$caseName => $${${z + 1}:???}", z + 1)
          case Some(TypeConstructor.Tuple(arity)) => (List.range(1, arity + 1)
            .map(elem => s"$${${elem + z}:_elem$elem}")
            .mkString(s"$enumName.$caseName(", ", ", s") => $${${arity + z + 1}:???}"), z + arity + 1)
          case _ => (s"$enumName.$caseName($${${z + 1}:_elem}) => $${${z + 2}:???}", z + 2)
        }
        (acc + "    case " + str + "\n", k)
    })
    SnippetCompletion("color.match", s"match color {\n$completion}", "docs")
  }
}
