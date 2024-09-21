package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypeConstructor, TypedAst}

object MagicMatchCompleter {

  def getCompletions(ident: Name.Ident, tpe: Type)(implicit root: TypedAst.Root): Iterable[Completion] = {
    println("magic match")
    getEnumSym(tpe) match {
      case Some(sym) =>
        val cases = root.enums(sym).cases
        val s = "match " + ident.name + "{" + cases.map(_._1.name).mkString(" => \n ") + "}"

        Completion.SnippetCompletion("color.match", s, "here is my documentation") :: Nil

      case None => Nil
    }

  }

  private def getEnumSym(tpe: Type): Option[Symbol.EnumSym] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) => Some(sym)
    case _ => None
  }
}
