package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypeConstructor, TypedAst}
import scala.annotation.TypeConstraint

object MagicMatchCompleter {

  /**
    * Returns a list of Completions for match, triggered by expr.match.
    */
  def getCompletions(name: String, tpe: Type ,ctx: CompletionContext)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (!"match".startsWith(name)) return Nil

    val ident = extractIdentifier(ctx.word)
    getEnumSym(tpe) match {
      case Some(sym) =>
        val cases = root.enums(sym).cases
        val (casesString, _) = cases.toList.sortBy(_._1.loc).foldLeft(("", 1))({
          case ((acc, z), (sym, cas)) =>
            val (str, k) = cas.tpe.typeConstructor match {
              case Some(TypeConstructor.Unit) => (s"$sym => $${${z + 1}:???}", z + 1)
              case Some(TypeConstructor.Tuple(arity)) => (List.range(1, arity + 1)
                .map(elem => s"$${${elem + z}:_elem$elem}")
                .mkString(s"$sym(", ", ", s") => $${${arity + z + 1}:???}"), z + arity + 1)
              case _ => (s"$sym($${${z + 1}:_elem}) => $${${z + 2}:???}", z + 2)
            }
            (acc + "    case " + str + "\n", k)
        })
        val matchExpr = s"match $ident {\n$casesString}"
        val prompt = s"$ident.match"
        Completion.SnippetCompletion(prompt, matchExpr, "Expand to a full match expression.") :: Nil
      case None => Nil
    }
  }

  /**
    * Extract the substring before the last . as the identifier
    */
  private def extractIdentifier(word: String): String = {
    word.substring(0, word.lastIndexOf("."))
  }

  /**
   * Returns the enum symbol of the given type, if it is an enum.
   */
  private def getEnumSym(tpe: Type): Option[Symbol.EnumSym] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) => Some(sym)
    case _ => None
  }
}
