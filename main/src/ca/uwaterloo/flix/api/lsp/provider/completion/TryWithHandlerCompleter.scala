package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TryWithCompletion
import ca.uwaterloo.flix.language.ast.Symbol.{CaseSym, EnumSym, OpSym, EffectSym}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor, TypedAst}

object TryWithHandlerCompleter extends Completer {
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[TryWithCompletion] = {
    println("looking")
    stripWord(context) match {
      case Some(word) =>
        print("found word: ")
        println(word)
        root.effects.flatMap {
          case (sym, eff) =>
            print("here is sym: ")
            println(sym.toString)
            if (matches(sym, word)) {
              println("FOUND MATCHING EFFECT")
              eff.ops.foreach(op => println(op.toString)) //print every op associated with an effect that matches
              None
            }
            else {
              None
            }
          case _ => None
        }
      case None =>
        println("no word found")
        Nil
    }
    Nil
  }

  private def stripWord(context: CompletionContext): Option[String] = {
    val regex = raw".*\s*with\s+(.*)".r
    context.prefix match {
      case regex(word) => Some(word)
      case _           => None
    }
  }

  private def matches(sym: EffectSym, word: String): Boolean = sym.name.startsWith(word)
}
