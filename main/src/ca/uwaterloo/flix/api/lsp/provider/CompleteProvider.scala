package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, Position}
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}

object CompleteProvider {

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, prefix: String, root: TypedAst.Root): List[CompletionItem] = {
    getKeywordCompletionItems() ::: getSnippetCompletionItems() ::: getSuggestions(root)
  }

  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Keywords:
    CompletionItem("namespace", "namespace", None, None, CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("{", "}")),

    // Keyword-like (e.g. println):
    CompletionItem("println", "println", None, None, CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("(", ")")),
  )

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Declaration-based:

    // Expressed-based:
    CompletionItem("match", "match ${1:exp} {\n case ${2:pat} => ${3:exp}\n}", None, None, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, None, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
  )

  /**
    * Returns a list of other completion items.
    */
  private def getSuggestions(root: TypedAst.Root): List[CompletionItem] = {
    // TODO: Magnus

    val result1 = if (root == null) Nil else {
      // TODO: Cleanup
      val listDefs = root.defs.filter(kv => kv._1.namespace == List("List") && kv._2.spec.mod.isPublic)
      listDefs.map {
        case (_, defn) =>
          implicit val audience = Audience.External
          val label = reconstructSignature(defn)
          val insertText = defInsertText(defn)
          val detail = Some(FormatScheme.formatScheme(defn.spec.declaredScheme))
          val documentation = Some(defn.spec.doc.text)
          CompletionItem(label, insertText, detail, documentation, CompletionItemKind.Function, InsertTextFormat.Snippet, List("(", ")"))
      }
    }.toList
    result1
  }

  // TODO: Magnus
  private def reconstructSignature(defn: TypedAst.Def): String = {
    implicit val audience = Audience.External

    val prefix = defn.sym.toString
    val args = defn.spec.fparams.map {
      case fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
    }
    val tpe = FormatType.formatType(getResultType(defn.spec.declaredScheme.base))
    val eff = FormatType.formatType(defn.spec.eff) // TODO: Dont show pure
    s"${prefix}(${args.mkString(", ")}): $tpe$eff"
  }

  // TODO: Magnus
  private def defInsertText(defn: TypedAst.Def): String = {
    val prefix = defn.sym.toString
    val args = defn.spec.fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"${prefix}(${args.mkString(", ")})"
  }

  /**
    * Returns the return type of the given function type `tpe0`.
    */
  private def getResultType(tpe0: Type): Type = tpe0.typeArguments.last

}
