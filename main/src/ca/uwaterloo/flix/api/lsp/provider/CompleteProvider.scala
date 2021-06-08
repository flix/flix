package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, Position}
import ca.uwaterloo.flix.language.ast.TypedAst

object CompleteProvider {

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, prefix: String, root: TypedAst.Root): List[CompletionItem] = {
    val result2 = List(
      CompletionItem("foo", "fooooooo", Some("This is a foo suggestion."), CompletionItemKind.Module, InsertTextFormat.PlainText, Nil),
    )

    result2 ::: getSnippetCompletionItems() ::: getKeywordCompletionItems() ::: getOtherCompletionItems(root)
  }


  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(): List[CompletionItem] = List(
    // TODO: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Keywords
    CompletionItem("namespace", "namespace", None, CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("{", "}")),

    // Keyword-like names (e.g. built-in functions).
    CompletionItem("println", "println", None, CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("(", ")")),
  )

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(): List[CompletionItem] = List(
    // TODO: Add more.
    // NB: Please keep the list alphabetically sorted.
    CompletionItem("match", "match ${1:exp} {\n case ${2:pat} => ${3:exp}\n}", None, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
  )

  /**
    * Returns a list of other completion items.
    */
  private def getOtherCompletionItems(root: TypedAst.Root): List[CompletionItem] = {
    // TODO: Magnus

    val result1 = if (root == null) Nil else {
      // TODO: Cleanup
      val listDefs = root.defs.filter(kv => kv._1.namespace == List("List") && kv._2.spec.mod.isPublic)
      listDefs.map {
        case (_, defn) =>
          val label = defn.sym.toString
          val insertText = defInsertText(defn)
          val detail = Some(defn.spec.doc.text.stripLeading())
          CompletionItem(label, insertText, detail, CompletionItemKind.Function, InsertTextFormat.Snippet, List("(", ")"))
      }
    }.toList
    result1
  }

  // TODO: DOC
  private def defInsertText(defn: TypedAst.Def): String = {
    val prefix = defn.sym.toString
    val args = defn.spec.fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"${prefix}(${args.mkString(", ")})"
  }

}
