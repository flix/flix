package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CompletionItem, InsertTextFormat}
import ca.uwaterloo.flix.language.ast.TypedAst

object CompleteProvider {

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(root: TypedAst.Root): List[CompletionItem] = {
    val result2 = List(
      CompletionItem("foo", "fooooooo", Some("This is a foo suggestion."), InsertTextFormat.PlainText),
    )

    result2 ::: getSnippetCompletionItems() ::: getKeywordCompletionItems() ::: getOtherCompletionItems(root)
  }


  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(): List[CompletionItem] = List(
    // TODO: Add more.
    CompletionItem("namespace", "namespace", None, InsertTextFormat.PlainText),
    CompletionItem("println", "println", None, InsertTextFormat.PlainText),
  )

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(): List[CompletionItem] = List(
    // TODO: Add more.
    CompletionItem("match", "match ${1:exp} {\n case ${2:pat} => ${3:exp}\n}", None, InsertTextFormat.Snippet),
    CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, InsertTextFormat.Snippet),
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
          CompletionItem(label, insertText, detail, InsertTextFormat.Snippet)
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
