/*
 * Copyright 2021 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.TypedAst

object Snippets {

  def lookupSnippets(root: TypedAst.Root): List[CompletionItem] = {
    // TODO: Experiment with lookup.
    val result1 = if (root == null) Nil else {
      val listDefs = root.defs.filter(_._1.namespace == List("List"))
      listDefs.map {
        case (_, defn) =>
          val label = defn.sym.name
          val insertText = defn.sym.name
          val detail = Some(defn.spec.doc.text)
          CompletionItem(label, insertText, detail, InsertTextFormat.PlainText)
      }
    }.take(10).toList

    // TODO: Experiment with snippets
    val result2 = List(
      CompletionItem("foo", "fooooooo", Some("This is a foo suggestion."), InsertTextFormat.PlainText),
      CompletionItem("bar", "baaaaaar", Some("This is a bar suggestion."), InsertTextFormat.PlainText),
      CompletionItem("baz", "baaaaaaz", Some("This is a baz suggestion."), InsertTextFormat.PlainText),
      CompletionItem("match", "match ${1:exp} {\n case ${2:pat} => ${3:exp}\n}", None, InsertTextFormat.Snippet),
      CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, InsertTextFormat.Snippet),
    )
    result2 ::: result1
  }

}
