/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SyntaxTree.{TreeKind}
import ca.uwaterloo.flix.language.ast.{Ast, SyntaxTree, SourceLocation, Token}
import ca.uwaterloo.flix.language.errors.Parser2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

object Parser2 {

  def run(root: Map[Ast.Source, Array[Token]])(implicit flix: Flix): Validation[Map[Ast.Source, SyntaxTree.Tree], CompilationMessage] =
    flix.phase("Parser2") {
      // Parse each source file in parallel.
      ParOps.parTraverseValues(root)(parse)
    }

  private def parse(ts: Array[Token]): Validation[SyntaxTree.Tree, CompilationMessage] = {
    // TODO: PARSER2
    Validation.success(SyntaxTree.Tree(TreeKind.Root, Array.empty, SourceLocation.Unknown))
  }


  /**
   * Utility function that computes a textual representation of a [[SyntaxTree.Tree]].
   * Meant for debugging use.
   */
  def syntaxTreeToDebugString(tree: SyntaxTree.Tree, nesting: Int = 1): String = {
    s"${tree.kind} (${tree.loc.beginLine}, ${tree.loc.beginCol}) -> (${tree.loc.endLine}, ${tree.loc.endCol}) ${
      tree.children.map {
        case SyntaxTree.Child.TokenChild(token) => s"\n${"  " * nesting}'${token.text}'"
        case SyntaxTree.Child.TreeChild(tree) => s"\n${"  " * nesting}${syntaxTreeToDebugString(tree, nesting + 1)}"
      }.mkString("")
    }"
  }
}
