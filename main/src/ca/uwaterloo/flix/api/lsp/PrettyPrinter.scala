/*
 * Copyright 2025 gwydd
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
package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}

import java.nio.file.Path

object PrettyPrinter {

  /**
    * Returns a "pretty-printed" string representation of the given syntax tree `root`.
    *
    * @param root the syntax tree root
    * @return the "pretty-printed" string representation
    */
  def printPretty(root: SyntaxTree.Root, uri: Path): String = {
    Console.println(s"Pretty printing syntax tree for: $uri")
    root.units.values.map(renderTree).mkString
  }

  private def renderTree(tree: SyntaxTree.Tree): String = {
    val sb = new StringBuilder

    def visit(t: SyntaxTree.Tree): Unit = {
      val SyntaxTree.Tree(_, children, _) = t
      children.foreach {
        case token: Token => sb.append(token.text)
        case childTree: SyntaxTree.Tree => visit(childTree)
        case _ => ()
      }
    }

    visit(tree)
    sb.toString()
  }
}
