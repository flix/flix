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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}

import java.net.URI
import java.nio.file.{Path, Paths}

object PrettyPrinter {

  /**
    * Returns a "pretty-printed" string representation of the syntax tree corresponding
    * to the given URI.
    *
    * @param root the syntax tree root
    * @param uri  the URI of the source to print
    * @return the "pretty-printed" string, or empty if not found
    */
  def printPretty(root: SyntaxTree.Root, uri: Path): String = {

    def toPath(s: String): Option[Path] = {
      def tryPaths(strings: String*): Option[Path] =
        strings.to(LazyList).flatMap { str =>
          try Some(Paths.get(str).toAbsolutePath.normalize)
          catch { case _: Exception => None }
        }.headOption

      tryPaths(new URI(s).toString, s)
    }

    val maybeSource: Option[Source] = root.units.keys.find { source =>
      toPath(source.name) match {
        case None => false
        case Some(path) => path == uri.toAbsolutePath.normalize
      }
    }

    maybeSource match {
      case None => ""
      case Some(source) =>
        root.units.get(source).map(renderTree).getOrElse("")
    }
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
