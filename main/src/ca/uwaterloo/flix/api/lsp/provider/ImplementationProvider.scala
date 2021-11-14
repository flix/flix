/*
 * Copyright 2021 Nicola Dardanis
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{LocationLink, Position}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.Symbol

object ImplementationProvider {

  /**
    * Returns implementations LocationLink for a given class.
    */
  def processImplementation(uri: String, position: Position)(implicit root: Root): List[LocationLink] = {
    if (root == null) {
      // No AST available.
      return Nil
    }

    val links = for {
      classSym <- classAt(uri, position)
      inst <- root.instances.getOrElse(classSym, Nil)
    } yield LocationLink.fromInstanceSym(inst.sym, classSym.loc)

    links.toList
  }

  /**
    * Returns the class symbol located at the given position as a singleton iterable.
    * Returns an empty iterable if there is no such class symbol.
    */
  private def classAt(uri: String, p: Position)(implicit root: Root): Iterable[Symbol.ClassSym] = {
    root.instances.keys.filter(classSym => classSym.loc.source.name == uri
      && (classSym.loc.beginLine < p.line
      || (classSym.loc.beginLine == p.line && classSym.loc.beginCol <= p.character))
      && (classSym.loc.endLine > p.line
      || (classSym.loc.endLine == p.line && classSym.loc.endCol >= p.character)))
  }
}
