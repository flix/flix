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

import ca.uwaterloo.flix.api.lsp.LocationLink
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Position, Symbol}

object ImplementationProvider {

  /**
    * Returns implementations LocationLink for a given trait.
    */
  def processImplementation(uri: String, position: Position)(implicit root: Root): List[LocationLink] = {
    if (root == null) {
      // No AST available.
      return Nil
    }

    val links = for {
      traitSym <- traitAt(uri, position)
      inst <- root.instances.getOrElse(traitSym, Nil)
    } yield LocationLink.fromInstanceTraitSymUse(inst.trt, traitSym.loc)

    links.toList
  }

  /**
    * Returns the trait symbol located at the given position as a singleton iterable.
    * Returns an empty iterable if there is no such trait symbol.
    */
  private def traitAt(uri: String, p: Position)(implicit root: Root): Iterable[Symbol.TraitSym] = {
    root.instances.keys.filter(traitSym => traitSym.loc.source.name == uri
      && (traitSym.loc.beginLine < p.line
      || (traitSym.loc.beginLine == p.line && traitSym.loc.beginCol <= p.character))
      && (traitSym.loc.endLine > p.line
      || (traitSym.loc.endLine == p.line && traitSym.loc.endCol >= p.character)))
  }
}
