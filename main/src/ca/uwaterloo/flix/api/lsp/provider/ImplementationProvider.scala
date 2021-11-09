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

object ImplementationProvider {

  /**
    * Returns implementations LocationLink for a given class.
    */
  def processImplementation(uri: String, position: Position)(implicit root: Root): List[LocationLink] = {
    if (root == null) {
      // No AST available.
      return Nil
    }

    root.instances.keys.filter(i => i.loc.source.name == uri
      && (i.loc.beginLine < position.line
        || (i.loc.beginLine == position.line && i.loc.beginCol <= position.character))
      && (i.loc.endLine > position.line
        || (i.loc.endLine == position.line && i.loc.endCol >= position.character))
    ).flatMap(c => root.instances.getOrElse(c, Nil).map(c => LocationLink.fromInstance(c, c.sym.loc))).toList
  }
}
