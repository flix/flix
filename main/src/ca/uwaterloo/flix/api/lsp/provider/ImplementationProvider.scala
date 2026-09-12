/*
 * Copyright 2021 Nicola Dardanis
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{LocationLink, Position}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.Symbol

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
      inst <- root.instances.get(traitSym)
    } yield LocationLink.fromInstanceTraitSymUse(inst.trt, traitSym.loc)

    links.toList
  }

  /**
    * Returns the trait symbol located at the given position as a singleton iterable.
    * Returns an empty iterable if there is no such trait symbol.
    */
  private def traitAt(uri: String, p: Position)(implicit root: Root): Iterable[Symbol.TraitSym] = {
    root.instances.keys.filter(traitSym => traitSym.loc.source.name == uri
      && (traitSym.loc.startLine < p.line
      || (traitSym.loc.startLine == p.line && traitSym.loc.startCol <= p.character))
      && (traitSym.loc.endLine > p.line
      || (traitSym.loc.endLine == p.line && traitSym.loc.endCol >= p.character)))
  }
}
