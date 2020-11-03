/*
 * Copyright 2020 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, Position, Range, TextEdit, WorkspaceEdit}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object RenameProvider {

  /**
    * Processes a rename request.
    */
  def processRename(newName: String, uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)
      case Some(entity) => entity match {

        case Entity.Def(defn) => renameDef(newName, defn.sym)

        case Entity.Exp(exp) => exp match {
          case Expression.Var(sym, _, _) => renameVar(newName, sym)
          case Expression.Def(sym, _, _) => renameDef(newName, sym)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Field(field) => renameField(newName, field)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => renameVar(newName, sym)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred) => renamePred(newName, pred)

        case Entity.FormalParam(fparam) => renameVar(newName, fparam.sym)

        case Entity.LocalVar(sym, _) => renameVar(newName, sym)

        case _ => mkNotFound(uri, pos)
      }
    }

  }

  private def rename(newName: String, occurrences: List[SourceLocation])(implicit index: Index, root: Root): JObject = {
    // Group by URI.
    val groupedByUri = occurrences.groupBy(_.source.name)

    // Construct text edits.
    val textEdits = groupedByUri map {
      case (uri, locs) => uri -> locs.map(loc => TextEdit(Range.from(loc), newName))
    }

    // Assemble the workspace edit.
    val workspaceEdit = WorkspaceEdit(textEdits)

    // Construct the JSON result.
    ("status" -> "success") ~ ("result" -> workspaceEdit.toJSON)
  }

  private def renameDef(newName: String, sym: Symbol.DefnSym)(implicit index: Index, root: Root): JObject = {
    val defn = sym.loc
    val uses = index.usesOf(sym)
    rename(newName, defn :: uses.toList)
  }

  private def renameField(newName: String, field: Name.Field)(implicit index: Index, root: Root): JObject = {
    val defs = index.defsOf(field)
    val uses = index.usesOf(field)
    rename(newName, (defs ++ uses).toList)
  }

  private def renamePred(newName: String, pred: Name.Pred)(implicit index: Index, root: Root): JObject = {
    val defs = index.defsOf(pred)
    val uses = index.usesOf(pred)
    rename(newName, (defs ++ uses).toList)
  }

  private def renameVar(newName: String, sym: Symbol.VarSym)(implicit index: Index, root: Root): JObject = {
    val defn = sym.loc
    val uses = index.usesOf(sym)
    rename(newName, defn :: uses.toList)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
