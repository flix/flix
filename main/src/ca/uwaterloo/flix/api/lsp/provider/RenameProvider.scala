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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object RenameProvider {

  /**
    * Processes a rename request.
    */
  def processRename(newName: String, uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None =>
        println("query returned none")
        mkNotFound(uri, pos)

      case Some(entity) => {
        println(entity)
        entity match {

          case Entity.Case(caze) =>
            println("case")
            println(caze.tag)
            renameTag(caze.sym, caze.tag, newName)

          case Entity.Def(defn) =>
            println("def")
            println(defn.sym)
            renameDef(defn.sym, newName)

          case Entity.Exp(exp) =>
            println("exp")
            exp match {
              case Expression.Var(sym, _, _) =>
                println("exp.var")
                println(sym)
                renameVar(sym, newName)
              case Expression.Def(sym, _, _) =>
                println("exp.def")
                println(sym)
                renameDef(sym, newName)
              case Expression.Tag(sym, tag, _, _, _, _, _) =>
                println("exp.tag")
                println(sym)
                renameTag(sym, tag, newName)
              case Expression.Apply(exp, _, _, _, _, _) =>
                exp match {
                  case Expression.Sig(sym, tpe, loc) =>
                    println("exp.sig")
                    println(sym)
                    renameSigClass(Expression.Sig(sym, tpe, loc), newName)
                  case _ => mkNotFound(uri, pos)
                }

              case _ =>
                println("exp.notfound")
                mkNotFound(uri, pos)
            }

          case Entity.Field(field) =>
            println("field")
            println(field.name)
            renameField(field, newName)

          case Entity.Pattern(pat) =>
            println("pattern")
            pat match {
              case Pattern.Var(sym, _, _) =>
                println("pattern.var")
                renameVar(sym, newName)
              case Pattern.Tag(sym, tag, _, _, _) =>
                println("pattern.tag")
                println(sym)
                println(tag)
                renameTag(sym, tag, newName)
              case _ =>
                println("pattern.notfound")
                mkNotFound(uri, pos)
            }

          case Entity.Class(clazz) =>
            println("class")
            println(clazz.sym)
            renameClass(clazz.sym, newName)

          case Entity.Pred(pred, _) =>
            println("pred")
            println(pred.name)
            renamePred(pred, newName)

          case Entity.FormalParam(fparam) =>
            println("formalparam")
            println(fparam.sym)
            renameVar(fparam.sym, newName)

          case Entity.LocalVar(sym, _) =>
            println("localvar")
            println(sym)
            renameVar(sym, newName)

          case Entity.Type(t) =>
            println("type")
            t match {
              case Type.Cst(tc, _) =>
                println("type.cst (type constructor?)")
                tc match {
                  case TypeConstructor.RecordRowExtend(field) =>
                    println("typeconstructor.recordrowextend")
                    println(field.name)
                    renameField(field, newName)
                  case TypeConstructor.SchemaRowExtend(pred) =>
                    println("typeconstructor.schemarowextend")
                    println(pred.name)
                    renamePred(pred, newName)
                  case _ =>
                    println("typeconstructor not found")
                    mkNotFound(uri, pos)
                }
              case Type.KindedVar(sym, loc) =>
                println("type.kindedvar")
                println(sym)
                renameTypeVar(sym, newName)
              case _ =>
                println("type.notfound")
                mkNotFound(uri, pos)
            }

          case _ =>
            println("entity not found")
            mkNotFound(uri, pos)
        }
      }
    }

  }

  /**
    * Constructs the JSON response for renaming all `occurrences` to `newName`.
    *
    * NB: The occurrences must *NOT* overlap nor be repeated. Hence they are a set.
    */
  private def rename(newName: String, occurrences: Set[SourceLocation])(implicit index: Index, root: Root): JObject = {
    // Convert the set of occurrences to a sorted list.
    val targets = occurrences.toList.sorted

    // Group by URI.
    val groupedByUri = targets.groupBy(_.source.name)

    // Construct text edits.
    val textEdits = groupedByUri map {
      case (uri, locs) => uri -> locs.map(loc => TextEdit(Range.from(loc), newName))
    }

    // Assemble the workspace edit.
    val workspaceEdit = WorkspaceEdit(textEdits)

    // Construct the JSON result.
    ("status" -> "success") ~ ("result" -> workspaceEdit.toJSON)
  }

  private def renameDef(sym: Symbol.DefnSym, newName: String)(implicit index: Index, root: Root): JObject = {
    val defn = sym.loc
    val uses = index.usesOf(sym)
    rename(newName, uses + defn)
  }

  private def renameField(field: Name.Field, newName: String)(implicit index: Index, root: Root): JObject = {
    val defs = index.defsOf(field)
    val uses = index.usesOf(field)
    rename(newName, defs ++ uses)
  }

  private def renameClass(sym: Symbol.ClassSym, newName: String)(implicit index: Index, root: Root): JObject = {
    val loc = sym.loc
    val uses = index.usesOf(sym)
    rename(newName, uses + loc)
  }

  private def renamePred(pred: Name.Pred, newName: String)(implicit index: Index, root: Root): JObject = {
    val defs = index.defsOf(pred)
    val uses = index.usesOf(pred)
    rename(newName, defs ++ uses)
  }

  private def renameTag(sym: Symbol.EnumSym, tag: Name.Tag, newName: String)(implicit index: Index, root: Root): JObject = {
    val defn = root.enums(sym).cases(tag).tag.loc
    val uses = index.usesOf(sym, tag)
    rename(newName, uses + defn)
  }

  private def renameSigClass(sig: Expression.Sig, newName: String)(implicit index: Index, root: Root): JObject = {
    // Currently renames classes and instances correctly
    // but applications of a member i.e. `A.b()` get renamed to
    // `A.newName()`.
    // Is there any way to tell `rename` to use a
    // `Symbol.SigSym.clazz` instead of its `name` field?
    root.classes.find {
      case (_, clazz) => clazz.signatures.exists(s => s.sym == sig.sym)
    }.map(_._1.loc) match {
      case None => ???
      case Some(clazz) =>
        val uses = index.usesOf(sig.sym.clazz)
        rename(newName, uses + clazz)
    }
  }

  private def renameTypeVar(sym: Symbol.KindedTypeVarSym, newName: String)(implicit index: Index, root: Root): JObject = {
    val defn = sym.loc
    val uses = index.usesOf(sym)
    rename(newName, uses + defn)
  }

  private def renameVar(sym: Symbol.VarSym, newName: String)(implicit index: Index, root: Root): JObject = {
    val defn = sym.loc
    val uses = index.usesOf(sym)
    rename(newName, uses + defn)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
