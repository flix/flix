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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, Location, Position}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypeConstructor}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object FindReferencesProvider {

  def findRefs(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {

        case Entity.Case(caze) => findTagReferences(caze.sym, caze.tag)

        case Entity.Class(class0) => findClassReferences(class0.sym)

        case Entity.Def(defn) => findDefReferences(defn.sym)

        case Entity.Sig(sig0) => findSigReferences(sig0.sym)

        case Entity.Enum(enum0) => findEnumReferences(enum0.sym)

        case Entity.Exp(exp) => exp match {
          case Expression.Def(sym, _, _) => findDefReferences(sym)
          case Expression.Sig(sym, _, _) => findSigReferences(sym)
          case Expression.Var(sym, _, _) => findVarReferences(sym)
          case Expression.Tag(sym, tag, _, _, _, _) => findTagReferences(sym, tag)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Field(field) => findFieldReferences(field)

        case Entity.FormalParam(param) => findVarReferences(param.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => findVarReferences(sym)
          case Pattern.Tag(sym, tag, _, _, _) => findTagReferences(sym, tag)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred) => findPredReferences(pred)

        case Entity.LocalVar(sym, _) => findVarReferences(sym)

        case Entity.Type(t) => t match {
          case Type.KindedVar(sym, loc) => findTypeVarReferences(sym)
          case Type.Cst(tc, _) => tc match {
            case TypeConstructor.RecordRowExtend(field) => findFieldReferences(field)
            case TypeConstructor.SchemaRowExtend(pred) => findPredReferences(pred)
            case TypeConstructor.KindedEnum(sym, _) => findEnumReferences(sym)
            case _ => mkNotFound(uri, pos)
          }
          case _ => mkNotFound(uri, pos)
        }

        case _ => mkNotFound(uri, pos)

      }
    }
  }

  private def findClassReferences(sym: Symbol.ClassSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findDefReferences(sym: Symbol.DefnSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findSigReferences(sym: Symbol.SigSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findEnumReferences(sym: Symbol.EnumSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findFieldReferences(field: Name.Field)(implicit index: Index, root: Root): JObject = {
    val defSites = index.defsOf(field)
    val useSites = index.usesOf(field)
    val locs = (defSites ++ useSites).toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findPredReferences(pred: Name.Pred)(implicit index: Index, root: Root): JObject = {
    val defSites = index.defsOf(pred)
    val useSites = index.usesOf(pred)
    val locs = (defSites ++ useSites).toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findTagReferences(sym: Symbol.EnumSym, tag: Name.Tag)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(root.enums(sym).cases(tag).loc)
    val useSites = index.usesOf(sym, tag)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findVarReferences(sym: Symbol.VarSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findTypeVarReferences(sym: Symbol.KindedTypeVarSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
