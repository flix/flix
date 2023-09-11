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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, Location, Position, ResponseStatus}
import ca.uwaterloo.flix.language.ast.TypedAst.{Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Symbol, Type, TypeConstructor}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object FindReferencesProvider {

  def findRefs(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {

        case Entity.Case(caze) => findCaseReferences(caze.sym)

        case Entity.Class(class0) => findClassReferences(class0.sym)

        case Entity.Def(defn) => findDefReferences(defn.sym)

        case Entity.Sig(sig0) => findSigReferences(sig0.sym)

        case Entity.Enum(enum0) => findEnumReferences(enum0.sym)

        case Entity.TypeAlias(alias0) => findTypeAliasReferences(alias0.sym)

        case Entity.AssocType(assoc) => findAssocTypeReferences(assoc.sym)

        case Entity.Effect(eff0) => findEffectReferences(eff0.sym)

        case Entity.Op(op0) => findOpReferences(op0.sym)

        case Entity.DefUse(sym, _, _) => findDefReferences(sym)

        case Entity.SigUse(sym, _, _) => findSigReferences(sym)

        case Entity.VarUse(sym, _, _) => findVarReferences(sym)

        case Entity.CaseUse(sym, _, _) => findCaseReferences(sym)

        case Entity.Exp(_) => mkNotFound(uri, pos)

        case Entity.Field(field) => findLabelReferences(field)

        case Entity.FormalParam(param) => findVarReferences(param.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => findVarReferences(sym)
          case Pattern.Tag(Ast.CaseSymUse(sym, _), _, _, _) => findCaseReferences(sym)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred, _) => findPredReferences(pred)

        case Entity.LocalVar(sym, _) => findVarReferences(sym)

        case Entity.Type(t) => t match {
          case Type.Var(sym, _) => findTypeVarReferences(sym)
          case Type.Cst(tc, _) => tc match {
            case TypeConstructor.RecordRowExtend(label) => findLabelReferences(label)
            case TypeConstructor.SchemaRowExtend(pred) => findPredReferences(pred)
            case TypeConstructor.Enum(sym, _) => findEnumReferences(sym)
            case TypeConstructor.Effect(sym) => findEffectReferences(sym)
            case _ => mkNotFound(uri, pos)
          }
          case _ => mkNotFound(uri, pos)
        }

        case Entity.OpUse(sym, _, _) => findOpReferences(sym)

        case Entity.TypeVar(sym) => findTypeVarReferences(sym)

      }
    }
  }

  private def findClassReferences(sym: Symbol.ClassSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findDefReferences(sym: Symbol.DefnSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findSigReferences(sym: Symbol.SigSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findEnumReferences(sym: Symbol.EnumSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findTypeAliasReferences(sym: Symbol.TypeAliasSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findAssocTypeReferences(sym: Symbol.AssocTypeSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findLabelReferences(label: Name.Label)(implicit index: Index, root: Root): JObject = {
    val defSites = index.defsOf(label)
    val useSites = index.usesOf(label)
    val locs = (defSites ++ useSites).toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findPredReferences(pred: Name.Pred)(implicit index: Index, root: Root): JObject = {
    val defSites = index.defsOf(pred)
    val useSites = index.usesOf(pred)
    val locs = (defSites ++ useSites).toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findCaseReferences(sym: Symbol.CaseSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(root.enums(sym.enumSym).cases(sym).loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findVarReferences(sym: Symbol.VarSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findTypeVarReferences(sym: Symbol.KindedTypeVarSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findEffectReferences(sym: Symbol.EffectSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  private def findOpReferences(sym: Symbol.OpSym)(implicit index: Index, root: Root): JObject = {
    val defSite = Location.from(sym.loc)
    val useSites = index.usesOf(sym)
    val locs = defSite :: useSites.toList.map(Location.from)
    ("status" -> ResponseStatus.Success) ~ ("result" -> locs.map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
