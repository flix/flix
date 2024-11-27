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

import ca.uwaterloo.flix.api.lsp.acceptors.{AllAcceptor, FileAcceptor, InsideAcceptor}
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.{Consumer, Index, Position, Range, ResponseStatus, TextEdit, Visitor, WorkspaceEdit}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object RenameProvider {

  /**
    * Processes a rename request.
    */
  def processRename(newName: String, uri: String, pos: Position)(implicit root: Root): JObject = {
    val consumer = StackConsumer()

    Visitor.visitRoot(root, consumer, InsideAcceptor(uri, pos))

    consumer
      .getStack
      .headOption
      .flatMap(getOccurs)
      .map(occurs => rename(newName, occurs))
      .getOrElse(mkNotFound(uri, pos))

//    index.query(uri, pos) match {
//      case None => mkNotFound(uri, pos)
//
//      case Some(entity) => entity match {
//
//        case Entity.Case(caze) => renameCase(caze.sym, newName)
//
//        case Entity.StructField(field) => renameStructField(field.sym, newName)
//
//        case Entity.Def(defn) => renameDef(defn.sym, newName)
//
//        case Entity.TypeAlias(alias) => renameTypeAlias(alias.sym, newName)
//
//        case Entity.VarUse(sym, _, _) => renameVar(sym, newName)
//
//        case Entity.DefUse(sym, _, _) => renameDef(sym, newName)
//
//        case Entity.CaseUse(sym, _, _) => renameCase(sym, newName)
//
//        case Entity.StructFieldUse(sym, _, _) => renameStructField(sym, newName)
//
//        case Entity.Exp(_) => mkNotFound(uri, pos)
//
//        case Entity.Label(label) => renameLabel(label, newName)
//
//        case Entity.Pattern(pat) => pat match {
//          case Pattern.Var(Binder(sym, _), _, _) => renameVar(sym, newName)
//          case Pattern.Tag(CaseSymUse(sym, _), _, _, _) => renameCase(sym, newName)
//          case _ => mkNotFound(uri, pos)
//        }
//
//        case Entity.Pred(pred, _) => renamePred(pred, newName)
//
//        case Entity.FormalParam(fparam) => renameVar(fparam.bnd.sym, newName)
//
//        case Entity.LocalVar(sym, _) => renameVar(sym, newName)
//
//        case Entity.Type(t) => t match {
//          case Type.Cst(tc, _) => tc match {
//            case TypeConstructor.RecordRowExtend(label) => renameLabel(label, newName)
//            case TypeConstructor.SchemaRowExtend(pred) => renamePred(pred, newName)
//            case _ => mkNotFound(uri, pos)
//          }
//          case Type.Var(sym, _) => renameTypeVar(sym, newName)
//          case _ => mkNotFound(uri, pos)
//        }
//
//        case Entity.Trait(_) => mkNotFound(uri, pos)
//        case Entity.AssocType(_) => mkNotFound(uri, pos)
//        case Entity.Effect(_) => mkNotFound(uri, pos)
//        case Entity.Enum(_) => mkNotFound(uri, pos)
//        case Entity.Struct(_) => mkNotFound(uri, pos)
//        case Entity.Op(_) => mkNotFound(uri, pos)
//        case Entity.OpUse(_, _, _) => mkNotFound(uri, pos)
//        case Entity.Sig(_) => mkNotFound(uri, pos)
//        case Entity.SigUse(_, _, _) => mkNotFound(uri, pos)
//        case Entity.TypeVar(_) => mkNotFound(uri, pos)
//      }
//    }

  }

  private def getOccurs(x: AnyRef)(implicit root: Root): Option[Set[SourceLocation]] = x match {
    // Defs
    case TypedAst.Def(sym, _, _, _) => Some(getDefOccurs(sym))
    case SymUse.DefSymUse(sym, _) => Some(getDefOccurs(sym))
    // Vars
    case TypedAst.Expr.Var(sym, _, _) => Some(getVarOccurs(sym))
    case TypedAst.Binder(sym, _) => Some(getVarOccurs(sym))
    // Case
    case TypedAst.Case(sym, _, _, _) => Some(getCaseSymOccurs(sym))
    case SymUse.CaseSymUse(sym, _) => Some(getCaseSymOccurs(sym))
    // Enum
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) => Some(getEnumSymOccurs(sym))
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(getEnumSymOccurs(sym))
    case _ => None
  }

  private def getEnumSymOccurs(sym: Symbol.EnumSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = consider(enm.sym, enm.sym.loc)
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = seperateEnumAndCaseSymOccur(sym)._1.foreach(enmLoc => consider(sym.sym.enumSym, enmLoc))

      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => consider(sym, loc)
        case _ => ()
      }

    }

    Visitor.visitRoot(root, EnumSymConsumer, AllAcceptor)

    occurs
  }

  private def getCaseSymOccurs(sym: Symbol.CaseSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.CaseSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object CaseSymConsumer extends Consumer {
      override def consumeCase(cse: TypedAst.Case): Unit = consider(cse.sym, cse.sym.loc)
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = consider(sym.sym, seperateEnumAndCaseSymOccur(sym)._2)
    }

    Visitor.visitRoot(root, CaseSymConsumer, AllAcceptor)

    occurs
  }

  private def seperateEnumAndCaseSymOccur(use: SymUse.CaseSymUse): (Option[SourceLocation], SourceLocation) = {
    // NB: it's safe to simply offset the columns to account for the enum name, since the case symbol occurrence must be on a single line
    val SymUse.CaseSymUse(sym, loc) = use
    val enumNameLen = sym.enumSym.name.length
    val occurLen = loc.sp2.col - loc.sp1.col
    val caseWithEnumNameLen = sym.enumSym.name.length + sym.name.length + 1

    val occurContainsEnumName = occurLen == caseWithEnumNameLen
    if (occurContainsEnumName) {
      // Column position of separating "." between enum symbol occurrence and case symbol occurrence.
      val sepCol = loc.sp1.col + enumNameLen

      // Extract case symbol occurrence SourceLocation
      val enumEndColWithoutCaseName = sepCol.toShort
      val enumSymEnd = SourcePosition(loc.sp1.source, loc.sp2.line, enumEndColWithoutCaseName)
      val enumSymOccurLoc = SourceLocation(loc.isReal, loc.sp1, enumSymEnd)

      // Extract enum symbol occurrence SourceLocation
      val caseSymStartColWithoutEnumName = (sepCol + 1).toShort
      val caseSymStart = SourcePosition(loc.sp1.source, loc.sp1.line, caseSymStartColWithoutEnumName)
      val caseSymOccurLoc = SourceLocation(loc.isReal, caseSymStart, loc.sp2)

      (Some(enumSymOccurLoc), caseSymOccurLoc)
    } else {
      (None, loc)
    }
  }

  private def getVarOccurs(sym: Symbol.VarSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.VarSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object VarSymConsumer extends Consumer {
      override def consumeBinder(bnd: TypedAst.Binder): Unit = consider(bnd.sym, bnd.sym.loc)
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.Var(s, _, loc) => consider(s, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, AllAcceptor)

    occurs
  }

  private def getDefOccurs(sym: Symbol.DefnSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object DefnSymConsumer extends Consumer {
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = consider(sym.sym, sym.loc)
      override def consumeDef(defn: TypedAst.Def): Unit = consider(defn.sym, defn.sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, AllAcceptor)

    occurs
  }

  /**
    * Constructs the JSON response for renaming all `occurences` to `newName`.
    *
    * NB: The occurrences must *NOT* overlap nor be repeated. Hence they are a set.
    */
  private def rename(newName: String, occurrences: Set[SourceLocation]): JObject = {
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
    ("status" -> ResponseStatus.Success) ~ ("result" -> workspaceEdit.toJSON)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
