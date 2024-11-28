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
import ca.uwaterloo.flix.language.ast.shared.{SymUse, TraitConstraint}
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
    // Structs
    case TypedAst.Struct(_, _, _, sym, _, _, _, _) => Some(getStructSymOccurs(sym))
    case Type.Cst(TypeConstructor.Struct(sym, _), _) => Some(getStructSymOccurs(sym))
    case TypedAst.StructField(sym, _, _) => Some(getStructFieldSymOccurs(sym))
    case SymUse.StructFieldSymUse(symUse, _) => Some(getStructFieldSymOccurs(symUse))
    // Traits
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => Some(getTraitSymOccurs(sym))
    case SymUse.TraitSymUse(sym, _) => Some(getTraitSymOccurs(sym))
    case TraitConstraint.Head(sym, _) => Some(getTraitSymOccurs(sym))
    // Sig
    case TypedAst.Sig(sym, _, _, _) => Some(getSigSymOccurs(sym))
    case SymUse.SigSymUse(sym, _) => Some(getSigSymOccurs(sym))
    // Effects
    case TypedAst.Effect(_, _, _, sym, _, _) => Some(getEffectSymOccurs(sym))
    case SymUse.EffectSymUse(sym, _) => Some(getEffectSymOccurs(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Some(getEffectSymOccurs(sym))
    case _ => None
  }

  private def getEffectSymOccurs(sym: Symbol.EffectSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.EffectSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object EffectSymConsumer extends Consumer {
      override def consumeEff(eff: TypedAst.Effect): Unit = consider(eff.sym, eff.sym.loc)
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = consider(effUse.sym, effUse.loc)
      override def consumeOpSymUse(sym: SymUse.OpSymUse): Unit = sepEffAndOpSymOccur(sym)._1.foreach(effLoc => consider(sym.sym.eff, effLoc))
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Effect(sym), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, AllAcceptor)

    occurs
  }

  private def sepEffAndOpSymOccur(symUse: SymUse.OpSymUse): (Option[SourceLocation], SourceLocation) = {
    val SymUse.OpSymUse(sym, loc) = symUse
    val effNameLen = sym.name.length
    val occurLen = loc.sp2.col - loc.sp1.col
    val opWithEffNameLen = effNameLen + sym.name.length + 1

    val occurContainsEffName = occurLen == opWithEffNameLen
    if (occurContainsEffName) {
      val sepCol = loc.sp1.col + effNameLen

      // Extract effect symbol occurrence
      val effEndColWithoutOpName = sepCol.toShort
      val effSymEnd = SourcePosition(loc.sp2.source, loc.sp2.line, effEndColWithoutOpName)
      val effSymLoc = SourceLocation(loc.isReal, loc.sp1, effSymEnd)

      // Extract op symbol occurrence
      val opStartColWithoutOpName = (sepCol + 1).toShort
      val opSymStart = SourcePosition(loc.sp1.source, loc.sp1.line, opStartColWithoutOpName)
      val opSymLoc = SourceLocation(loc.isReal, opSymStart, loc.sp2)

      (Some(effSymLoc), opSymLoc)
    } else {
      (None, loc)
    }
  }

  private def getSigSymOccurs(sym: Symbol.SigSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.SigSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object SigSymConsumer extends Consumer {
      override def consumeSig(sig: TypedAst.Sig): Unit = consider(sig.sym, sig.sym.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = consider(symUse.sym, sepTrtAndSigSymOccur(symUse)._2)
    }

    Visitor.visitRoot(root, SigSymConsumer, AllAcceptor)

    println(root.instances)
    val implOccurs = root
      .instances(sym.trt)
      .flatMap(ins => ins.defs)
      .filter(defn => defn.sym.text == sym.name)
      .map(defn => defn.sym.loc)

    occurs ++ implOccurs
  }

  private def getTraitSymOccurs(sym: Symbol.TraitSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.TraitSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object TraitSymConsumer extends Consumer {
      override def consumeTrait(traitt: TypedAst.Trait): Unit = consider(traitt.sym, traitt.sym.loc)
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = consider(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = consider(tcHead.sym, tcHead.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = sepTrtAndSigSymOccur(symUse)._1.foreach(traitLoc => consider(symUse.sym.trt, traitLoc))

    }

    Visitor.visitRoot(root, TraitSymConsumer, AllAcceptor)

    occurs
  }

  private def sepTrtAndSigSymOccur(symUse: SymUse.SigSymUse): (Option[SourceLocation], SourceLocation) = {
    val SymUse.SigSymUse(sym, loc) = symUse
    val traitNameLen = sym.trt.name.length
    val occurLen = loc.sp2.col - loc.sp1.col
    val sigWithTraitNameLen = traitNameLen + sym.name.length + 1

    val occurContainsTraitName = occurLen == sigWithTraitNameLen
    if (occurContainsTraitName) {
      val sepCol = loc.sp1.col + traitNameLen

      // Extract trait symbol SourceLocation
      val traitEndColWithoutSigName = sepCol.toShort
      val traitSymEnd = SourcePosition(loc.sp2.source, loc.sp2.line, traitEndColWithoutSigName)
      val traitSymLoc = SourceLocation(loc.isReal, loc.sp1, traitSymEnd)

      // Extract sig symbol SourceLocation
      val sigStartColWithoutTraitName = (sepCol + 1).toShort
      val sigSymStart = SourcePosition(loc.sp1.source, loc.sp1.line, sigStartColWithoutTraitName)
      val sigSymLoc = SourceLocation(loc.isReal, sigSymStart, loc.sp2)

      (Some(traitSymLoc), sigSymLoc)
    } else {
      (None, loc)
    }
  }

  private def getStructFieldSymOccurs(sym: Symbol.StructFieldSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.StructFieldSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object StructFieldSymConsumer extends Consumer {
      override def consumeStructField(field: TypedAst.StructField): Unit = consider(field.sym, field.sym.loc)
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = consider(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, AllAcceptor)

    occurs
  }

  private def getStructSymOccurs(sym: Symbol.StructSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.StructSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object StructSymConsumer extends Consumer {
      override def consumeStruct(struct: TypedAst.Struct): Unit = consider(struct.sym, struct.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Struct(sym, _), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, StructSymConsumer, AllAcceptor)

    occurs
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
