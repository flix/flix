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

import ca.uwaterloo.flix.api.lsp.Visitor.Consumer
import ca.uwaterloo.flix.api.lsp.{DocumentHighlight, DocumentHighlightKind, Position, Range, ResponseStatus, StackConsumer, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Case, Expr, Root}
import ca.uwaterloo.flix.language.ast.shared.{SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

import scala.annotation.tailrec

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit root: Root): JObject = {
    val stackConsumer = StackConsumer()
    Visitor.visitRoot(root, stackConsumer, Visitor.InsideAcceptor(uri, pos))

    stackConsumer.getStack.headOption match {
      case None => mkNotFound(uri, pos)
      case Some(x) => highlightAny(uri, pos, x)
    }

  }

  @tailrec
  private def getTypeSymOccur(tpe: Type): Option[(Symbol, SourceLocation)] = tpe match {
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => Some(sym, loc)
    case Type.Cst(TypeConstructor.Struct(sym, _), loc) => Some(sym, loc)
    case Type.Cst(TypeConstructor.Effect(sym), loc) => Some(sym, loc)
    case Type.Apply(tpe1, _, _) => getTypeSymOccur(tpe1)
    case _ => None
  }

  private def highlightAny(uri: String, pos: Position, x: AnyRef)(implicit root: Root): JObject = x match {
    case Expr.Var(varSym, _, loc) => highlightVarSym(uri, varSym, loc)
    case Binder(sym, _) => highlightVarSym(uri, sym, sym.loc)
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) => highlightEnumSym(uri, sym, sym.loc)
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => highlightEnumSym(uri, sym, loc)
    case Case(sym, _, _, _) => highlightCaseSym(uri, sym, sym.loc)
    case SymUse.CaseSymUse(sym, loc) => highlightCaseSym(uri, sym, loc)
    case TypedAst.Def(sym, _, _, _) => highlightDefnSym(uri, sym, sym.loc)
    case SymUse.DefSymUse(sym, loc) => highlightDefnSym(uri, sym, loc)
    case TypedAst.Struct(_, _, _, sym, _, _, _, _) => highlightStructSym(uri, sym, sym.loc)
    case tpe: Type => getTypeSymOccur(tpe) match {
      case Some((sym: Symbol.StructSym, loc)) => highlightStructSym(uri, sym, loc)
      case Some((sym: Symbol.EnumSym, loc)) => highlightEnumSym(uri, sym, loc)
      case Some((sym: Symbol.EffectSym, loc)) => highlightEffectSym(uri, sym, loc)
      case _ => mkNotFound(uri, pos)
    }
    case TypedAst.StructField(sym, _, _) => highlightStructFieldSym(uri, sym, sym.loc)
    case SymUse.StructFieldSymUse(sym, loc) => highlightStructFieldSym(uri, sym, loc)
    case TypedAst.Effect(_, _, _, sym, _, _) => highlightEffectSym(uri, sym, sym.loc)
    case SymUse.EffectSymUse(sym, loc) => highlightEffectSym(uri, sym, loc)
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => highlightTraitSym(uri, sym, sym.loc)
    case SymUse.TraitSymUse(sym, loc) => highlightTraitSym(uri, sym, loc)
    case TraitConstraint.Head(sym, loc) => highlightTraitSym(uri, sym, loc)
    case Ast.Derivation(sym, loc) => highlightTraitSym(uri, sym, loc)
    case TypedAst.Sig(sym, _, _, _) => highlightSigSym(uri, sym, sym.loc)
    case SymUse.SigSymUse(sym, loc) => highlightSigSym(uri, sym, loc)
    case _ => mkNotFound(uri, pos)
  }

  private def highlightSigSym(uri: String, sym: Symbol.SigSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.SigSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object SigSymConsumer extends Consumer {
      override def consumeSig(sig: TypedAst.Sig): Unit = consider(sig.sym, sig.sym.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = consider(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, SigSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightTraitSym(uri: String, sym: Symbol.TraitSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.TraitSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object TraitSymConsumer extends Consumer {
      override def consumeTrait(traitt: TypedAst.Trait): Unit = consider(traitt.sym, traitt.sym.loc)
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = consider(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = consider(tcHead.sym, tcHead.loc)
      override def consumeDerivation(derive: Ast.Derivation): Unit = consider(derive.trt, derive.loc)
    }

    Visitor.visitRoot(root, TraitSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def mkHighlights(cursor: SourceLocation, occurs: List[SourceLocation]): JObject = {
    val write = DocumentHighlight(Range.from(cursor), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightEffectSym(uri: String, sym: Symbol.EffectSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.EffectSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object EffectSymConsumer extends Consumer {
      override def consumeEff(eff: TypedAst.Effect): Unit = consider(eff.sym, eff.sym.loc)
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = consider(effUse.sym, effUse.loc)
      override def consumeType(tpe: Type): Unit = getTypeSymOccur(tpe) match {
        case Some((sym: Symbol.EffectSym, loc)) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightEnumSym(uri: String, sym: Symbol.EnumSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = consider(enm.sym, enm.sym.loc)
      override def consumeType(tpe: Type): Unit = getTypeSymOccur(tpe) match {
        case Some((sym: Symbol.EnumSym, loc)) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightStructFieldSym(uri: String, sym: Symbol.StructFieldSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.StructFieldSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object StructFieldSymConsumer extends Consumer {
      override def consumeStructField(field: TypedAst.StructField): Unit = consider(field.sym, field.sym.loc)
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = consider(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightStructSym(uri: String, sym: Symbol.StructSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.StructSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object StructSymConsumer extends Consumer {
      override def consumeStruct(struct: TypedAst.Struct): Unit = consider(struct.sym, struct.sym.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.StructNew(sym, _, _, _, _, loc) => consider(sym, loc)
        case _ => ()
      }
      override def consumeType(tpe: Type): Unit = getTypeSymOccur(tpe) match {
        case Some((sym: Symbol.StructSym, loc)) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, StructSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightDefnSym(uri: String, sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = consider(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = consider(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightCaseSym(uri: String, sym: Symbol.CaseSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.CaseSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object CaseSymConsumer extends Consumer {
      override def consumeCaseSymUse(sym: CaseSymUse): Unit = consider(sym.sym, sym.loc)
      override def consumeCase(cse: TypedAst.Case): Unit = consider(cse.sym, cse.sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  private def highlightVarSym(uri: String, sym: Symbol.VarSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def consider(x: Symbol.VarSym, loc: SourceLocation): Unit = {
      if (x == sym) {
        occurs = loc :: occurs
      }
    }

    object VarSymConsumer extends Consumer {
      override def consumeLocalDefSym(symUse: SymUse.LocalDefSymUse): Unit = consider(symUse.sym, symUse.loc)
      override def consumeBinder(bnd: TypedAst.Binder): Unit = consider(bnd.sym, bnd.sym.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.Var(sym, _, loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, Visitor.FileAcceptor(uri))

    mkHighlights(loc, occurs)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
