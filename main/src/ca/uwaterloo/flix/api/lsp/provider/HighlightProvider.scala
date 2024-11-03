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
import ca.uwaterloo.flix.api.lsp.{DocumentHighlight, DocumentHighlightKind, Entity, Index, Position, Range, ResponseStatus, StackConsumer, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Case, Def, Expr, Pattern, Root}
import ca.uwaterloo.flix.language.ast.shared.{Modifiers, SymUse}
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.Exception
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
    case _ => mkNotFound(uri, pos)
  }

  private def highlightEffectSym(uri: String, sym: Symbol.EffectSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil
    def add(x: SourceLocation): Unit = {
      occurs = x :: occurs
    }
    def check(x: Symbol.EffectSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    object EffectSymConsumer extends Consumer {
      override def consumeEff(eff: TypedAst.Effect): Unit = check(eff.sym, eff.sym.loc)
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = check(effUse.sym, effUse.loc)
      override def consumeType(tpe: Type): Unit = getTypeSymOccur(tpe) match {
        case Some((sym: Symbol.EffectSym, loc)) => check(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightEnumSym(uri: String, sym: Symbol.EnumSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil
    def add(x: SourceLocation): Unit = {
      occurs = x :: occurs
    }
    def check(x: Symbol.EnumSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = check(enm.sym, enm.sym.loc)
      override def consumeType(tpe: Type): Unit = getTypeSymOccur(tpe) match {
        case Some((sym: Symbol.EnumSym, loc)) => check(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightStructFieldSym(uri: String, sym: Symbol.StructFieldSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil
    def add(x: SourceLocation): Unit = {
      occurs = x :: occurs
    }
    def check(x: Symbol.StructFieldSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    object StructFieldSymConsumer extends Consumer {
      override def consumeStructField(field: TypedAst.StructField): Unit = check(field.sym, field.sym.loc)
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = check(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, Visitor.FileAcceptor(uri))
    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightStructSym(uri: String, sym: Symbol.StructSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil
    def add(x: SourceLocation): Unit = {
      occurs = x :: occurs
    }
    def check(x: Symbol.StructSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    object StructSymConsumer extends Consumer {
      override def consumeStruct(struct: TypedAst.Struct): Unit = check(struct.sym, struct.sym.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.StructNew(sym, _, _, _, _, loc) => check(sym, loc)
        case _ => ()
      }
      override def consumeType(tpe: Type): Unit = getTypeSymOccur(tpe) match {
        case Some((sym: Symbol.StructSym, loc)) => check(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, StructSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightDefnSym(uri: String, sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil
    def add(x: SourceLocation): Unit = {
      occurs = x :: occurs
    }
    def check(x: Symbol.DefnSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = check(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = check(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightCaseSym(uri: String, sym: Symbol.CaseSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil
    def add(loc: SourceLocation): Unit = {
      occurs = loc :: occurs
    }

    def check(x: Symbol.CaseSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    object CaseSymConsumer extends Consumer {
      override def consumeCaseSymUse(sym: CaseSymUse): Unit = check(sym.sym, sym.loc)
      override def consumeCase(cse: TypedAst.Case): Unit = check(cse.sym, cse.sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightVarSym(uri: String, sym: Symbol.VarSym, loc: SourceLocation)(implicit root: Root): JObject = {
    var occurs: List[SourceLocation] = Nil

    def check(x: Symbol.VarSym, loc: SourceLocation): Unit = if (x == sym) { add(loc) }

    def add(loc: SourceLocation): Unit = {
      occurs = loc :: occurs
    }

    object VarSymConsumer extends Consumer {
      override def consumeLocalDefSym(symUse: SymUse.LocalDefSymUse): Unit = check(symUse.sym, symUse.loc)
      override def consumeBinder(bnd: TypedAst.Binder): Unit = check(bnd.sym, bnd.sym.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.Var(sym, _, loc) => check(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write)
    val reads = occurs.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))

  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
