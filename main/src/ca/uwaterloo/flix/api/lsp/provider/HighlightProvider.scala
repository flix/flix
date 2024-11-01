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
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.Exception
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit root: Root): JObject = {
    val stackConsumer = StackConsumer()
    Visitor.visitRoot(root, stackConsumer, Visitor.InsideAcceptor(uri, pos))

    stackConsumer.getStack.headOption match {
      case None => mkNotFound(uri, pos)
      case Some(x) => highlightAny(uri, pos, x)
    }

  }

  private def highlightAny(uri: String, pos: Position, x: AnyRef)(implicit root: Root): JObject = x match {
    case Expr.Var(varSym, _, loc) => highlightVarSym(uri, varSym, loc)
    case Binder(sym, _) => highlightVarSym(uri, sym, sym.loc)
    case Case(sym, _, _, _) => highlightCaseSym(uri, sym, sym.loc)
    case SymUse.CaseSymUse(sym, loc) => highlightCaseSym(uri, sym, loc)
    case TypedAst.Def(sym, _, _, _) => highlightDefnSym(uri, sym, sym.loc)
    case SymUse.DefSymUse(sym, loc) => highlightDefnSym(uri, sym, loc)
    case _ => mkNotFound(uri, pos)
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
