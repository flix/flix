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
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Case, Expr, Pattern, Root}
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
    case Expr.Var(varSym, _, _) => highlightVarSym(uri, varSym)
    case Binder(sym, _) => highlightVarSym(uri, sym)
    case Case(sym, _, _, _) => highlightCaseSym(uri, sym)
    case CaseSymUse(sym, _) => highlightCaseSym(uri, sym)
    case _ => mkNotFound(uri, pos)
  }

  private def highlightCaseSym(uri: String, sym: Symbol.CaseSym)(implicit root: Root): JObject = {
    var occurs: List[Symbol.CaseSym] = Nil
    def add(x: Symbol.CaseSym): Unit = {
      occurs = x :: occurs
    }

    def check(x: Symbol.CaseSym): Unit = if (x == sym) { add(x) }

    object CaseSymConsumer extends Consumer {
      override def consumeCaseSymUse(sym: CaseSymUse): Unit = check(sym.sym)
      override def consumeCase(cse: TypedAst.Case): Unit = check(cse.sym)
    }

    Visitor.visitRoot(root, CaseSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(sym.loc), DocumentHighlightKind.Write)
    val reads = occurs.map(sym => DocumentHighlight(Range.from(sym.loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightVarSym(uri: String, sym: Symbol.VarSym)(implicit root: Root): JObject = {
    var occurs: List[Symbol.VarSym] = Nil

    def check(x: Symbol.VarSym): Unit = if (x == sym) { add(x) }

    def add(x: Symbol.VarSym): Unit = {
      occurs = x :: occurs
    }

    object VarSymConsumer extends Consumer {
      override def consumeLocalDefSym(symUse: SymUse.LocalDefSymUse): Unit = check(symUse.sym)
      override def consumeBinder(bnd: TypedAst.Binder): Unit = check(bnd.sym)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.Var(sym, _, _) => check(sym)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, Visitor.FileAcceptor(uri))

    val write = DocumentHighlight(Range.from(sym.loc), DocumentHighlightKind.Write)
    val reads = occurs.map(sym => DocumentHighlight(Range.from(sym.loc), DocumentHighlightKind.Read))

    val highlights = write :: reads

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))

  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
