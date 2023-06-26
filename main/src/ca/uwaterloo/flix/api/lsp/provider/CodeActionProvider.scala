/*
 * Copyright 2023 Holger Dal Mogensen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{CodeAction, CodeActionContext, CodeActionKind, Entity, Index, Position, Range, TextEdit, WorkspaceEdit}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.{RedundancyError, ResolutionError, TypeError}

object CodeActionProvider {

  def getCodeActions(uri: String, range: Range, context: CodeActionContext, currentErrors: List[CompilationMessage])(implicit index: Index, root: Root, flix: Flix): List[CodeAction] = {
    getActionsFromErrors(uri, range, currentErrors) ++
      getActionsFromIndex(uri, range, currentErrors)
  }

  /**
    * Returns code actions based on the current errors.
    */
  private def getActionsFromErrors(uri: String, range: Range, currentErrors: List[CompilationMessage])(implicit index: Index, root: Root, flix: Flix): List[CodeAction] = currentErrors.flatMap {
    case ResolutionError.UndefinedName(qn, ns, _, _, loc) if onSameLine(range, loc) =>
      if (qn.namespace.isRoot)
        mkUseDef(qn.ident, uri)
      else
        Nil

    case RedundancyError.UnusedVarSym(sym) if onSameLine(range, sym.loc) =>
      mkUnusedVarCodeAction(sym, uri) :: Nil

    case ResolutionError.UndefinedType(qn, ns, loc) if onSameLine(range, loc) =>
      mkIntroduceNewEnum(qn.ident.name, uri) :: Nil

    case _ => Nil
  }

  /**
    * Returns code actions based on the current index and the given range.
    */
  private def getActionsFromIndex(uri: String, range: Range, currentErrors: List[CompilationMessage])(implicit index: Index, root: Root, flix: Flix): List[CodeAction] =
    index.query(uri, range.start) match {
      case None => Nil // No code actions.

      case Some(entity) => entity match {
        case Entity.Enum(e) =>
          val sym = e.sym
          List(mkDeriveEq(sym, uri), mkDeriveOrder(sym, uri), mkDeriveToString(sym, uri))
        case _ =>
          Nil // No code actions.
      }
    }

  /**
    * Returns a code action that proposes to `use` a def.
    *
    * For example, if we have:
    *
    * {{{
    *   map(x -> x + 1, Nil)
    * }}}
    *
    * then we propose to insert:
    *
    * {{{
    *   use List.map;
    * }}}
    */
  private def mkUseDef(ident: Name.Ident, uri: String)(implicit root: Root): List[CodeAction] = {
    root.defs.collect {
      case (sym, _) if sym.name == ident.name =>
        CodeAction(
          title = s"use $sym",
          kind = CodeActionKind.QuickFix,
          edit = Some(WorkspaceEdit(
            Map(uri -> List(TextEdit(
              Range(Position(0, 0), Position(0, 0)), // TODO: We should figure out where to best place the use.
              s"use $sym;\n"
            )))
          )),
          command = None
        )
    }.toList.sortBy(_.title)
  }

  /**
    * Returns a code action that proposes to prefix an unused variable by an underscore.
    *
    * For example, if we have:
    *
    * {{{
    *   let abc = 123
    * }}}
    *
    * where `abc` is unused this code action proposes to replace it by `_abc`.
    */
  private def mkUnusedVarCodeAction(sym: Symbol.VarSym, uri: String): CodeAction = CodeAction(
    title = s"Prefix unused variable with underscore",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position.fromBegin(sym.loc), Position.fromBegin(sym.loc)),
        s"_"
      )))
    )),
    command = None
  )

  /**
    * Returns a code action that proposes to create a new enum.
    *
    * For example, if we have:
    *
    * {{{
    *   def foo(): Abc = ???
    * }}}
    *
    * where the `Abc` type is not defined this code action proposes to add:
    * {{{
    *   enum Abc { }
    * }}}
    */
  private def mkIntroduceNewEnum(name: String, uri: String): CodeAction = CodeAction(
    title = s"Introduce new enum $name",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position(0, 0), Position(0, 0)), // TODO: We should figure out where to best place the new enum.
        s"""
           |enum $name {
           |
           |}
           |""".stripMargin
      )))
    )),
    command = None
  )

  // TODO: We should only offer to derive type classes which have not already been derived.

  /**
    * Returns a code action to derive the `Eq` type class.
    */
  private def mkDeriveEq(sym: Symbol.EnumSym, uri: String): CodeAction = mkDerive(sym, "Eq", uri)

  /**
    * Returns a code action to derive the `Order` type class.
    */
  private def mkDeriveOrder(sym: Symbol.EnumSym, uri: String): CodeAction = mkDerive(sym, "Order", uri)

  /**
    * Returns a code action to derive the `ToString` type class.
    */
  private def mkDeriveToString(sym: Symbol.EnumSym, uri: String): CodeAction = mkDerive(sym, "ToString", uri)

  // TODO: Add derivation for the Hash and Sendable type classes.

  /**
    * Returns a code action to derive the given type class `clazz` for the given enum symbol `sym`.
    */
  private def mkDerive(sym: Symbol.EnumSym, clazz: String, uri: String): CodeAction = CodeAction(
    title = s"Derive $clazz",
    kind = CodeActionKind.Refactor,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position.fromEnd(sym.loc), Position.fromEnd(sym.loc)),
        s" with $clazz"
      )))
    )),
    command = None
  )

  /**
    * Returns `true` if the given `range` starts on the same line as the given source location `loc`.
    */
  // TODO: We should introduce a mechanism that checks if the given range *overlaps* with the given loc.
  private def onSameLine(range: Range, loc: SourceLocation): Boolean = range.start.line == loc.beginLine

}
