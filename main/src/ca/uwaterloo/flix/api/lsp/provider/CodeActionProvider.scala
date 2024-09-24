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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.{InstanceError, RedundancyError, ResolutionError, TypeError}
import ca.uwaterloo.flix.util.Similarity

object CodeActionProvider {

  def getCodeActions(uri: String, range: Range, context: CodeActionContext, currentErrors: List[CompilationMessage])(implicit index: Index, root: Root, flix: Flix): List[CodeAction] = {
    getActionsFromErrors(uri, range, currentErrors) ++ getActionsFromIndex(uri, range, currentErrors)
  }

  /** Returns code actions based on the current errors. */
  private def getActionsFromErrors(uri: String, range: Range, currentErrors: List[CompilationMessage])(implicit index: Index, root: Root, flix: Flix): List[CodeAction] = currentErrors.flatMap {
    case ResolutionError.UndefinedName(qn, _, env, _, loc) if onSameLine(range, loc) =>
      if (qn.namespace.isRoot)
        mkUseDef(qn.ident, uri) ++ mkFixMisspelling(qn, loc, env, uri)
      else
        Nil
    case ResolutionError.UndefinedTrait(qn, _, loc) if onSameLine(range, loc) =>
      if (qn.namespace.isRoot)
        mkUseTrait(qn.ident, uri)
      else
        Nil
    case ResolutionError.UndefinedEffect(qn, _, loc) if onSameLine(range, loc) =>
      if (qn.namespace.isRoot)
        mkUseEffect(qn.ident, uri)
      else
        Nil
    case ResolutionError.UndefinedType(qn, _, loc) if onSameLine(range, loc) =>
      mkIntroduceNewEnum(qn.ident.name, uri) :: {
        if (qn.namespace.isRoot)
          mkUseType(qn.ident, uri)
        else
          Nil
      }

    case RedundancyError.UnusedVarSym(sym) if onSameLine(range, sym.loc) =>
      mkUnusedVarCodeAction(sym, uri) :: Nil
    case RedundancyError.UnusedDefSym(sym) if onSameLine(range, sym.loc) =>
      mkUnusedDefCodeAction(sym, uri) :: Nil
    case RedundancyError.UnusedFormalParam(sym) if onSameLine(range, sym.loc) =>
      mkUnusedParamCodeAction(sym, uri) :: Nil
    case RedundancyError.UnusedTypeParam(sym, _) if onSameLine(range, sym.loc) =>
      mkUnusedTypeParamCodeAction(sym, uri) :: Nil
    case RedundancyError.UnusedEffectSym(sym) if onSameLine(range, sym.loc) =>
      mkUnusedEffectCodeAction(sym, uri) :: Nil
    case RedundancyError.UnusedEnumSym(sym) if onSameLine(range, sym.loc) =>
      mkUnusedEnumCodeAction(sym, uri) :: Nil
    case RedundancyError.UnusedEnumTag(_, caseSym) if onSameLine(range, caseSym.loc) =>
      mkUnusedEnumTagCodeAction(caseSym, uri) :: Nil

    case TypeError.MissingInstanceEq(tpe, _, loc) if onSameLine(range, loc) =>
      mkDeriveMissingEq(tpe, uri)
    case TypeError.MissingInstanceOrder(tpe, _, loc) if onSameLine(range, loc) =>
      mkDeriveMissingOrder(tpe, uri)
    case TypeError.MissingInstanceToString(tpe, _, loc) if onSameLine(range, loc) =>
      mkDeriveMissingToString(tpe, uri)
    case InstanceError.MissingSuperTraitInstance(tpe, sub, sup, loc) if onSameLine(range, loc) =>
      mkDeriveMissingSuperTrait(tpe, sup, uri)
    case _ => Nil
  }

  /** Returns code actions based on the current index and the given range. */
  private def getActionsFromIndex(uri: String, range: Range, currentErrors: List[CompilationMessage])(implicit index: Index, root: Root, flix: Flix): List[CodeAction] =
    index.query(uri, range.start) match {
      case None => Nil // No code actions.

      case Some(entity) => entity match {
        case Entity.Enum(e) =>
          List(mkDeriveEq(e, uri), mkDeriveOrder(e, uri), mkDeriveToString(e, uri)).flatten
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
    val syms = root.defs.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri)
  }

  /** Returns a code action that proposes to `use` a trait. */
  private def mkUseTrait(ident: Name.Ident, uri: String)(implicit root: Root): List[CodeAction] = {
    val syms = root.traits.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri)
  }

  /** Returns a code action that proposes to `use` an effect. */
  private def mkUseEffect(ident: Name.Ident, uri: String)(implicit root: Root): List[CodeAction] = {
    val syms = root.effects.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri)
  }

  /** Returns a code action that proposes to `use` a type. */
  private def mkUseType(ident: Name.Ident, uri: String)(implicit root: Root): List[CodeAction] = {
    val names = root.enums.map { case (sym, _) => sym.name } ++
      root.restrictableEnums.map { case (sym, _) => sym.name } ++
      root.traits.map { case (sym, _) => sym.name } ++
      root.typeAliases.map { case (sym, _) => sym.name }

    val syms = (root.enums ++ root.restrictableEnums ++ root.traits ++ root.typeAliases).map {
      case (sym, _) => sym
    }

    mkUseSym(ident, names, syms, uri)
  }

  /**
    * Internal helper function for all `mkUseX`.
    * Returns a list of code action that proposes to `use` a symbol.
    *
    * For example, if we have:
    *
    * {{{
    *   map(x -> x + 1, Nil)
    * }}}
    *
    * and we pass in the list of defs in the root,
    * then we propose to insert:
    *
    * {{{
    *   use List.map;
    * }}}
    *
    * @param ident The identifier we are searching for.
    * @param syms  The symbols that are candidates for being used.
    * @param names The names of the symbols given as `syms`.
    *              These should include the same number and be in the same order.
    * @param uri   URI of the document the change should be made in.
    */
  // Names have to be included seperately because symbols aren't guaranteed to have a name
  private def mkUseSym(ident: Name.Ident, names: Iterable[String], syms: Iterable[Symbol], uri: String): List[CodeAction] = {
    syms.zip(names).collect {
      case (sym, name) if name == ident.name =>
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
  private def mkUnusedVarCodeAction(sym: Symbol.VarSym, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      "Prefix unused variable with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /** Returns a code action that proposes to prefix the name of an unused function by an underscore. */
  private def mkUnusedDefCodeAction(sym: Symbol.DefnSym, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      "Prefix unused function with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /** Returns a code action that proposes to prefix the name of an unused formal parameter by an underscore. */
  private def mkUnusedParamCodeAction(sym: Symbol.VarSym, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      "Prefix unused parameter with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /** Returns a code action that proposes to prefix the name of an unused type parameter by an underscore. */
  private def mkUnusedTypeParamCodeAction(sym: Name.Ident, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      "Prefix unused type parameter with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /** Returns a code action that proposes to prefix the name of an unused effect by an underscore. */
  private def mkUnusedEffectCodeAction(sym: Symbol.EffectSym, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      "Prefix unused effect with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /** Returns a code action that proposes to prefix the name of an unused enum by an underscore. */
  private def mkUnusedEnumCodeAction(sym: Symbol.EnumSym, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      s"Prefix unused enum with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /** Returns a code action that proposes to prefix the name of an unused enum case by an underscore. */
  private def mkUnusedEnumTagCodeAction(sym: Symbol.CaseSym, uri: String): CodeAction =
    mkPrefixWithUnderscore(
      s"Prefix unused case with underscore",
      Position.fromBegin(sym.loc),
      uri,
    )

  /**
    * Internal helper function for all `mkUnusedXCodeAction`.
    * Returns a code action that proposes to insert an underscore at `beginPos`.
    */
  private def mkPrefixWithUnderscore(title: String, beginPos: Position, uri: String): CodeAction = CodeAction(
    title,
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(beginPos, beginPos),
        "_"
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

  /**
    * Returns a list of quickfix code action to suggest possibly correct spellings.
    * Uses Levenshtein Distance to find closed spellings.
    */
  private def mkFixMisspelling(qn: Name.QName, loc: SourceLocation, env: Map[String, Symbol.VarSym], uri: String): List[CodeAction] = {
    val minLength = 3
    val maxDistance = 3
    val possibleNames: List[String] = env.toList.map(_._1).filter(n => (n.length - qn.ident.name.length).abs < maxDistance)
                                                          .filter(n => Similarity.levenshtein(n, qn.ident.name) < maxDistance)
    if (qn.ident.name.length > minLength)
      possibleNames.map(n => mkCorrectSpelling(n, loc, uri))
    else
      Nil
  }

  /**
    * Internal helper function for `mkFixMisspelling`.
    * Returns a quickfix code action for a possibly correct name.
    */
  private def mkCorrectSpelling(correctName: String, loc: SourceLocation, uri: String): CodeAction =
    CodeAction(
      title = s"Did you mean: `$correctName`?",
      kind = CodeActionKind.QuickFix,
      edit = Some(WorkspaceEdit(
        Map(uri -> List(TextEdit(
          Range(Position.fromBegin(loc), Position.fromEnd(loc)),
          correctName
        )))
      )),
      command = None
    )

  /** Returns a quickfix code action to derive the `Eq` trait for the given type `tpe` if it is an enum. */
  private def mkDeriveMissingEq(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "Eq", uri)

  /** Returns a quickfix code action to derive the `Order` trait for the given type `tpe` if it is an enum. */
  private def mkDeriveMissingOrder(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "Order", uri)

  /** Returns a quickfix code action to derive the `ToString` trait for the given type `tpe` if it is an enum. */
  private def mkDeriveMissingToString(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "ToString", uri)

  /** Returns a quickfix code action to derive the missing supertrait for the given subtrait. */
  private def mkDeriveMissingSuperTrait(tpe: Type, superTrait: Symbol.TraitSym, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, superTrait.name, uri)

  /**
    * Internal helper function for all `mkDeriveMissingX`.
    * Returns a quickfix code action to derive the given trait `trt`
    * for the given type `tpe` if it is an enum in the root.
    */
  private def mkDeriveMissing(tpe: Type, trt: String, uri: String)(implicit root: Root): Option[CodeAction] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) =>
      root.enums.get(sym).map { e =>
        CodeAction(
          title = s"Derive $trt",
          kind = CodeActionKind.QuickFix,
          edit = Some(addDerivation(e, trt, uri)),
          command = None
        )
      }
    case _ => None
  }

  // TODO: We should only offer to derive traits which have not already been derived.

  /** Returns a code action to derive the `Eq` trait. */
  private def mkDeriveEq(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "Eq", uri)

  /** Returns a code action to derive the `Order` trait. */
  private def mkDeriveOrder(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "Order", uri)

  /** Returns a code action to derive the `ToString` trait. */
  private def mkDeriveToString(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "ToString", uri)

  // TODO: Add derivation for the Hash and Sendable traits.

  /**
    * Returns a code action to derive the given trait `trt` for the given enum `e` if it isn't already.
    * `None` otherwise.
    */
  private def mkDerive(e: TypedAst.Enum, trt: String, uri: String): Option[CodeAction] = {
    val alreadyDerived = e.derives.traits.exists(d => d.trt.name == trt)
    if (alreadyDerived) None
    else Some(
      CodeAction(
        title = s"Derive $trt",
        kind = CodeActionKind.Refactor,
        edit = Some(addDerivation(e, trt, uri)),
        command = None
      )
    )
  }

  /**
    * Returns a workspace edit that properly derives the given trait, `trt`, for the enum, `e`.
    *
    * For example, if we have:
    * {{{
    *   enum Abc {}
    * }}}
    * we could derive the trait 'Eq' like so:
    * {{{
    *   enum Abc with Eq { }
    * }}}
    *
    * Or if there are already other derivations present:
    * {{{
    *   enum Abc with ToString {}
    * }}}
    * it will be appended at the end:
    * {{{
    *   enum Abc with ToString, Eq { }
    * }}}
    */
  private def addDerivation(e: TypedAst.Enum, trt: String, uri: String): WorkspaceEdit = {
    val text =
      if (e.derives.traits.isEmpty)
        s" with $trt"
      else
        s", $trt"

    val end = Position.fromEnd(e.derives.loc)

    WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(end, end),
        text
      )))
    )
  }

  /** Returns `true` if the given `range` starts on the same line as the given source location `loc`. */
  // TODO: We should introduce a mechanism that checks if the given range *overlaps* with the given loc.
  private def onSameLine(range: Range, loc: SourceLocation): Boolean = range.start.line == loc.beginLine

}
