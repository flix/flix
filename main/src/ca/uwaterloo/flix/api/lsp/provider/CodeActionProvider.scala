/*
 * Copyright 2023 Holger Dal Mogensen
 * Copyright 2024 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.{CodeAction, CodeActionKind, Position, Range, TextEdit, WorkspaceEdit}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.{InstanceError, ResolutionError, TypeError}
import ca.uwaterloo.flix.util.{Similarity, ClassMap}

/**
  * The CodeActionProvider offers quickfix suggestions.
  *
  * The space of possible quickfixes is huge. Hence, we should only offer quickfixes that are meaningfully useful.
  *
  * That is, quickfix suggestions that will often be used: either because they offer good ergonomics or because they resolve a tricky issue.
  */
object CodeActionProvider {

  def getCodeActions(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[CodeAction] = {
    getActionsFromErrors(uri, range, errors) ++ getActionsFromRange(uri, range)
  }

  private def getActionsFromErrors(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[CodeAction] = errors.flatMap {
    case ResolutionError.UndefinedEffect(qn, _, loc) if overlaps(range, loc) =>
      if (qn.namespace.isRoot)
        mkUseEffect(qn.ident, uri)
      else
        Nil

    case ResolutionError.UndefinedJvmClass(name, _, loc) if overlaps(range, loc) =>
      mkImportJava(name, uri)

    case ResolutionError.UndefinedName(qn, _, env, _, loc) if overlaps(range, loc) =>
    mkImportJava(qn.ident.name, uri) ++ mkNewDef(qn.ident.name, uri) ++ {
      if (qn.namespace.isRoot)
        mkUseDef(qn.ident, uri) ++ mkFixMisspelling(qn, loc, env, uri)
      else
        Nil
    }

    case ResolutionError.UndefinedTrait(qn, _, loc) if overlaps(range, loc) =>
      if (qn.namespace.isRoot)
        mkUseTrait(qn.ident, uri)
      else
        Nil

    case ResolutionError.UndefinedType(qn, _, loc) if overlaps(range, loc) =>
      mkNewEnum(qn.ident.name, uri) :: mkNewStruct(qn.ident.name, uri) :: {
        if (qn.namespace.isRoot)
          mkUseType(qn.ident, uri)
        else
          Nil
      }

    case TypeError.MissingInstanceEq(tpe, _, loc) if overlaps(range, loc) =>
      mkDeriveMissingEq(tpe, uri)

    case TypeError.MissingInstanceOrder(tpe, _, loc) if overlaps(range, loc) =>
      mkDeriveMissingOrder(tpe, uri)

    case TypeError.MissingInstanceToString(tpe, _, loc) if overlaps(range, loc) =>
      mkDeriveMissingToString(tpe, uri)

    case InstanceError.MissingSuperTraitInstance(tpe, _, sup, loc) if overlaps(range, loc) =>
      mkDeriveMissingSuperTrait(tpe, sup, uri)

    case _ => Nil
  }

  /**
    * Returns code actions based on the current index and the given range.
    */
  private def getActionsFromRange(uri: String, range: Range)(implicit root: Root): List[CodeAction] = {
    root.enums.foldLeft(List.empty[CodeAction]) {
      case (acc, (sym, enm)) if overlaps(range, sym.loc) =>
        List(mkDeriveEq(enm, uri), mkDeriveOrder(enm, uri), mkDeriveToString(enm, uri)).flatten ::: acc
      case (acc, _) => acc
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

  /**
    * Returns a code action that proposes to `use` a trait.
    */
  private def mkUseTrait(ident: Name.Ident, uri: String)(implicit root: Root): List[CodeAction] = {
    val syms = root.traits.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri)
  }

  /**
    * Returns a code action that proposes to `use` an effect.
    */
  private def mkUseEffect(ident: Name.Ident, uri: String)(implicit root: Root): List[CodeAction] = {
    val syms = root.effects.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri)
  }

  /**
    * Returns a code action that proposes to `use` a type.
    */
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
  // Names have to be included separately because symbols aren't guaranteed to have a name
  private def mkUseSym(ident: Name.Ident, names: Iterable[String], syms: Iterable[Symbol], uri: String): List[CodeAction] = {
    syms.zip(names).collect {
      case (sym, name) if name == ident.name =>
        CodeAction(
          title = s"use $sym",
          kind = CodeActionKind.QuickFix,
          edit = Some(WorkspaceEdit(
            Map(uri -> List(TextEdit(
              Range(Position(1, 1), Position(1, 1)), // TODO: We should figure out where to best place the use.
              s"use $sym;\n"
            )))
          )),
          command = None
        )
    }.toList.sortBy(_.title)
  }

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
  private def mkNewEnum(name: String, uri: String): CodeAction = CodeAction(
    title = s"Introduce new enum $name",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position(1, 1), Position(1, 1)), // TODO: We should figure out where to best place the new enum.
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
    * Returns a code action that proposes to import corresponding Java class.
    *
    * For example, if we have:
    *
    * {{{
    *   def foo(): = new File("data.txt")
    * }}}
    *
    * where the undefined class `File` is a valid Java class, this code action proposes to add:
    * {{{
    *   import java.io.File
    * }}}
    */
  private def mkImportJava(name: String, uri: String): List[CodeAction] = {
    ClassMap.TheMap.get(name).map { path =>
      CodeAction(
        title = s"Import $name from Java",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(
          Map(uri -> List(TextEdit(
            Range(Position(1, 1), Position(1, 1)),
            s"import $path\n"
          )))
        )),
        command = None
      )
    }
  }.toList

  /**
   * Returns a code action that proposes to create a new function.
   *
   * For example, if we have:
   *
   * {{{
   *   let x = f()
   * }}}
   *
   * where the name `f` is not defined this code action proposes to add:
   * {{{
   *   def f(): =
   * }}}
   */
  private def mkNewDef(name: String, uri: String): CodeAction = CodeAction(
    title = s"Introduce new function $name",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position(1, 1), Position(1, 1)),
        s"""
           |def $name(): =
           |
           |""".stripMargin
      )))
    )),
    command = None
  )

  /**
    * Returns a code action that proposes to create a new struct.
    *
    * For example, if we have:
    *
    * {{{
    *   def foo(): Abc = ???
    * }}}
    *
    * where the `Abc` type is not defined this code action proposes to add:
    * {{{
    *   struct Abc[r] { }
    * }}}
    */
  private def mkNewStruct(name: String, uri: String): CodeAction = CodeAction(
    title = s"Introduce new struct $name",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position(1, 1), Position(1, 1)),
        s"""
           |struct $name[r] {
           |
           |}
           |""".stripMargin
      )))
    )),
    command = None
  )



  /**
    * Returns a list of quickfix code action to suggest possibly correct spellings.
    *
    * Uses Levenshtein Distance to find close spellings.
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

  /**
    * Returns a quickfix code action to derive the `Eq` trait for the given type `tpe` if it is an enum.
    */
  private def mkDeriveMissingEq(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "Eq", uri)

  /**
    * Returns a quickfix code action to derive the `Order` trait for the given type `tpe` if it is an enum.
    */
  private def mkDeriveMissingOrder(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "Order", uri)

  /**
    * Returns a quickfix code action to derive the `ToString` trait for the given type `tpe` if it is an enum.
    */
  private def mkDeriveMissingToString(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "ToString", uri)

  /**
    * Returns a quickfix code action to derive the missing supertrait for the given subtrait.
    */
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

  /**
    * Returns a code action to derive the `Eq` trait.
    */
  private def mkDeriveEq(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "Eq", uri)

  /**
    * Returns a code action to derive the `Order` trait.
    */
  private def mkDeriveOrder(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "Order", uri)

  /**
    * Returns a code action to derive the `ToString` trait.
    */
  private def mkDeriveToString(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "ToString", uri)

  /**
    * Returns a code action to derive the given trait `trt` for the given enum `e` if it isn't already.
    * `None` otherwise.
    */
  private def mkDerive(e: TypedAst.Enum, trt: String, uri: String): Option[CodeAction] = {
    val alreadyDerived = e.derives.traits.exists(d => d.trt.name == trt)
    if (alreadyDerived)
      None
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

    // Compute the end source location.
    // If there is already a derives clause we use its source location.
    // Otherwise, we use the source location of the enum symbol.
    val end = if (e.derives.loc != SourceLocation.Unknown) e.derives.loc else e.sym.loc

    WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position.fromEnd(end), Position.fromEnd(end)),
        text
      )))
    )
  }

  /**
    * Returns `true` if the given `range` starts on the same line as the given source location `loc`.
    */
  private def overlaps(range: Range, loc: SourceLocation): Boolean = {
    val range2 = sourceLocation2Range(loc)
    range.overlapsWith(range2)
  }

  private def sourcePosition2Position(sourcePosition: SourcePosition): Position = {
    Position(sourcePosition.line, sourcePosition.col)
  }

  private def sourceLocation2Range(sourceLocation: SourceLocation): Range = {
    Range(sourcePosition2Position(sourceLocation.sp1), sourcePosition2Position(sourceLocation.sp2))
  }
}
