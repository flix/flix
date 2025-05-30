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

import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils
import ca.uwaterloo.flix.api.lsp.{CodeAction, CodeActionKind, Position, Range, TextEdit, WorkspaceEdit}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.AnchorPosition
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol}
import ca.uwaterloo.flix.language.errors.ResolutionError

/**
  * The CodeActionProvider offers quickfix suggestions.
  *
  * The space of possible quickfixes is huge. Hence, we should only offer quickfixes that are meaningfully useful.
  *
  * That is, quickfix suggestions that will often be used: either because they offer good ergonomics or because they resolve a tricky issue.
  */
object CodeActionProvider {

  def getCodeActions(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[CodeAction] = {
    getActionsFromErrors(uri, range, errors)
  }

  private def getActionsFromErrors(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[CodeAction] = errors.flatMap {
    case ResolutionError.UndefinedEffect(qn, ap, _, _, loc) if overlaps(range, loc) =>
      mkUseEffect(qn.ident, uri, ap)

    case ResolutionError.UndefinedStruct(qn, ap, loc) if overlaps(range, loc) =>
      mkUseStruct(qn.ident, uri, ap)

    case ResolutionError.UndefinedJvmImport(name, ap, _, loc) if overlaps(range, loc) =>
      mkImportJava(Name.mkQName(name), uri, ap)

    case ResolutionError.UndefinedName(qn, ap, _, loc) if overlaps(range, loc) =>
      mkUseDef(qn.ident, uri, ap) ++ mkImportJava(qn, uri, ap)

    case ResolutionError.UndefinedTrait(qn, _, ap, _, _, loc) if overlaps(range, loc) =>
      mkUseTrait(qn.ident, uri, ap)

    case ResolutionError.UndefinedTag(name, ap, _, _, loc) if overlaps(range, loc) =>
      mkUseTag(name.ident.name, uri, ap) ++ mkQualifyTag(name.ident.name, uri, loc)

    case ResolutionError.UndefinedType(qn, _, ap, _, loc) if overlaps(range, loc) =>
      mkUseType(qn.ident, uri, ap) ++ mkImportJava(qn, uri, ap)

    case _ => Nil
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
  private def mkUseDef(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.defs.collect {
      case (sym, defn) if CompletionUtils.isAvailable(defn) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to `use` a trait.
    */
  private def mkUseTrait(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.traits.collect {
      case (sym, trt) if CompletionUtils.isAvailable(trt) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to `use` an effect.
    */
  private def mkUseEffect(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.effects.collect {
      case (sym, eff) if CompletionUtils.isAvailable(eff) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to use the tag of an enum.
    *
    * For example, if we have:
    *
    * {{{
    *   match color {
    *      case Red => ???
    *   }
    * }}}
    *
    * where the user actually want to refer to Color.Red, this code action proposes to add:
    * {{{
    *   use Color.Red
    * }}}
    */
  private def mkUseTag(tagName: String, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val candidateEnums = root.enums.filter { case (_, enm) => enm.cases.keys.exists(_.name == tagName) && CompletionUtils.isAvailable(enm) }
    candidateEnums.keys.map { enumName =>
      CodeAction(
        title = s"use '$enumName.$tagName'",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(Map(uri -> List(mkTextEdit(ap, s"use $enumName.$tagName"))))),
        command = None
      )
    }.toList
  }

  /**
    * Returns a code action that proposes to qualify the tag with the enum name.
    *
    * For example, if we have:
    *
    * {{{
    *   match color {
    *      case Red => ???
    *   }
    * }}}
    *
    * where the user actually want to refer to Color.Red, this code action proposes to replace the tag with:
    * {{{
    *   case Color.Red => ???
    * }}}
    */
  private def mkQualifyTag(tagName: String, uri: String, loc: SourceLocation)(implicit root: Root): List[CodeAction] = {
    val candidateEnums = root.enums.filter { case (_, enm) => enm.cases.keys.exists(_.name == tagName) && CompletionUtils.isAvailable(enm) }
    candidateEnums.keys.map { enumName =>
      CodeAction(
        title = s"Prefix with '$enumName.'",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(Map(uri -> List(TextEdit(sourceLocation2Range(loc), s"$enumName.$tagName"))))),
        command = None
      )
    }.toList
  }

  /**
    * Returns a code action that proposes to `use` a type.
    */
  private def mkUseType(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val enumNames = root.enums.collect { case (sym, enm) if CompletionUtils.isAvailable(enm) => sym.name }
    val enumSyms = root.enums.collect { case (sym, enm) if CompletionUtils.isAvailable(enm) => sym }

    val restrictableEnumNames = root.restrictableEnums.collect { case (sym, enm) if CompletionUtils.isAvailable(enm) => sym.name }
    val restrictableEnumSyms = root.restrictableEnums.collect { case (sym, enm) if CompletionUtils.isAvailable(enm) => sym }

    val traitNames = root.traits.collect { case (sym, trt) if CompletionUtils.isAvailable(trt) => sym.name }
    val traitSyms = root.traits.collect { case (sym, trt) if CompletionUtils.isAvailable(trt) => sym }

    val typeAliasNames = root.typeAliases.collect { case (sym, alias) if CompletionUtils.isAvailable(alias) => sym.name }
    val typeAliasSyms = root.typeAliases.collect { case (sym, alias) if CompletionUtils.isAvailable(alias) => sym }

    val names = enumNames ++ restrictableEnumNames ++ traitNames ++ typeAliasNames
    val syms = enumSyms ++ restrictableEnumSyms ++ traitSyms ++ typeAliasSyms

    mkUseSym(ident, names, syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to `use` a struct.
    */
  private def mkUseStruct(ident: Name.Ident, uri: String, position: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.structs.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, position)
  }

  /**
    * Returns a TextEdit that is inserted and indented according to the given `ap`.
    * This function will:
    *   - add leadingSpaces before the given text.
    *   - add leadingSpaces after each newline.
    *   - add a newline at the end.
    *
    * Example:
    * Given text = "\ndef foo(): =\n", ap = AnchorPosition(line=1, col=0, spaces=4)
    * The result will be:
    * TextEdit(Range(Position(1, 0), Position(1, 0)), "    \n    def foo(): =\n    \n")
    */
  private def mkTextEdit(ap: AnchorPosition, text: String): TextEdit = {
    val insertPosition = Position(ap.line, ap.col)
    val leadingSpaces = " " * ap.spaces
    TextEdit(
      Range(insertPosition, insertPosition),
      leadingSpaces + text.replace("\n", s"\n$leadingSpaces") + "\n"
    )
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
  private def mkUseSym(ident: Name.Ident, names: Iterable[String], syms: Iterable[Symbol], uri: String, ap: AnchorPosition): List[CodeAction] =
    syms.zip(names).collect {
      case (sym, name) if name == ident.name =>
        CodeAction(
          title = s"use '$sym'",
          kind = CodeActionKind.QuickFix,
          edit = Some(WorkspaceEdit(Map(uri -> List(mkTextEdit(ap, s"use $sym;"))))),
          command = None
        )
    }.toList.sortBy(_.title)

  /**
    * Returns a code action that proposes to import the corresponding Java class.
    * First, we try to import the class with the name matching the head of the `qn.namespace.idents`.
    * If there is no namespace, we try to import the class with the name matching `qn.ident`.
    *
    * Example:
    * if we have
    *
    * {{{
    *  let a = Math.sin(1)
    * }}}
    *
    * where qn.ident is "sin" and qn.namespace.idents is ["Math"],  this code action proposes to add:
    *
    * {{{
    *  import java.lang.Math
    * }}}
    */
  private def mkImportJava(qn: Name.QName, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    // If `qn.namespace.idents.headOption` returns None, we use the `qn.ident.name`. Otherwise, we use the head of the namespace.
    // In the example above, headOption would return Some("Math"), so we will use "Math".
    val className = qn.namespace.idents.headOption.map(_.name).getOrElse(qn.ident.name)
    root.availableClasses.byClass.get(className).toList.flatten.map { path =>
      val completePath = path.mkString(".") + "." + className
      CodeAction(
        title = s"import '$completePath'",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(Map(uri -> List(mkTextEdit(ap, s"import $completePath"))))),
        command = None
      )
    }
  }

  /**
    * Returns `true` if the given `range` starts on the same line as the given source location `loc`.
    */
  private def overlaps(range: Range, loc: SourceLocation): Boolean = {
    val range2 = sourceLocation2Range(loc)
    range.overlapsWith(range2)
  }

  private def sourcePosition2Position(sourcePosition: SourcePosition): Position = {
    Position(sourcePosition.lineOneIndexed, sourcePosition.colOneIndexed)
  }

  private def sourceLocation2Range(sourceLocation: SourceLocation): Range = {
    Range(sourcePosition2Position(sourceLocation.sp1), sourcePosition2Position(sourceLocation.sp2))
  }
}
