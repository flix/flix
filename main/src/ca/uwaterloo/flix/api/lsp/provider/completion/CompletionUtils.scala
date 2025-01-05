/*
 * Copyright 2022 Paul Butcher, Lukas Rønn, Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Def
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{LocalScope, Resolution}
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.language.ast.Symbol
import scala.annotation.tailrec

object CompletionUtils {

  private def isUnitType(tpe: Type): Boolean = tpe == Type.Unit

  private def isUnitFunction(fparams: List[TypedAst.FormalParam]): Boolean = fparams.length == 1 && isUnitType(fparams.head.tpe)

  def getParamsLabelForEnumTags(cas: TypedAst.Case)(implicit flix: Flix): String = {
    cas.tpes.length match {
      case 0 => ""
      case _ => s"(${cas.tpes.map(FormatType.formatType(_)).mkString(", ")})"
    }
  }

  def getLabelForNameAndSpec(name: String, spec: TypedAst.Spec)(implicit flix: Flix): String = name + getLabelForSpec(spec)

  def getLabelForSpec(spec: TypedAst.Spec)(implicit flix: Flix): String = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, eff0, _, _) =>
      val args = if (isUnitFunction(fparams))
        Nil
      else
        fparams.map {
          fparam => s"${fparam.bnd.sym.text}: ${FormatType.formatType(fparam.tpe)}"
        }

      val retTpe = FormatType.formatType(retTpe0)

      val eff = eff0 match {
        case Type.Cst(TypeConstructor.Pure, _) => ""
        case p => raw" \ " + FormatType.formatType(p)
      }

      s"(${args.mkString(", ")}): $retTpe$eff"
  }

  /**
    * Generate a snippet which represents calling a function.
    * Drops the last one or two arguments in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    */
  def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): String = {
    val functionIsUnit = isUnitFunction(fparams)

    val args = fparams.dropRight(paramsToDrop).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
    }
    if (functionIsUnit)
      s"$name()"
    else if (args.nonEmpty)
      s"$name(${args.mkString(", ")})"
    else
      name
  }

  /**
    * Helper function for deciding if a snippet can be generated.
    * Returns false if there are too few arguments.
    */
  def canApplySnippet(fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): Boolean = {
    val functionIsUnit = isUnitFunction(fparams)

    if (paramsToDrop > fparams.length || (functionIsUnit && paramsToDrop > 0)) false else true
  }

  /**
    * Calculates how many params to drops in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    */
  private def paramsToDrop(implicit context: CompletionContext): Int = {
    context.previousWord match {
      case "||>" => 2
      case "|>" | "!>" => 1
      case _ => 0
    }
  }

  /**
    * Under some circumstances, even though we set `isIncomplete`, which is supposed to opt-out
    * of this behaviour, VSCode filters returned completions when the user types more text
    * without calling the language server again (so it has no chance to return different
    * completions).
    *
    * If we use `label` as filter text (which is the default), this can result in many false
    * positives, e.g. if the user types "MyList[t", the "t" will result in many potential Def
    * and Sig completions. If the user then types "]" VSCode will filter this list using the
    * "word" "t]" which will match many of these completions (e.g. "Nec.tail(c: Nec[a]): ...").
    *
    * To avoid this behaviour, we set `filterText` for Def and Sig completions to be just the
    * name. The "(" is there so that they still see completions if they enter the opening
    * bracket of a function call (but not if they start filling in the argument list).
    */
  def getFilterTextForName(name: String): String = {
    s"$name("
  }

  def getNestedModules(word: String)(implicit root: TypedAst.Root): List[Symbol.ModuleSym] = {
    ModuleSymFragment.parseModuleSym(word) match {
      case ModuleSymFragment.Complete(modSym) =>
        root.modules.getOrElse(modSym, Nil).collect {
          case sym: Symbol.ModuleSym => sym
        }
      case ModuleSymFragment.Partial(modSym, suffix) =>
        root.modules.getOrElse(modSym, Nil).collect {
          case sym: Symbol.ModuleSym if matches(sym, suffix) => sym
        }
      case _ => Nil
    }
  }

  /**
   * Returns `true` if the given module `sym` matches the given `suffix`.
   *
   * (Aaa.Bbb.Ccc, Cc) => true
   * (Aaa.Bbb.Ccc, Dd) => false
   * (/, Cc)           => true
   */
  private def matches(sym: Symbol.ModuleSym, suffix: String): Boolean = {
    if (sym.isRoot) {
      true
    } else {
      sym.ns.last.startsWith(suffix) // We know that ns cannot be empty because it is not the root.
    }
  }

  /**
    * Filters the definitions in the given `root` by the given `word` and `env`.
    * If `whetherInScope` is `true`, we return the matched defs in the root module or in the scope
    * If `whetherInScope` is `false`, we return the matched defs not in the root module and not in the scope
    */
  def filterDefsByScope(word: String, root: TypedAst.Root, env: LocalScope, whetherInScope: Boolean): Iterable[TypedAst.Def] = {
    val matchedDefs = root.defs.filter{case (_, decl) => matchesDef(decl, word)}
    val rootModuleMatches = matchedDefs.collect{
        case (sym, decl) if whetherInScope && sym.namespace.isEmpty => decl
    }
    val scopeMatches = matchedDefs.collect{
      case (sym, decl) if sym.namespace.nonEmpty && checkScope(decl, env, whetherInScope) => decl
    }
    rootModuleMatches ++ scopeMatches
  }

  /**
    * When `whetherInScope` is `true`, we check if the given definition `decl` is in the scope.
    * When `whetherInScope` is `false`, we check if the given definition `decl` is not in the scope.
    */
  private def checkScope(decl: TypedAst.Def, scope: LocalScope, whetherInScope: Boolean): Boolean = {
    val thisName = decl.sym.toString
    val inScope = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Def(thatName, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    if (whetherInScope) inScope else !inScope
  }

  /**
    * Returns `true` if the given definition `decl` should be included in the suggestions.
    */
  private def matchesDef(decl: TypedAst.Def, word: String): Boolean = {
    def isInternal(decl: TypedAst.Def): Boolean = decl.spec.ann.isInternal

    val isPublic = decl.spec.mod.isPublic && !isInternal(decl)
    val isMatch = fuzzyMatch(word, decl.sym.text)

    isMatch && isPublic
  }

  /**
    * Checks if we should offer AutoUseCompletion or AutoImportCompletion.
    * Currently, we will only offer them if at least three characters have been typed.
    */
  def shouldComplete(word: String): Boolean = word.length >= 3

  /**
    * Returns `true` if the query is a fuzzy match for the key.
    * After splitting query and key by camel case, every query segment must be a prefix of some key segment in order.
    * Works for camelCase and UpperCamelCase.
    *
    * Example:
    *   - fuzzyMatch("fBT",  "fooBarTest") = true
    *   - fuzzyMatch("fBrT", "fooBarTest") = false
    *   - fuzzyMatch("fTB",  "fooBarTest") = false
    *
    * @param query  The query string, usually from the user input.
    * @param key    The key string, usually from the completion item.
    */
  def fuzzyMatch(query: String, key: String): Boolean = {
    @tailrec
    def matchSegments(query: List[String], key: List[String]): Boolean = (query, key) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (qHead :: qTail, kHead :: kTail) =>
        if (kHead.startsWith(qHead))
          matchSegments(qTail, kTail)
        else
          matchSegments(query, kTail)
    }
    matchSegments(splitByCamelCase(query), splitByCamelCase(key))
  }

  /**
    * Splits a string by camel case.
    *
    * Example: "fooBarTest" -> List("foo", "Bar", "Test")
    */
  private def splitByCamelCase(input: String): List[String] = {
    input.split("(?=[A-Z])").toList
  }
}
