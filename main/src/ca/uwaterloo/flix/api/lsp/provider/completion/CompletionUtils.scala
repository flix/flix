/*
 * Copyright 2022 Paul Butcher, Lukas Rønn, Magnus Madsen
 * Copyright 2025 Chenhao Gao
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
import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Def
import ca.uwaterloo.flix.language.ast.TypedAst.Decl
import ca.uwaterloo.flix.language.ast.shared.{LocalScope, QualifiedSym, Resolution}
import ca.uwaterloo.flix.language.fmt.FormatType

import scala.annotation.tailrec

object CompletionUtils {

  private def isUnitType(tpe: Type): Boolean = tpe == Type.Unit

  private def isUnitFunction(fparams: List[TypedAst.FormalParam]): Boolean = fparams.length == 1 && isUnitType(fparams.head.tpe)

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
    * Generate a snippet which represents defining an effect operation handler, with an extra `resume` as the last argument.
    */
  def getOpHandlerSnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): String = {
    val functionIsUnit = isUnitFunction(fparams)

    val args = fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
    } :+ s"$${${fparams.length + 1}:resume}"
    if (functionIsUnit)
      s"$name($${1:resume}) = "
    else
      s"$name(${args.mkString(", ")}) = "
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
    * @param query The query string, usually from the user input.
    * @param key   The key string, usually from the completion item.
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

  /**
    * Checks if the namespace and ident from the error matches the qualified name.
    * We require a full match on the namespace and a fuzzy match on the ident.
    *
    * Example:
    * matchesQualifiedName(["A", "B"], "fooBar", ["A", "B"], "fB") => true
    */
  def matchesQualifiedName(targetNamespace: List[String], targetIdent: String, qn: Name.QName): Boolean = {
    targetNamespace == qn.namespace.parts && fuzzyMatch(qn.ident.name, targetIdent)
  }

  /**
    * Format type params in the right form to be displayed in the list of completions
    * e.g. "[a, b, c]"
    */
  def formatTParams(tparams: List[TypedAst.TypeParam]): String = {
    tparams match {
      case Nil => ""
      case _ => tparams.map(_.name).mkString("[", ", ", "]")
    }
  }

  /**
    * Format type params in the right form to be inserted as a snippet
    * e.g. "[${1:a}, ${2:b}, ${3:c}]"
    */
  def formatTParamsSnippet(tparams: List[TypedAst.TypeParam]): String = {
    tparams match {
      case Nil => ""
      case _ => tparams.zipWithIndex.map {
        case (tparam, idx) => "$" + s"{${idx + 1}:${tparam.name}}"
      }.mkString("[", ", ", "]")
    }
  }


  /**
    * Checks if the given class is public.
    */
  def isAvailable(decl: Decl): Boolean =
    !decl.ann.isInternal && decl.mod.isPublic

  /**
    * Checks if the given def is public.
    */
  def isAvailable(defn: TypedAst.Def): Boolean = isAvailable(defn.spec)

  /**
    * Checks if the given sig is public.
    */
  def isAvailable(sig: TypedAst.Sig): Boolean = isAvailable(sig.spec)

  /**
    * Checks if the given op is public.
    */
  def isAvailable(op: TypedAst.Op): Boolean = isAvailable(op.spec)

  /**
    * Checks if the def of the given symbol is public.
    */
  def isAvailable(defn: Symbol.DefnSym)(implicit root: TypedAst.Root): Boolean = root.defs.get(defn).exists(isAvailable)

  /**
    * Checks if the trait of the given symbol is public.
    */
  def isAvailable(trt: Symbol.TraitSym)(implicit root: TypedAst.Root): Boolean = root.traits.get(trt).exists(isAvailable)

  /**
    * Checks if the effect of the given symbol is public.
    */
  def isAvailable(eff: Symbol.EffectSym)(implicit root: TypedAst.Root): Boolean = root.effects.get(eff).exists(isAvailable)

  /**
    * Checks if the enum of the given symbol is public.
    */
  def isAvailable(enumMap: Symbol.EnumSym)(implicit root: TypedAst.Root): Boolean = root.enums.get(enumMap).exists(isAvailable)

  /**
    * Checks if the sym and the given qualified name matches.
    *
    * @param sym        The symbol to check, usually from root.
    * @param qn         The qualified name to check, usually from user input.
    * @param qualified  Whether the qualified name is qualified.
    */
  private def fmtType(tpe: Type, holes: Map[Symbol, String])(implicit flix: Flix): String = {
    val replaced = holes.foldLeft(tpe) { case (t, (sym, hole)) => replaceText(sym, t, hole) }
    FormatType.formatType(replaced)
  }

  /**
    * Formats the given associated type `assoc`.
    */
  private def fmtAssocType(assoc: TypedAst.AssocTypeSig, holes: Map[Symbol, String]): String = {
    s"    type ${assoc.sym.name} = ${holes(assoc.sym)}"
  }

  /**
    * Formats the given formal parameters in `spec`.
    */
  private def fmtFormalParams(spec: TypedAst.Spec, holes: Map[Symbol, String])(implicit flix: Flix): String =
    spec.fparams.map(fparam => s"${fparam.bnd.sym.text}: ${fmtType(fparam.tpe, holes)}").mkString(", ")

  /**
    * Formats the given signature `sig`.
    */
  private def fmtSignature(sig: TypedAst.Sig, holes: Map[Symbol, String])(implicit flix: Flix): String = {
    val fparams = fmtFormalParams(sig.spec, holes)
    val retTpe = fmtType(sig.spec.retTpe, holes)
    val eff = sig.spec.eff match {
      case Type.Cst(TypeConstructor.Pure, _) => ""
      case e => raw" \ " + fmtType(e, holes)
    }
    s"    pub def ${sig.sym.name}($fparams): $retTpe$eff = ???"
  }

  def fmtInstanceSnippet(trt: TypedAst.Trait, qualified: Boolean)(implicit flix: Flix): String = {
    val instanceHole = "${1:t}"
    val holes: Map[Symbol, String] = {
      (trt.tparam.sym -> instanceHole) +:
        trt.assocs.zipWithIndex.map { case (a, i) => a.sym -> s"$$${i + 2}" }
    }.toMap

    val traitName = if (qualified) trt.sym.toString else trt.sym.name
    val signatures = trt.sigs.filter(_.exp.isEmpty)

    val body = {
      trt.assocs.map(a => fmtAssocType(a, holes)) ++
        signatures.map(s => fmtSignature(s, holes))
    }.mkString("\n\n")

    s"$traitName[$instanceHole] {\n\n$body\n\n}\n"
  }

  /**
    * Formats the given trait `trt`.
    */
  def fmtTrait(trt: TypedAst.Trait): String = {
    s"trait ${trt.sym.name}[${trt.tparam.name.name}]"
  }

  /**
    * Checks if the sym and the given qualified name matches.
    *
    * @param sym        The symbol to check, usually from root.
    * @param qn         The qualified name to check, usually from user input.
    * @param qualified  Whether the qualified name is qualified.
    */
  def matchesName(sym: QualifiedSym, qn: Name.QName, qualified: Boolean): Boolean = {
    if (qualified) {
      matchesQualifiedName(sym.namespace, sym.name, qn)
    } else
      fuzzyMatch(qn.ident.name, sym.name)
  }
}
