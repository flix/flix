/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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

import ca.uwaterloo.flix.api.lsp.{CompletionItemKind, Range}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.fuzzyMatch
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object UseCompleter {
  /**
    * Returns a List of Completions for use clause.
    */
  def getCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val namespace = qn.namespace.idents.map(_.name)
    val ident = qn.ident.name
    val moduleSym = Symbol.mkModuleSym(namespace)
    root.modules.get(moduleSym).collect {
      case mod: Symbol.ModuleSym if fuzzyMatch(ident, mod.ns.last) => UseCompletion(mod.toString, range, Priority.Medium(0), CompletionItemKind.Module)
      case enm: Symbol.EnumSym if fuzzyMatch(ident, enm.name) && CompletionUtils.isAvailable(enm) => UseCompletion(enm.toString, range, Priority.Medium(0), CompletionItemKind.Enum)
      case eff: Symbol.EffectSym if fuzzyMatch(ident, eff.name) && CompletionUtils.isAvailable(eff) => UseCompletion(eff.toString, range, Priority.Medium(0), CompletionItemKind.Event)
      case defn: Symbol.DefnSym if fuzzyMatch(ident, defn.name) && CompletionUtils.isAvailable(defn) => UseCompletion(defn.toString, range, Priority.Medium(0), CompletionItemKind.Function)
      case trt: Symbol.TraitSym if fuzzyMatch(ident, trt.name) && CompletionUtils.isAvailable(trt) => UseCompletion(trt.toString, range, Priority.Medium(0), CompletionItemKind.Interface)
    } ++ getSigCompletions(qn, range) ++ getOpCompletions(qn, range) ++ getTagCompletions(qn, range)
  }

  /**
    * Returns a List of Completion for signatures.
    */
  private def getSigCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val traitSym = Symbol.mkTraitSym(qn.namespace.toString)
    root.traits.get(traitSym).filter(CompletionUtils.isAvailable).map(_.sigs.collect {
      case sig if fuzzyMatch(qn.ident.name, sig.sym.name) && sig.spec.mod.isPublic =>
        UseCompletion(sig.sym.toString, range, Priority.Medium(0), CompletionItemKind.Method)
    }).getOrElse(Nil)
  }

  /**
    * Returns a List of Completion for ops.
    */
  private def getOpCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val effectSym = Symbol.mkEffectSym(qn.namespace.toString)
    root.effects.get(effectSym).filter(CompletionUtils.isAvailable).map(_.ops.collect {
      case op if fuzzyMatch(qn.ident.name, op.sym.name) =>
        UseCompletion(op.sym.toString, range, Priority.Medium(0), CompletionItemKind.Method)
    }).getOrElse(Nil)
  }

  /**
    * Returns a List of Completion for tags.
    */
  private def getTagCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val enumSym = Symbol.mkEnumSym(qn.namespace.toString)
    root.enums.get(enumSym).filter(CompletionUtils.isAvailable).map(_.cases.values.collect {
      case tag if fuzzyMatch(qn.ident.name, tag.sym.name) =>
        UseCompletion(tag.sym.toString, range, Priority.Medium(0), CompletionItemKind.EnumMember)
    }).getOrElse(Nil)
  }
}
