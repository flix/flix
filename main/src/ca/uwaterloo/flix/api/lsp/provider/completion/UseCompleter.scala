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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.{ModCompletion, UseDefCompletion, UseEffCompletion, UseEnumCompletion, UseEnumTagCompletion, UseOpCompletion, UseSignatureCompletion, UseTrtCompletion}
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.fuzzyMatch
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object UseCompleter {
  /**
    * Returns a List of Completions for use clause.
    */
  def getCompletions(uri: String, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] ={
    val moduleSym = Symbol.mkModuleSym(namespace)
    root.modules.get(moduleSym).collect{
      case mod:  Symbol.ModuleSym if fuzzyMatch(ident, mod.ns.last) => ModCompletion(mod)
      case enum: Symbol.EnumSym   if fuzzyMatch(ident, enum.name)   => UseEnumCompletion(enum.toString)
      case eff:  Symbol.EffectSym if fuzzyMatch(ident, eff.name)    => UseEffCompletion(eff.toString)
      case defn: Symbol.DefnSym   if fuzzyMatch(ident, defn.name)   => UseDefCompletion(defn.toString)
      case trt:  Symbol.TraitSym  if fuzzyMatch(ident, trt.name)    => UseTrtCompletion(trt.toString)
    } ++ getSigCompletions(uri, namespace, ident) ++ getOpCompletions(namespace, ident) ++ getTagCompletions(namespace, ident)
  }

  /**
    * Returns a List of Completion for signatures.
    */
  private def getSigCompletions(uri: String, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val traitSym = Symbol.mkTraitSym(namespace.mkString("."))
    root.traits.get(traitSym).map(_.sigs.collect {
      case sig if fuzzyMatch(ident, sig.sym.name) && (sig.spec.mod.isPublic || sig.sym.loc.source.name == uri) =>
        UseSignatureCompletion(sig.sym.toString)
    }).getOrElse(Nil)
  }

  /**
    * Returns a List of Completion for ops.
    */
  private def getOpCompletions(namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val effectSym = Symbol.mkEffectSym(namespace.mkString("."))
    root.effects.get(effectSym).map(_.ops.collect {
      case op if fuzzyMatch(ident, op.sym.name) => UseOpCompletion(op.sym.toString)
    }).getOrElse(Nil)
  }

  /**
    * Returns a List of Completion for tags.
    */
  private def getTagCompletions(namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val enumSym = Symbol.mkEnumSym(namespace.mkString("."))
    root.enums.get(enumSym).map(_.cases.values.collect {
      case tag if fuzzyMatch(ident, tag.sym.name) => UseEnumTagCompletion(tag.sym.toString)
    }).getOrElse(Nil)
  }
}
