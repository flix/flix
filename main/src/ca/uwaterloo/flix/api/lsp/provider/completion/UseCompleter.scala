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
import ca.uwaterloo.flix.language.ast.Name.QName
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.{ResolutionError, WeederError}

object UseCompleter {
  /**
    * Returns a List of Completions from UndefinedUse.
    */
  def getCompletions(err: ResolutionError.UndefinedUse, uri: String)(implicit root: TypedAst.Root): Iterable[Completion] =
    getCompletions(err.qn, uri)

  /**
    * Returns a List of Completion from UnqualifiedUse.
    */
  def getCompletions(err: WeederError.UnqualifiedUse, uri: String)(implicit root: TypedAst.Root): Iterable[Completion] =
    getCompletions(err.qn, uri)

  /**
    * Returns a List of Completion from qualified name and uri.
    */
  private def getCompletions(qn: QName, uri: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val namespace = qn.namespace.idents.map(_.name) ++ {
      if (followedByDot(qn.loc)) List(qn.ident.name) else Nil
    }
    val ident = if(followedByDot(qn.loc)) "" else qn.ident.name
    val moduleSym = Symbol.mkModuleSym(namespace)
    root.modules.getOrElse(moduleSym, Nil).collect{
      case mod:  Symbol.ModuleSym if fuzzyMatch(ident, mod.ns.last) => ModCompletion(mod)
      case enum: Symbol.EnumSym   if fuzzyMatch(ident, enum.name)   => UseEnumCompletion(enum.toString)
      case eff:  Symbol.EffectSym if fuzzyMatch(ident, eff.name)    => UseEffCompletion(eff.toString)
      case defn: Symbol.DefnSym   if fuzzyMatch(ident, defn.name)   => UseDefCompletion(defn.toString)
      case trt:  Symbol.TraitSym  if fuzzyMatch(ident, trt.name)    => UseTrtCompletion(trt.toString)
    } ++ getSigCompletions(qn, uri) ++ getOpCompletions(qn) ++ getTagCompletions(qn)
  }

  /**
    * Returns true if the character immediately following the location is a dot.
    * Note:
    *   - loc.endCol will point to the next character after QName.
    *   - loc is 1-indexed, so we are actually checking the character at loc.endCol-1.
    */
  private def followedByDot(loc: SourceLocation): Boolean = {
    val line = loc.lineAt(loc.endLine)
    loc.endCol <= line.length && line.charAt(loc.endCol-1) == '.'
  }

  /**
    * Returns a List of Completion for signatures.
    */
  private def getSigCompletions(name: QName, uri: String)(implicit root: TypedAst.Root): Iterable[Completion] =
    root.sigs.values.collect{
      case sig if fuzzyMatch(name.ident.name, sig.sym.name) && (sig.spec.mod.isPublic || sig.sym.loc.source.name == uri) =>
        UseSignatureCompletion(sig.sym.toString)
    }

  /**
    * Returns a List of Completion for ops.
    */
  private def getOpCompletions(name: QName)(implicit root: TypedAst.Root): Iterable[Completion] =
    root.effects.values.flatMap(_.ops).collect{
      case op if fuzzyMatch(name.ident.name, op.sym.name) =>
        UseOpCompletion(op.sym.toString)
    }

  /**
    * Returns a List of Completion for tags.
    */
  private def getTagCompletions(qn: QName)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val enums = mkEnumSymNoTag(qn) ++ mkEnumSymWithTag(qn)
    enums.flatMap(enum => enum.cases.collect{
      case (_, c) => UseEnumTagCompletion(enum.sym, c)
    })
  }

  /**
    * Returns a List of Enum from qualified name, assuming no trailing tag.
    *
    * Example:
    *   - qn = "Foo.Bar.Baz" => EnumSym([Foo, Bar], Baz, Unknown)
    *   - qn = "Foo" => EnumSym([], Foo, Unknown)
    */
  private def mkEnumSymNoTag(qn: QName)(implicit root: TypedAst.Root): List[TypedAst.Enum] = {
    val enumSym = new Symbol.EnumSym(qn.namespace.idents.map(_.name), qn.ident.name, SourceLocation.Unknown)
    root.enums.get(enumSym).toList
  }

  /**
    * Returns a List of Enum from qualified name, assuming a trailing tag.
    * We will assume qn.ident is the tag, and create an EnumSym from only qn.namespace. Thus, qn.namespace should not be empty.
    *
    * Example:
    *   - qn = "Foo.Bar.Ba" => EnumSym([Foo], Bar, Unknown)
    *   - qn = "Foo.B" => EnumSym([], Foo, Unknown)
    */
  private def mkEnumSymWithTag(qn: QName)(implicit root: TypedAst.Root): List[TypedAst.Enum] = {
    if (qn.namespace.idents.isEmpty)
      return Nil
    val enumSym = new Symbol.EnumSym(qn.namespace.idents.dropRight(1).map(_.name), qn.namespace.idents.last.name, SourceLocation.Unknown)
    root.enums.get(enumSym).toList
  }
}
