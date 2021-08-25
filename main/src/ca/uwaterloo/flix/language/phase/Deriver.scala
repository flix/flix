/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{ResolvedAst, SemanticOperator, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.Validation

// MATT docs
object Deriver extends Phase[ResolvedAst.Root, ResolvedAst.Root] {

  override def run(input: ResolvedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Root, CompilationError] = {

  }

  def createToString(enum: ResolvedAst.Enum): ResolvedAst.Instance = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams, derives, cases, tpeDeprecated, sc, loc)

  }

  def createToString(enumSym: Symbol.EnumSym, caze: ResolvedAst.Case)(implicit flix: Flix): ResolvedAst.MatchRule = caze match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated, sc) =>
      val varPat = ResolvedAst.Pattern.Var(Symbol.freshVarSym(), SourceLocation.Unknown)
      val pat = ResolvedAst.Pattern.Tag(enumSym, tag, ResolvedAst.Pattern.Var(Symbol.freshVarSym(), SourceLocation.Unknown), SourceLocation.Unknown)

      val guard = ResolvedAst.Expression.True(SourceLocation.Unknown)

      val tagPart = ResolvedAst.Expression.Str(tag.name + "(", SourceLocation.Unknown)
      val valuePart = ResolvedAst.Expression.Apply(ResolvedAst.Expression.Sig())
      val exp = concat(ResolvedAst.Expression.Str())
  }

  def concat(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression): ResolvedAst.Expression = {
    ResolvedAst.Expression.Binary(SemanticOperator.StringOp.Concat, exp1, exp2, SourceLocation.Unknown)
  }
}
