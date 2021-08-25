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
import ca.uwaterloo.flix.language.ast.{Ast, MinLib, Name, ResolvedAst, SemanticOperator, SourceLocation, Symbol, UnkindedType}
import ca.uwaterloo.flix.util.Validation

// MATT docs
object Deriver extends Phase[ResolvedAst.Root, ResolvedAst.Root] {

  override def run(input: ResolvedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Root, CompilationError] = {

  }

  def createToString(enum: ResolvedAst.Enum)(implicit flix: Flix): ResolvedAst.Instance = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams, derives, cases, tpeDeprecated, sc, loc) =>
      val matchRules = cases.values.map(createToStringMatchRule(_, sym))
      val varSym = Symbol.freshVarSym()
      val exp = ResolvedAst.Expression.Match(
        ResolvedAst.Expression.Var(varSym, varSym.tvar, SourceLocation.Unknown),
        matchRules.toList,
        SourceLocation.Unknown
      )
      val spec = ResolvedAst.Spec(
        doc = Ast.Doc(Nil, SourceLocation.Unknown),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = ???, // MATT get these from the type of the enum I guess
        fparams = List(ResolvedAst.FormalParam(varSym, Ast.Modifiers.Empty, ???, SourceLocation.Unknown)), // MATT use scheme I guess
        sc = ???, // MATT use sc again
        tpe = ???, // MATT sc again; is this return tpe?
        eff = UnkindedType.Cst(UnkindedType.Constructor.Pure, SourceLocation.Unknown),
        loc = SourceLocation.Unknown
      )
      val defn = ResolvedAst.Def(Symbol.mkDefnSym("ToString.toString"), spec, exp)

      ResolvedAst.Instance(
        doc = Ast.Doc(Nil, SourceLocation.Unknown),
        mod = Ast.Modifiers.Empty,
        sym = MinLib.ToString.sym,
        tpe = ???, // MATT get from sc,
        tconstrs = ???, // MATT should be all tparams
        defs = List(defn),
        ns = Name.RootNS,
        loc = SourceLocation.Unknown
      )
  }

  def createToStringMatchRule(caze: ResolvedAst.Case, enumSym: Symbol.EnumSym)(implicit flix: Flix): ResolvedAst.MatchRule = caze match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated, sc) =>
      val varSym = Symbol.freshVarSym()
      val varPat = ResolvedAst.Pattern.Var(varSym, SourceLocation.Unknown)
      val pat = ResolvedAst.Pattern.Tag(enumSym, tag, varPat, SourceLocation.Unknown)

      val guard = ResolvedAst.Expression.True(SourceLocation.Unknown)

      val tagPart = ResolvedAst.Expression.Str(tag.name + "(", SourceLocation.Unknown)
      val valuePart = ResolvedAst.Expression.Apply(
        ResolvedAst.Expression.Sig(MinLib.ToString.ToString.sym, SourceLocation.Unknown),
        List(ResolvedAst.Expression.Var(varSym, varSym.tvar, SourceLocation.Unknown)),
        SourceLocation.Unknown
      )
      val endPart = ResolvedAst.Expression.Str(")", SourceLocation.Unknown)
      val exp = concat(concat(tagPart, valuePart), endPart)

      ResolvedAst.MatchRule(pat, guard, exp)
  }

  def concat(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression): ResolvedAst.Expression = {
    ResolvedAst.Expression.Binary(SemanticOperator.StringOp.Concat, exp1, exp2, SourceLocation.Unknown)
  }
}
