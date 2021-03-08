/*
 * Copyright 2021 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{Constraint, Def, Expression, Pattern, Predicate, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{ParOps, Validation}

object Lowering extends Phase[Root, Root] {

  // TODO: Add enum symbols for every declared enum type.
  val PolaritySym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
  val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyTerm")

  /**
    * Translates internal Datalog constraints into Flix Datalog constraints.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Lowering") {
    val defs = ParOps.parMap(root.defs.values, visitDef)

    // TODO: Visit expressions in other parts of the AST (e.g. in classes and instances.)

    root.copy(defs = defs.map(kv => kv.sym -> kv).toMap).toSuccess
  }

  private def visitDef(defn: Def): Def = {
    val e = visitExp(defn.exp)
    defn.copy(exp = e)
  }

  private def visitExp(exp0: Expression): Expression = exp0 match {
    case Expression.Unit(loc) => ???

    case Expression.Null(tpe, loc) => ???

    case Expression.True(loc) => ???

    case Expression.False(loc) => ???

    case Expression.Char(lit, loc) => ???

    case Expression.Float32(lit, loc) => ???

    case Expression.Float64(lit, loc) => ???

    case Expression.Int8(lit, loc) => ???

    case Expression.Int16(lit, loc) => ???

    case Expression.Int32(lit, loc) => ???

    case Expression.Int64(lit, loc) => ???

    case Expression.BigInt(lit, loc) => ???

    case Expression.Str(lit, loc) => ???

    case Expression.Default(tpe, loc) => ???

    case Expression.Wild(tpe, loc) => ???

    case Expression.Var(sym, tpe, loc) => ???

    case Expression.Def(sym, tpe, loc) => ???

    case Expression.Sig(sym, tpe, loc) => ???

    case Expression.Hole(sym, tpe, eff, loc) => ???

    case Expression.Lambda(fparam, exp, tpe, loc) => ???

    case Expression.Apply(exp, exps, tpe, eff, loc) => ???

    case Expression.Unary(sop, exp, tpe, eff, loc) => ???

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) => ???

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => ???

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ???

    case Expression.Stm(exp1, exp2, tpe, eff, loc) => ???

    case Expression.Match(exp, rules, tpe, eff, loc) => ???

    case Expression.Choose(exps, rules, tpe, eff, loc) => ???

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ???

    case Expression.Tuple(elms, tpe, eff, loc) => ???

    case Expression.RecordEmpty(tpe, loc) => ???

    case Expression.RecordSelect(exp, field, tpe, eff, loc) => ???

    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => ???

    case Expression.RecordRestrict(field, rest, tpe, eff, loc) => ???

    case Expression.ArrayLit(elms, tpe, eff, loc) => ???

    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ???

    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ???

    case Expression.ArrayLength(base, eff, loc) => ???

    case Expression.ArrayStore(base, index, elm, loc) => ???

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???

    case Expression.Ref(exp, tpe, eff, loc) => ???

    case Expression.Deref(exp, tpe, eff, loc) => ???

    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ???

    case Expression.Existential(fparam, exp, loc) => ???

    case Expression.Universal(fparam, exp, loc) => ???

    case Expression.Ascribe(exp, tpe, eff, loc) => ???

    case Expression.Cast(exp, tpe, eff, loc) => ???

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ???

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => ???

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => ???

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => ???

    case Expression.GetField(field, exp, tpe, eff, loc) => ???

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => ???

    case Expression.GetStaticField(field, tpe, eff, loc) => ???

    case Expression.PutStaticField(field, exp, tpe, eff, loc) => ???

    case Expression.NewChannel(exp, tpe, eff, loc) => ???

    case Expression.GetChannel(exp, tpe, eff, loc) => ???

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ???

    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ???

    case Expression.Spawn(exp, tpe, eff, loc) => ???

    case Expression.Lazy(exp, tpe, loc) => ???

    case Expression.Force(exp, tpe, eff, loc) => ???

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
      // TODO: Call into solver
      ???

    case Expression.FixpointCompose(exp1, exp2, stf, tpe, eff, loc) =>
      // TODO: Call into solver
      ???

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      // TODO: Call into solver
      ???

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
      // TODO: Call into solver
      ???

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
      // TODO: Call into solver
      ???

    case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) => ???
  }

  private def visitType(t: Type): Type = ???

  private def visitConstraint(c: Constraint): Expression = ???

  private def visitHeadPred(p: Predicate.Head): Expression = p match {
    case Head.Atom(pred, den, terms, tpe, loc) =>
      ???

    case Head.Union(exp, tpe, loc) => ???
  }

  private def visitBodyPred(p: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val p = visitPolarity(polarity, loc)
      val ts = terms.map(visitBodyTerm)

      ???
    case Body.Guard(exp, loc) =>

      ???
  }

  private def visitHeadTerm(e: Expression): Expression = ???

  private def visitBodyTerm(p: Pattern): Expression = p match {
    case Pattern.Wild(tpe, loc) => ??? // TODO: Support ?
    case Pattern.Var(sym, tpe, loc) => ??? // TODO: Translate to BodyTerm.Var
    case Pattern.Unit(loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.True(loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.False(loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Char(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Float32(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Float64(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int8(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int16(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int32(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int64(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.BigInt(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Str(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Tag(sym, tag, pat, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Tuple(elms, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Array(elms, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.ArrayTailSpread(elms, sym, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
  }

  private def visitPolarity(p: Ast.Polarity, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = p match {
    case Polarity.Positive =>
      val (_, tpe) = Scheme.instantiate(root.enums(PolaritySym).sc, InstantiateMode.Flexible)
      mkUnitTag(PolaritySym, "Positive", tpe, loc)

    case Polarity.Negative =>
      val (_, tpe) = Scheme.instantiate(root.enums(PolaritySym).sc, InstantiateMode.Flexible)
      mkUnitTag(PolaritySym, "Negative", tpe, loc)

  }

  private def visitSourceLocation(loc: SourceLocation): Expression = ???

  private def mkUnitTag(sym: Symbol.EnumSym, tag: String, tpe: Type, loc: SourceLocation): Expression = {
    val innerExp = Expression.Unit(loc)
    Expression.Tag(sym, Name.Tag(tag, loc), innerExp, tpe, Type.Pure, loc)
  }

}
