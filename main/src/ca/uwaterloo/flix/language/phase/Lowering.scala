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
import ca.uwaterloo.flix.language.ast.TypedAst.{Constraint, Def, Expression, FormalParam, Pattern, Predicate, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object Lowering extends Phase[Root, Root] {

  val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyPredicate")
  val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyTerm")
  val ConstraintSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Constraint")
  val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.HeadPredicate")
  val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.HeadTerm")
  val PolaritySym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
  val PredSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.PredSym")
  val SourceLocationSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.SourceLocation")
  val VarSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.VarSym")

  val UnsafeBox: Symbol.EnumSym = Symbol.mkEnumSym("UnsafeBox")

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
    case Expression.Unit(_) => exp0

    case Expression.Null(_, _) => exp0

    case Expression.True(_) => exp0

    case Expression.False(_) => exp0

    case Expression.Char(_, _) => exp0

    case Expression.Float32(_, _) => exp0

    case Expression.Float64(_, _) => exp0

    case Expression.Int8(_, _) => exp0

    case Expression.Int16(_, _) => exp0

    case Expression.Int32(_, _) => exp0

    case Expression.Int64(_, _) => exp0

    case Expression.BigInt(_, _) => exp0

    case Expression.Str(_, _) => exp0

    case Expression.Default(_, _) => exp0

    case Expression.Wild(_, _) => exp0

    case Expression.Var(_, _, _) => exp0

    case Expression.Def(_, _, _) => exp0

    case Expression.Sig(_, _, _) => exp0

    case Expression.Hole(_, _, _, _) => exp0

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Lambda(p, e, t, loc)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      val t = visitType(tpe)
      Expression.Apply(e, es, t, eff, loc)

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Unary(sop, e, t, eff, loc)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Binary(sop, e1, e2, t, eff, loc)

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Let(sym, e1, e2, t, eff, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      Expression.IfThenElse(e1, e2, e3, t, eff, loc)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Stm(e1, e2, t, eff, loc)

    case Expression.Match(exp, rules, tpe, eff, loc) => ???

    case Expression.Choose(exps, rules, tpe, eff, loc) => ???

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Tag(sym, tag, e, t, eff, loc)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      val es = visitExps(elms)
      val t = visitType(tpe)
      Expression.Tuple(es, t, eff, loc)

    case Expression.RecordEmpty(tpe, loc) =>
      val t = visitType(tpe)
      Expression.RecordEmpty(t, loc)

    case Expression.RecordSelect(exp, field, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.RecordSelect(e, field, t, eff, loc)

    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val t = visitType(tpe)
      Expression.RecordExtend(field, v, r, t, eff, loc)

    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      val r = visitExp(rest)
      val t = visitType(tpe)
      Expression.RecordRestrict(field, r, t, eff, loc)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      val es = visitExps(elms)
      val t = visitType(tpe)
      Expression.ArrayLit(es, t, eff, loc)

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      val e = visitExp(elm)
      val l = visitExp(len)
      val t = visitType(tpe)
      Expression.ArrayNew(e, l, t, eff, loc)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      Expression.ArrayLoad(b, i, t, eff, loc)

    case Expression.ArrayLength(base, eff, loc) =>
      val b = visitExp(base)
      Expression.ArrayLength(b, eff, loc)

    case Expression.ArrayStore(base, index, elm, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val e = visitExp(elm)
      Expression.ArrayStore(b, i, e, loc)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = visitExp(base)
      val bi = visitExp(beginIndex)
      val ei = visitExp(endIndex)
      val t = visitType(tpe)
      Expression.ArraySlice(b, bi, ei, t, loc)

    case Expression.Ref(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Ref(e, t, eff, loc)

    case Expression.Deref(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Deref(e, t, eff, loc)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Assign(e1, e2, t, eff, loc)

    case Expression.Existential(fparam, exp, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      Expression.Existential(p, e, loc)

    case Expression.Universal(fparam, exp, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      Expression.Universal(p, e, loc)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Ascribe(e, t, eff, loc)

    case Expression.Cast(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Cast(e, t, eff, loc)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ???

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => ???

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => ???

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => ???

    case Expression.GetField(field, exp, tpe, eff, loc) => ???

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => ???

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe)
      Expression.GetStaticField(field, t, eff, loc)

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.PutStaticField(field, e, t, eff, loc)

    case Expression.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.NewChannel(e, t, eff, loc)

    case Expression.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.GetChannel(e, t, eff, loc)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.PutChannel(e1, e2, t, eff, loc)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ???

    case Expression.Spawn(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Spawn(e, t, eff, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Lazy(e, t, loc)

    case Expression.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Force(e, t, eff, loc)

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
      // TODO: Use stratification here or compute in solver?

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

    case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) =>
      // TODO: Call into solver
      ???
  }

  private def visitExps(exps: List[Expression]): List[Expression] = exps.map(visitExp)

  private def visitType(t: Type): Type = ???

  private def visitFormalParam(fparam: FormalParam): FormalParam = ???

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
    case Pattern.Wild(tpe, loc) => ??? // TODO: Translate to ???

    case Pattern.Var(sym, tpe, loc) => ??? // TODO: Translate to BodyTerm.Var

    case Pattern.Unit(loc) => mkUnsafeBox(Expression.Unit(loc))

    case Pattern.True(loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.False(loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Char(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Float32(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Float64(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Int8(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Int16(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Int32(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.Int64(lit, loc) => ??? // TODO: Box to Object and then UnsafeBox.

    case Pattern.BigInt(lit, loc) => mkUnsafeBox(Expression.BigInt(lit, loc))

    case Pattern.Str(lit, loc) => mkUnsafeBox(Expression.Str(lit, loc))

    case Pattern.Tag(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$p'.") // TODO: Support tags???

    case Pattern.Tuple(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$p'.")

    case Pattern.Array(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$p'.")

    case Pattern.ArrayTailSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$p'.")

    case Pattern.ArrayHeadSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$p'.")
  }

  /**
    * Translates a [[Polarity]] AST node to an expression.
    */
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

  private def mkUnsafeBox(exp: Expression): Expression = ??? // TODO

}
