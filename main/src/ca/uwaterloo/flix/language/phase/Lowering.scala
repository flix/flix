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
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, ChoicePattern, ChoiceRule, Constraint, Def, Expression, FormalParam, MatchRule, Pattern, Predicate, Root, SelectChannelRule}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
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
    val defs = ParOps.parMap(root.defs.values, (d: Def) => visitDef(d)(root, flix))

    // TODO: Visit expressions in other parts of the AST (e.g. in classes and instances.)

    root.copy(defs = defs.map(kv => kv.sym -> kv).toMap).toSuccess
  }

  private def visitDef(defn: Def)(implicit root: Root, flix: Flix): Def = {
    val e = visitExp(defn.exp)
    defn.copy(exp = e)
  }

  /**
    * Lowers the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit root: Root, flix: Flix): Expression = exp0 match {
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

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe)
      Expression.Match(e, rs, t, eff, loc)

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      val es = visitExps(exps)
      val rs = rules.map(visitChoiceRule)
      val t = visitType(tpe)
      Expression.Choose(es, rs, t, eff, loc)

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

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe)
      Expression.TryCatch(e, rs, t, eff, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      val as = visitExps(args)
      val t = visitType(tpe)
      Expression.InvokeConstructor(constructor, as, t, eff, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      val e = visitExp(exp)
      val as = visitExps(args)
      val t = visitType(tpe)
      Expression.InvokeMethod(method, e, as, t, eff, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      val as = visitExps(args)
      val t = visitType(tpe)
      Expression.InvokeStaticMethod(method, as, t, eff, loc)

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.GetField(field, e, t, eff, loc)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.PutField(field, e1, e2, t, eff, loc)

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

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val d = default.map(visitExp)
      val t = visitType(tpe)
      Expression.SelectChannel(rs, d, tpe, t, loc)

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

  /**
    * Lowers the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expression])(implicit root: Root, flix: Flix): List[Expression] = exps.map(visitExp)

  private def visitPat(pat0: Pattern)(implicit root: Root, flix: Flix): Pattern = ??? // TODO (Just need to replace types)

  /**
    * Lowers the given type `tpe0`.
    */
  private def visitType(tpe0: Type)(implicit root: Root, flix: Flix): Type = tpe0 match {
    case Type.Var(_, _, _, _) => tpe0

    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.SchemaEmpty =>
        ??? // TODO: Use ConstraintType.

      case TypeConstructor.SchemaExtend(pred) =>
        ??? // TODO: Use ConstraintType.

      case _ => tpe0
    }

    case Type.Apply(tpe1, tpe2) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      Type.Apply(t1, t2)

    case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe0'.")
  }

  /**
    * Lowers the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam)(implicit root: Root, flix: Flix): FormalParam = fparam0 match {
    case FormalParam(sym, mod, tpe, loc) =>
      val t = visitType(tpe)
      FormalParam(sym, mod, t, loc)
  }

  /**
    * Lowers the given choice rule `rule0`.
    */
  private def visitChoiceRule(rule0: ChoiceRule)(implicit root: Root, flix: Flix): ChoiceRule = rule0 match {
    case ChoiceRule(pat, exp) =>
      val p = pat.map {
        case p@ChoicePattern.Wild(loc) => p
        case p@ChoicePattern.Absent(loc) => p
        case ChoicePattern.Present(sym, tpe, loc) =>
          val t = visitType(tpe)
          ChoicePattern.Present(sym, t, loc)
      }
      val e = visitExp(exp)
      ChoiceRule(p, e)
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def visitCatchRule(rule0: CatchRule)(implicit root: Root, flix: Flix): CatchRule = rule0 match {
    case CatchRule(sym, clazz, exp) =>
      val e = visitExp(exp)
      CatchRule(sym, clazz, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: MatchRule)(implicit root: Root, flix: Flix): MatchRule = rule0 match {
    case MatchRule(pat, guard, exp) =>
      val p = visitPat(pat)
      val g = visitExp(guard)
      val e = visitExp(exp)
      MatchRule(p, g, e)
  }

  /**
    * Lowers the given select channel rule `rule0`.
    */
  private def visitSelectChannelRule(rule0: SelectChannelRule)(implicit root: Root, flix: Flix): SelectChannelRule = rule0 match {
    case SelectChannelRule(sym, chan, exp) =>
      val c = visitExp(chan)
      val e = visitExp(exp)
      SelectChannelRule(sym, c, e)
  }

  private def visitConstraint(c: Constraint)(implicit root: Root, flix: Flix): Expression = c match {
    case Constraint(_, head, body, loc) =>
      val h = visitHeadPred(head)
      val bs = body.map(visitBodyPred)
      // TODO: Need a way to construct a list bs. Just need to foldRight over the Cons tag.
      ??? // TODO
  }

  private def visitHeadPred(p: Predicate.Head)(implicit root: Root, flix: Flix): Expression = p match {
    case Head.Atom(pred, den, terms, tpe, loc) =>
      ??? // TODO

    case Head.Union(exp, tpe, loc) =>
      // TODO: We need to come up with a different design for Union.
      ???
  }

  private def visitBodyPred(p: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val p = visitPolarity(polarity, loc)
      val ts = terms.map(visitBodyTerm)

      ??? // TODO
    case Body.Guard(exp, loc) =>
      ??? // TODO
  }

  /**
    * Lowers the given head term `exp0` (a subset of expressions).
    */
  private def visitHeadTerm(exp0: Expression)(implicit root: Root, flix: Flix): Expression = exp0 match {
    case Expression.Var(sym, _, _) => mkHeadVarTerm(sym)

    case Expression.Unit(loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.True(loc) => mkUnsafeBox(boxBool(Expression.True(loc)))

    case Expression.False(loc) => mkUnsafeBox(boxBool(Expression.False(loc)))

    case Expression.Char(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.Float32(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.Float64(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.Int8(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.Int16(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.Int32(lit, loc) => mkUnsafeBox(boxInt32(Expression.Int32(lit, loc)))

    case Expression.Int64(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.BigInt(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Expression.Str(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    // TODO: Wrap other expressions in a function...

    case _ => throw InternalCompilerException(s"Unexpected expression: '$exp0'.")
  }

  /**
    * Lowers the given body term `pat0` (a subset of patterns).
    */
  private def visitBodyTerm(pat0: Pattern)(implicit root: Root, flix: Flix): Expression = pat0 match {
    case Pattern.Wild(_, loc) => mkBodyWildTerm(loc)

    case Pattern.Var(sym, _, _) => mkBodyVarTerm(sym)

    case Pattern.Unit(loc) => mkUnsafeBox(Expression.Unit(loc))

    case Pattern.True(loc) => mkUnsafeBox(boxBool(Expression.True(loc)))

    case Pattern.False(loc) => mkUnsafeBox(boxBool(Expression.False(loc)))

    case Pattern.Char(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Pattern.Float32(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Pattern.Float64(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Pattern.Int8(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Pattern.Int16(lit, loc) => ??? // TODO: Benjamin: Similar to the cases above.

    case Pattern.Int32(lit, loc) => mkUnsafeBox(boxInt32(Expression.Int32(lit, loc)))

    case Pattern.Int64(lit, loc) => mkUnsafeBox(boxInt64(Expression.Int64(lit, loc)))

    case Pattern.BigInt(lit, loc) => mkUnsafeBox(Expression.BigInt(lit, loc))

    case Pattern.Str(lit, loc) => mkUnsafeBox(Expression.Str(lit, loc))

    case Pattern.Tag(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.") // TODO: Support tags???

    case Pattern.Tuple(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.Array(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.ArrayTailSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.ArrayHeadSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")
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

  private def visitSourceLocation(loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = ???

  private def mkUnitTag(sym: Symbol.EnumSym, tag: String, tpe: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val innerExp = Expression.Unit(loc)
    Expression.Tag(sym, Name.Tag(tag, loc), innerExp, tpe, Type.Pure, loc)
  }

  private def mkHeadVarTerm(sym: Symbol.VarSym)(implicit root: Root, flix: Flix): Expression = ??? // TODO

  private def mkBodyWildTerm(loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = ??? // TODO

  private def mkBodyVarTerm(sym: Symbol.VarSym)(implicit root: Root, flix: Flix): Expression = ??? // TODO

  private def boxBool(exp: Expression)(implicit root: Root, flix: Flix): Expression = ??? // TODO

  // TODO: Benjamin: Add other boxing operations.
  private def boxInt32(exp: Expression)(implicit root: Root, flix: Flix): Expression = {
    val loc = exp.loc
    val clazz = classOf[java.lang.Integer]
    val method = clazz.getMethod("valueOf", Integer.TYPE)
    val tpe = Type.mkNative(clazz)
    val eff = Type.Pure
    Expression.InvokeStaticMethod(method, List(exp), tpe, eff, loc)
  }

  private def boxInt64(exp: Expression)(implicit root: Root, flix: Flix): Expression = ??? // TODO

  private def mkUnsafeBox(exp: Expression)(implicit root: Root, flix: Flix): Expression = ??? // TODO

}
