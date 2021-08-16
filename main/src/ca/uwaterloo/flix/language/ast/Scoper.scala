package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.errors.ScopeError
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.language.phase.unification.ScopeInferMonad
import ca.uwaterloo.flix.util.Validation

object Scoper extends Phase[Root, Root] {
  override def run(input: Root)(implicit flix: Flix): Validation[Root, ScopeError] = flix.phase("Scoper") {
    // check all defs
    // check all sigs
    // check all instance defs
  }

  private def visitDef(defn: Def): Validation[Def, ScopeError] = defn match {
    case Def(sym, spec, impl) => ???
  }

  private def inferExp(exp: Expression): ScopeInferMonad[(Type, ScopeScheme)] = exp match {
    case Expression.Unit(loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Null(tpe, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.True(loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.False(loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Char(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Float32(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Float64(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Int8(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Int16(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Int32(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Int64(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.BigInt(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Str(lit, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Default(tpe, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Wild(tpe, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme.Unit))
    case Expression.Var(sym, tpe, loc) => ScopeInferMonad.point((Type.Unscoped, ScopeScheme))
    case Expression.Def(sym, tpe, loc) =>
    case Expression.Sig(sym, tpe, loc) =>
    case Expression.Hole(sym, tpe, eff, loc) =>
    case Expression.Lambda(fparam, exp, tpe, loc) =>
    case Expression.Apply(exp, exps, tpe, eff, loc) =>
    case Expression.Unary(sop, exp, tpe, eff, loc) =>
    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
    case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
    case Expression.LetRegion(sym, exp, tpe, eff, loc) =>
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
    case Expression.Match(exp, rules, tpe, eff, loc) =>
    case Expression.Choose(exps, rules, tpe, eff, loc) =>
    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
    case Expression.Tuple(elms, tpe, eff, loc) =>
    case Expression.RecordEmpty(tpe, loc) =>
    case Expression.RecordSelect(exp, field, tpe, eff, loc) =>
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
    case Expression.ArrayLit(elms, tpe, eff, loc) =>
    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
    case Expression.ArrayLength(base, eff, loc) =>
    case Expression.ArrayStore(base, index, elm, loc) =>
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
    case Expression.Ref(exp, tpe, eff, loc) =>
    case Expression.Deref(exp, tpe, eff, loc) =>
    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
    case Expression.Existential(fparam, exp, loc) =>
    case Expression.Universal(fparam, exp, loc) =>
    case Expression.Ascribe(exp, tpe, eff, loc) =>
    case Expression.Cast(exp, tpe, eff, loc) =>
    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
    case Expression.GetField(field, exp, tpe, eff, loc) =>
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
    case Expression.GetStaticField(field, tpe, eff, loc) =>
    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
    case Expression.NewChannel(exp, tpe, eff, loc) =>
    case Expression.GetChannel(exp, tpe, eff, loc) =>
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
    case Expression.Spawn(exp, tpe, eff, loc) =>
    case Expression.Lazy(exp, tpe, loc) =>
    case Expression.Force(exp, tpe, eff, loc) =>
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
    case Expression.MatchEff(exp1, exp2, exp3, tpe, eff, loc) =>
  }

  private case class ScopeInfo(sc: Type, scSc: ScopeScheme)
}
