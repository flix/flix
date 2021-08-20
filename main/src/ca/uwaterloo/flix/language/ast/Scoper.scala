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

  private def inferExp(exp: Expression, root: Root): ScopeInferMonad[ScopeInfo] = exp match {
    case Expression.Unit(loc) => ScopeInferMonad.point(ScopeType.Unit.asUnscoped)
    case Expression.Null(tpe, loc) => ScopeInferMonad.point(ScopeType.Null.asUnscoped) // MATT probably need to use the tpe
    case Expression.True(loc) => ScopeInferMonad.point(ScopeType.True.asUnscoped)
    case Expression.False(loc) => ScopeInferMonad.point(ScopeType.False.asUnscoped)
    case Expression.Char(lit, loc) => ScopeInferMonad.point(ScopeType.Char.asUnscoped)
    case Expression.Float32(lit, loc) => ScopeInferMonad.point(ScopeType.Float32.asUnscoped)
    case Expression.Float64(lit, loc) => ScopeInferMonad.point(ScopeType.Float64.asUnscoped)
    case Expression.Int8(lit, loc) => ScopeInferMonad.point(ScopeType.Int8.asUnscoped)
    case Expression.Int16(lit, loc) => ScopeInferMonad.point(ScopeType.Int16.asUnscoped)
    case Expression.Int32(lit, loc) => ScopeInferMonad.point(ScopeType.Int32.asUnscoped)
    case Expression.Int64(lit, loc) => ScopeInferMonad.point(ScopeType.Int64.asUnscoped)
    case Expression.BigInt(lit, loc) => ScopeInferMonad.point(ScopeType.BigInt.asUnscoped)
    case Expression.Str(lit, loc) => ScopeInferMonad.point(ScopeType.Str.asUnscoped)
    case Expression.Default(tpe, loc) => ScopeInferMonad.point(ScopeType.Unit.asUnscoped) // MATT hack
    case Expression.Wild(tpe, loc) => ??? // MATT needs associated ScopeInfo
    case Expression.Var(sym, tpe, sco, loc) => ScopeInferMonad.point(sco)
    case Expression.Def(sym, tpe, loc) => ScopeInferMonad.point(root.defs(sym).spec.sco.asUnscoped)
    case Expression.Sig(sym, tpe, loc) => ScopeInferMonad.point(root.sigs(sym).spec.sco.asUnscoped)
    case Expression.Hole(sym, tpe, eff, loc) => ??? // MATT
    case Expression.Lambda(fparam, exp, tpe, loc) => // MATT careful with external/internal scopes
      // result scope type is fparam.sco -> tpe.sco
      // if exp has scoped free vars:
        // then overall Scoped
      // else
        // overall UnScoped
    case Expression.Apply(exp, exps, tpe, eff, loc) => // MATT careful with external/internal scopes
      // unify exp scope with (a1, a2, a3...) -> b
      // unify (a1, a2, a3) with exps
      // result scope type is b

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      for {
        sco <- inferExp(exp, root)
      } yield sco.scopeType.asUnscoped
    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      // sco1.scopeType and sco2.scopeType are the same
      for {
        sco1 <- inferExp(exp1, root)
        sco2 <- inferExp(exp2, root)
      } yield sco1.scopeType.asUnscoped
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
}
