package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Scopedness, Symbol, Type}
import ca.uwaterloo.flix.language.errors.ScopeError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess


// MATT license
// MATT docs
object Scoper extends Phase[Root, Root] {

  private val noEscapes = Map.empty[Symbol.VarSym, Expression]

  override def run(input: Root)(implicit flix: Flix): Validation[Root, ScopeError] = {
    // analyze defs
    // analyze sigs
    // analyze instances
  }

  private def checkDef(defn: Def): Validation[Unit, ScopeError] = defn match {
    case Def(sym, spec, impl) => checkImpl(impl)
  }

  private def checkImpl(impl: Impl): Validation[Unit, ScopeError] = impl match {
    case Impl(exp, inferredScheme) => checkExp(exp, Nil)
  }

  private def checkExp(exp0: Expression, scopedEnv: List[Symbol.VarSym]): Validation[Map[Symbol.VarSym, Expression], ScopeError] = exp match {
    case Expression.Unit(loc) => noEscapes.toSuccess
    case Expression.Null(tpe, loc) => noEscapes.toSuccess
    case Expression.True(loc) => noEscapes.toSuccess
    case Expression.False(loc) => noEscapes.toSuccess
    case Expression.Char(lit, loc) => noEscapes.toSuccess
    case Expression.Float32(lit, loc) => noEscapes.toSuccess
    case Expression.Float64(lit, loc) => noEscapes.toSuccess
    case Expression.Int8(lit, loc) => noEscapes.toSuccess
    case Expression.Int16(lit, loc) => noEscapes.toSuccess
    case Expression.Int32(lit, loc) => noEscapes.toSuccess
    case Expression.Int64(lit, loc) => noEscapes.toSuccess
    case Expression.BigInt(lit, loc) => noEscapes.toSuccess
    case Expression.Str(lit, loc) => noEscapes.toSuccess
    case Expression.Default(tpe, loc) => noEscapes.toSuccess
    case Expression.Wild(tpe, loc) => noEscapes.toSuccess
    case exp@Expression.Var(sym, tpe, loc) => sym.scopedness match {
      // Case 1: The var is scoped; nothing to check
      case Scopedness.Unscoped => noEscapes.toSuccess
      // Case 2: The var is unscoped.
      case Scopedness.Scoped =>
        // Case 2.1: We're accessing the var outside its scope: Error
        if (!scopedEnv.contains(sym)) {
          ??? // ScopeError
          // Case 2.2: We're still in the var's scope.
        } else {
          Map(sym -> exp).toSuccess
        }
    }
    case Expression.Def(sym, tpe, loc) => noEscapes.toSuccess
    case Expression.Sig(sym, tpe, loc) => noEscapes.toSuccess
    case Expression.Hole(sym, tpe, eff, loc) => noEscapes.toSuccess

    // If we're here, then we are defining a first-class function.
    // So we reset the scoped env.
    case Expression.Lambda(fparam, exp, tpe, loc) => fparam.sym.scopedness match {
      case Scopedness.Unscoped =>
        for {
          _ <- checkExp(exp, Nil)
        } yield noEscapes
      case Scopedness.Scoped =>
        for {
          _ <- checkExp(exp, List(fparam.sym))
        } yield noEscapes
    }
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
}
