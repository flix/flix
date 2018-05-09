package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.Ast.HoleContext
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type}

object TypedAstOps {

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  def freeVarsOf(pat0: Pattern): Set[Symbol.VarSym] = pat0 match {
    case Pattern.Wild(tpe, loc) => Set.empty
    case Pattern.Var(sym, tpe, loc) => Set(sym)
    case Pattern.Unit(loc) => Set.empty
    case Pattern.True(loc) => Set.empty
    case Pattern.False(loc) => Set.empty
    case Pattern.Char(lit, loc) => Set.empty
    case Pattern.Float32(lit, loc) => Set.empty
    case Pattern.Float64(lit, loc) => Set.empty
    case Pattern.Int8(lit, loc) => Set.empty
    case Pattern.Int16(lit, loc) => Set.empty
    case Pattern.Int32(lit, loc) => Set.empty
    case Pattern.Int64(lit, loc) => Set.empty
    case Pattern.BigInt(lit, loc) => Set.empty
    case Pattern.Str(lit, loc) => Set.empty
    case Pattern.Tag(sym, tag, pat, tpe, loc) => freeVarsOf(pat)
    case Pattern.Tuple(elms, tpe, loc) => (elms flatMap freeVarsOf).toSet
  }

  /**
    * Returns a map of the holes in the given ast `root`.
    */
  def holesOf(root: Root): Map[Symbol.HoleSym, HoleContext] = {
    /**
      * Finds the holes and hole contexts in the given expression `exp0`.
      */
    def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Type]): Map[Symbol.HoleSym, HoleContext] = exp0 match {
      case Expression.Wild(tpe, eff, loc) => Map.empty
      case Expression.Var(sym, tpe, eff, loc) => Map.empty
      case Expression.Def(sym, tpe, eff, loc) => Map.empty
      case Expression.Eff(sym, tpe, eff, loc) => Map.empty

      case Expression.Hole(sym, tpe, eff, loc) =>
        Map(sym -> HoleContext(sym, tpe, env0))

      case Expression.Unit(loc) => Map.empty
      case Expression.True(loc) => Map.empty
      case Expression.False(loc) => Map.empty
      case Expression.Char(lit, loc) => Map.empty
      case Expression.Float32(lit, loc) => Map.empty
      case Expression.Float64(lit, loc) => Map.empty
      case Expression.Int8(lit, loc) => Map.empty
      case Expression.Int16(lit, loc) => Map.empty
      case Expression.Int32(lit, loc) => Map.empty
      case Expression.Int64(lit, loc) => Map.empty
      case Expression.BigInt(lit, loc) => Map.empty
      case Expression.Str(lit, loc) => Map.empty

      case Expression.Lambda(fparams, exp, tpe, eff, loc) =>
        val env1 = fparams.map(p => p.sym -> p.tpe).toSet
        visitExp(exp, env0 ++ env1)

      case Expression.Apply(exp, args, tpe, eff, loc) =>
        args.foldLeft(visitExp(exp, env0)) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.Unary(op, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0 + (sym -> exp1.tpe))

      case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0 + (sym -> exp1.tpe)) ++ visitExp(exp2, env0 + (sym -> exp1.tpe))

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0) ++ visitExp(exp3, env0)

      case Expression.Match(matchExp, rules, tpe, eff, loc) =>
        val m = visitExp(matchExp, env0)
        rules.foldLeft(m) {
          case (macc, MatchRule(pat, guard, exp)) =>
            macc ++ visitExp(guard, env0) ++ visitExp(exp, binds(pat) ++ env0)
        }

      case Expression.Switch(rules, tpe, eff, loc) =>
        rules.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, (exp1, exp2)) => macc ++ visitExp(exp1, env0) ++ visitExp(exp2, env0)
        }

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Tuple(elms, tpe, eff, loc) =>
        elms.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, elm) => macc ++ visitExp(elm, env0)
        }

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        elms.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]){
          case (macc, elm) => macc ++ visitExp(elm, env0)
        }

      case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        visitExp(elm, env0)

      case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        visitExp(base, env0) ++ visitExp(index, env0)

      case Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
        visitExp(base, env0) ++ visitExp(index, env0) ++ visitExp(elm, env0)

      case Expression.ArrayLength(base, tpe, eff, loc) =>
        visitExp(base, env0)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) =>
        visitExp(base, env0) ++ visitExp(beginIndex, env0) ++ visitExp(endIndex, env0)

      case Expression.VectorLit(elms, tpe, eff, loc) =>
        elms.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]){
          case (macc, elm) => macc ++ visitExp(elm, env0)
        }

      case Expression.VectorNew(elm, len, tpe, eff, loc) =>
        visitExp(elm, env0)

      case Expression.VectorLoad(base, index, tpe, eff, loc) =>
        visitExp(base, env0)

      case Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
        visitExp(base, env0)

      case Expression.VectorLength(base, tpe, eff, loc) =>
        visitExp(base, env0)

      case Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) =>
        visitExp(base, env0) ++ visitExp(endIndex, env0)

      case Expression.Unique(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Ref(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Deref(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
        bindings.foldLeft(visitExp(exp, env0)) {
          case (macc, HandlerBinding(sym, handler)) => macc ++ visitExp(handler, env0)
        }

      case Expression.Existential(fparam, exp, eff, loc) =>
        visitExp(exp, env0 + (fparam.sym -> fparam.tpe))

      case Expression.Universal(fparam, exp, eff, loc) =>
        visitExp(exp, env0 + (fparam.sym -> fparam.tpe))

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Cast(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
        args.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.NativeField(field, tpe, eff, loc) => Map.empty

      case Expression.NativeMethod(method, args, tpe, eff, loc) =>
        args.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.UserError(tpe, eff, loc) => Map.empty
    }

    /**
      * Returns the set of variable symbols bound by the given pattern `pat0`.
      */
    def binds(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
      case Pattern.Wild(tpe, loc) => Map.empty
      case Pattern.Var(sym, tpe, loc) => Map(sym -> tpe)
      case Pattern.Unit(loc) => Map.empty
      case Pattern.True(loc) => Map.empty
      case Pattern.False(loc) => Map.empty
      case Pattern.Char(lit, loc) => Map.empty
      case Pattern.Float32(lit, loc) => Map.empty
      case Pattern.Float64(lit, loc) => Map.empty
      case Pattern.Int8(lit, loc) => Map.empty
      case Pattern.Int16(lit, loc) => Map.empty
      case Pattern.Int32(lit, loc) => Map.empty
      case Pattern.Int64(lit, loc) => Map.empty
      case Pattern.BigInt(lit, loc) => Map.empty
      case Pattern.Str(lit, loc) => Map.empty
      case Pattern.Tag(sym, tag, pat, tpe, loc) => binds(pat)
      case Pattern.Tuple(elms, tpe, loc) => elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, elm) => macc ++ binds(elm)
      }
    }

    /**
      * Returns the set of variables bound by the given list of formal parameters `fparams`.
      */
    def getEnvFromParams(fparams: List[FormalParam]): Map[Symbol.VarSym, Type] =
      fparams.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, FormalParam(sym, mod, tpe, loc)) => macc + (sym -> tpe)
      }

    // Visit every definition.
    root.defs.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
      case (macc, (sym, defn)) => macc ++ visitExp(defn.exp, getEnvFromParams(defn.fparams))
    }
  }

}
