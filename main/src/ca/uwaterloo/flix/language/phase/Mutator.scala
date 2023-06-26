package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Mutator {

  // TODO: The purpose of this phase is to:
  // 1. Pick a random def (perhaps rather a def from a specific file ?)
  // 2. Pick a random expr (how?)
  // 3. Slightly modify the expr (e.g. changing (x + 1) to (x - 1).

  // Once this phase has been implemented, then we can introduce a new MutationTester class and a few changes to the Flix object to tie everything together.

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Mutator") {

    // Pick a random def
    val pickedDef = random(root.defs.values.toList)

    // Mutate the def.
    val mutatedDef = mutateDef(pickedDef)

    // Rebuild the AST and return it.
    root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef)).toSuccess
  }

  private def mutateDef(defn: Def): Def = {
    // We mutate the expression and rebuild in the def.
    val mutatedExp = mutateExpr(defn.impl.exp)

    defn.copy(impl = defn.impl.copy(exp = mutatedExp))
  }

  // TODO recurse on the expr, find a random node, and mutate it.
  private def mutateExpr(exp: Expression): Expression = exp match {
    case Expression.Cst(cst, tpe, loc) => cst match {
      case Constant.Unit => exp
      case Constant.Null => ???
      case Constant.Bool(lit) => ???
      case Constant.Char(lit) => ???
      case Constant.Float32(lit) => ???
      case Constant.Float64(lit) => ???
      case Constant.BigDecimal(lit) => ???
      case Constant.Int8(lit) => ???
      case Constant.Int16(lit) => ???
      case Constant.Int32(lit) =>
        // Two very simple and boring mutations:
        val l = lit match {
          case 0 => 1
          case 1 => 0
          case _ => lit
        }
        Expression.Cst(Constant.Int32(l), tpe, loc)
      case Constant.Int64(lit) => ???
      case Constant.BigInt(lit) => ???
      case Constant.Str(lit) => ???
      case Constant.Regex(lit) => ???
    }
    case Expression.Var(sym, tpe, loc) => ???
    case Expression.Def(sym, tpe, loc) => ???
    case Expression.Sig(sym, tpe, loc) => ???
    case Expression.Hole(sym, tpe, loc) => ???
    case Expression.HoleWithExp(exp, tpe, eff, loc) => ???
    case Expression.OpenAs(symUse, exp, tpe, loc) => ???
    case Expression.Use(sym, alias, exp, loc) => ???
    case Expression.Lambda(fparam, exp, tpe, loc) => ???
    case Expression.Apply(exp, exps, tpe, eff, loc) => ???
    case Expression.Unary(sop, exp, tpe, eff, loc) => ???
    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) => ???
    case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) => ???
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) => ???
    case Expression.Region(tpe, loc) => ???
    case Expression.Scope(sym, regionVar, exp, tpe, eff, loc) => ???
    case Expression.ScopeExit(exp1, exp2, tpe, eff, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expression.Stm(exp1, exp2, tpe, eff, loc) => ???
    case Expression.Discard(exp, eff, loc) => ???
    case Expression.Match(exp, rules, tpe, eff, loc) => ???
    case Expression.TypeMatch(exp, rules, tpe, eff, loc) => ???
    case Expression.RelationalChoose(exps, rules, tpe, eff, loc) => ???
    case Expression.RestrictableChoose(star, exp, rules, tpe, eff, loc) => ???
    case Expression.Tag(sym, exp, tpe, eff, loc) => ???
    case Expression.RestrictableTag(sym, exp, tpe, eff, loc) => ???
    case Expression.Tuple(elms, tpe, eff, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, eff, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) => ???
    case Expression.ArrayLit(exps, exp, tpe, eff, loc) => ???
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ???
    case Expression.ArrayLength(base, eff, loc) => ???
    case Expression.ArrayStore(base, index, elm, eff, loc) => ???
    case Expression.VectorLit(exps, tpe, eff, loc) => ???
    case Expression.VectorLoad(exp1, exp2, tpe, eff, loc) => ???
    case Expression.VectorLength(exp, loc) => ???
    case Expression.Ref(exp1, exp2, tpe, eff, loc) => ???
    case Expression.Deref(exp, tpe, eff, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ???
    case Expression.Ascribe(exp, tpe, eff, loc) => ???
    case Expression.InstanceOf(exp, clazz, loc) => ???
    case Expression.CheckedCast(cast, exp, tpe, eff, loc) => ???
    case Expression.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => ???
    case Expression.UncheckedMaskingCast(exp, tpe, eff, loc) => ???
    case Expression.Without(exp, effUse, tpe, eff, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ???
    case Expression.TryWith(exp, effUse, rules, tpe, eff, loc) => ???
    case Expression.Do(op, exps, tpe, eff, loc) => ???
    case Expression.Resume(exp, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => ???
    case Expression.GetField(field, exp, tpe, eff, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => ???
    case Expression.GetStaticField(field, tpe, eff, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, eff, loc) => ???
    case Expression.NewObject(name, clazz, tpe, eff, methods, loc) => ???
    case Expression.NewChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expression.GetChannel(exp, tpe, eff, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ???
    case Expression.Spawn(exp1, exp2, tpe, eff, loc) => ???
    case Expression.ParYield(frags, exp, tpe, eff, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, eff, loc) => ???
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => ???
    case Expression.FixpointLambda(pparams, exp, stf, tpe, eff, loc) => ???
    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) => ???
    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => ???
    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) => ???
    case Expression.FixpointInject(exp, pred, tpe, eff, loc) => ???
    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ???
    case Expression.Error(m, tpe, eff) => ???
  }

  private def random[A](l: List[A]): A = ??? // TODO

}
