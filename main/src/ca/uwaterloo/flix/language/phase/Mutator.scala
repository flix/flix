package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.SemanticOp.BoolOp
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
  private def mutateExpr(exp: Expr): Expr = exp match {
    case Expr.Cst(cst, tpe, loc) => cst match {
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
        Expr.Cst(Constant.Int32(l), tpe, loc)
      case Constant.Int64(lit) => ???
      case Constant.BigInt(lit) => ???
      case Constant.Str(lit) => ???
      case Constant.Regex(lit) => ???
    }
    case Expr.Var(sym, tpe, loc) => ???
    case Expr.Def(sym, tpe, loc) => ???
    case Expr.Sig(sym, tpe, loc) => ???
    case Expr.Hole(sym, tpe, loc) => ???
    case Expr.HoleWithExp(exp, tpe, eff, loc) => ???
    case Expr.OpenAs(symUse, exp, tpe, loc) => ???
    case Expr.Use(sym, alias, exp, loc) => ???
    case Expr.Lambda(fparam, exp, tpe, loc) => ???
    case Expr.Apply(exp, exps, tpe, eff, loc) => ???
    case Expr.Unary(sop, exp, tpe, eff, loc) => ???
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val op = sop match {
        case BoolOp.Eq => BoolOp.Neq
        case BoolOp.Neq => BoolOp.Eq
        case _ => ???
      }
      Expr.Binary(op, exp1, exp2, tpe, eff, loc)
    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => ???
    case Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) => ???
    case Expr.Region(tpe, loc) => ???
    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => ???
    case Expr.ScopeExit(exp1, exp2, tpe, eff, loc) => ???
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => ???
    case Expr.Discard(exp, eff, loc) => ???
    case Expr.Match(exp, rules, tpe, eff, loc) => ???
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => ???
    case Expr.RelationalChoose(exps, rules, tpe, eff, loc) => ???
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => ???
    case Expr.Tag(sym, exp, tpe, eff, loc) => ???
    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => ???
    case Expr.Tuple(elms, tpe, eff, loc) => ???
    case Expr.RecordEmpty(tpe, loc) => ???
    case Expr.RecordSelect(exp, field, tpe, eff, loc) => ???
    case Expr.RecordExtend(field, value, rest, tpe, eff, loc) => ???
    case Expr.RecordRestrict(field, rest, tpe, eff, loc) => ???
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => ???
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expr.ArrayLoad(base, index, tpe, eff, loc) => ???
    case Expr.ArrayLength(base, eff, loc) => ???
    case Expr.ArrayStore(base, index, elm, eff, loc) => ???
    case Expr.VectorLit(exps, tpe, eff, loc) => ???
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => ???
    case Expr.VectorLength(exp, loc) => ???
    case Expr.Ref(exp1, exp2, tpe, eff, loc) => ???
    case Expr.Deref(exp, tpe, eff, loc) => ???
    case Expr.Assign(exp1, exp2, tpe, eff, loc) => ???
    case Expr.Ascribe(exp, tpe, eff, loc) => ???
    case Expr.InstanceOf(exp, clazz, loc) => ???
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => ???
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => ???
    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => ???
    case Expr.Without(exp, effUse, tpe, eff, loc) => ???
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => ???
    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => ???
    case Expr.Do(op, exps, tpe, eff, loc) => ???
    case Expr.Resume(exp, tpe, loc) => ???
    case Expr.InvokeConstructor(constructor, args, tpe, eff, loc) => ???
    case Expr.InvokeMethod(method, exp, args, tpe, eff, loc) => ???
    case Expr.InvokeStaticMethod(method, args, tpe, eff, loc) => ???
    case Expr.GetField(field, exp, tpe, eff, loc) => ???
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => ???
    case Expr.GetStaticField(field, tpe, eff, loc) => ???
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => ???
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ???
    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expr.GetChannel(exp, tpe, eff, loc) => ???
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => ???
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => ???
    case Expr.ParYield(frags, exp, tpe, eff, loc) => ???
    case Expr.Lazy(exp, tpe, loc) => ???
    case Expr.Force(exp, tpe, eff, loc) => ???
    case Expr.FixpointConstraintSet(cs, stf, tpe, loc) => ???
    case Expr.FixpointLambda(pparams, exp, stf, tpe, eff, loc) => ???
    case Expr.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) => ???
    case Expr.FixpointSolve(exp, stf, tpe, eff, loc) => ???
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => ???
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => ???
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => ???
    case Expr.Error(m, tpe, eff) => ???
  }

  private def random[A](l: List[A]): A = ??? // TODO

}
