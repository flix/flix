package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.KindedAst.Expr
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, LevelEnv, RigidityEnv, Scheme, SourceLocation, Symbol, Type}

import scala.collection.mutable.ListBuffer

object ConstraintGeneration {

  sealed class Constraint

  object Constraint {
    case class Equality(tpe1: Type, tpe2: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation) extends Constraint

    case class Class(sym: Symbol.ClassSym, tpe: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation) extends Constraint
  }

  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    c.constrs.append(Constraint.Equality(tpe1, tpe2, c.renv, c.lenv, loc))
  }

  def addTypeConstraints(tconstrs0: List[Ast.TypeConstraint], loc: SourceLocation)(implicit c: Context): Unit = {
    val tconstrs = tconstrs0.map {
      case Ast.TypeConstraint(head, arg, _) => Constraint.Class(head.sym, arg, c.renv, c.lenv, loc)
    }
    c.constrs.addAll(tconstrs)
  }

  case class Context(constrs: ListBuffer[Constraint], var renv: RigidityEnv, var lenv: LevelEnv)

  def visitExp(exp0: KindedAst.Expr, renv: RigidityEnv, lenv: LevelEnv)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = exp0 match {
    case Expr.Var(sym, loc) => (sym.tvar, Type.Pure)
    case Expr.Def(sym, tvar, loc) =>
      val defn = root.defs(sym)
      val (tconstrs, defTpe) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
      unifyTypeM(tvar, defTpe, loc)
      addTypeConstraints(tconstrs, loc)
      val resTpe = defTpe
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Sig(sym, tvar, loc) =>
      val sig = root.classes(sym.clazz).sigs(sym)
      val (tconstrs, sigTpe) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
      unifyTypeM(tvar, sigTpe, loc)
      addTypeConstraints(tconstrs, loc)
      val resTpe = sigTpe
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Hole(sym, tpe, loc) =>
      (tpe, Type.Pure)

    case Expr.HoleWithExp(exp, tvar, evar, loc) =>
      val (tpe, eff) = visitExp(exp, renv, lenv)
      // effect type is AT LEAST the inner expression's effect
      val atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Eff, loc.asSynthetic), loc.asSynthetic)
      unifyTypeM(atLeastEff, evar, loc)
      // result type is whatever is needed for the hole
      val resTpe = tvar
      val resEff = atLeastEff
      (resTpe, resEff)

    case Expr.OpenAs(symUse, exp, tvar, loc) => ???

    case Expr.Use(sym, alias, exp, loc) =>
      visitExp(exp, renv, lenv)

    case Expr.Cst(cst, loc) =>
      val resTpe = TypeReconstruction.constantType(cst)
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Apply(exp, exps, tpe, eff, loc) => ???

    case Expr.Lambda(fparam, exp, loc) => {
      unifyTypeM(fparam.sym.tvar, fparam.tpe, loc)
      val (tpe, eff) = visitExp(exp, renv, lenv)
      val resTpe = Type.mkArrowWithEffect(fparam.tpe, eff, tpe, loc)
      val resEff = Type.Pure
      (resTpe, resEff)
    }
    case Expr.Unary(sop, exp, tpe, loc) => ???
    case Expr.Binary(sop, exp1, exp2, tpe, loc) => ???

    case Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val (tpe1, eff1) = visitExp(exp1, renv, lenv)
      val (tpe2, eff2) = visitExp(exp2, renv, lenv)
      val (tpe3, eff3) = visitExp(exp3, renv, lenv)
      unifyTypeM(tpe1, Type.Bool, exp1.loc)
      unifyTypeM(tpe2, tpe3, loc)
      val resTpe = tpe3
      val resEff = Type.mkUnion(eff1, eff2, eff3, loc)
      (resTpe, resEff)

    case Expr.Stm(exp1, exp2, loc) =>
      val (tpe1, eff1) = visitExp(exp1, renv, lenv)
      val (tpe2, eff2) = visitExp(exp2, renv, lenv)
      val resTpe = tpe2
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)

    case Expr.Discard(exp, loc) =>
      val (tpe, eff) = visitExp(exp, renv, lenv)
      val resTpe = Type.Unit
      val resEff = eff
      (resTpe, resEff)

    case Expr.Let(sym, mod, exp1, exp2, loc) =>
      val (tpe1, eff1) = visitExp(exp1, renv, lenv)
      unifyTypeM(sym.tvar, tpe1, loc)
      val (tpe2, eff2) = visitExp(exp2, renv, lenv)
      val resTpe = tpe2
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)

    case Expr.LetRec(sym, mod, exp1, exp2, loc) => ???

    case Expr.Region(tpe, loc) =>
      val resTpe = tpe
      val effTpe = Type.Pure
      (resTpe, effTpe)

    case Expr.Scope(sym, regionVar, exp1, pvar, loc) => ???
    case Expr.ScopeExit(exp1, exp2, loc) => ???
    case Expr.Match(exp, rules, loc) => ???
    case Expr.TypeMatch(exp, rules, loc) => ???
    case Expr.RelationalChoose(star, exps, rules, tpe, loc) => ???
    case Expr.RestrictableChoose(star, exp, rules, tpe, loc) => ???
    case Expr.Tag(sym, exp, tpe, loc) => ???
    case Expr.RestrictableTag(sym, exp, isOpen, tpe, loc) => ???
    case Expr.Tuple(elms, loc) => ???
    case Expr.RecordEmpty(loc) => ???
    case Expr.RecordSelect(exp, label, tpe, loc) => ???
    case Expr.RecordExtend(label, value, rest, tpe, loc) => ???
    case Expr.RecordRestrict(label, rest, tpe, loc) => ???
    case Expr.ArrayLit(exps, exp, tvar, pvar, loc) => ???
    case Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) => ???
    case Expr.ArrayLoad(base, index, tpe, pvar, loc) => ???
    case Expr.ArrayStore(base, index, elm, pvar, loc) => ???
    case Expr.ArrayLength(base, loc) => ???
    case Expr.VectorLit(exps, tvar, pvar, loc) => ???
    case Expr.VectorLoad(exp1, exp2, tpe, pvar, loc) => ???
    case Expr.VectorLength(exp, loc) => ???
    case Expr.Ref(exp1, exp2, tvar, pvar, loc) => ???
    case Expr.Deref(exp, tvar, pvar, loc) => ???
    case Expr.Assign(exp1, exp2, pvar, loc) => ???
    case Expr.Ascribe(exp, expectedType, expectedPur, tpe, loc) => ???
    case Expr.InstanceOf(exp, clazz, loc) => ???
    case Expr.CheckedCast(cast, exp, tvar, pvar, loc) => ???
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, loc) => ???
    case Expr.UncheckedMaskingCast(exp, loc) => ???
    case Expr.Without(exp, eff, loc) => ???
    case Expr.TryCatch(exp, rules, loc) => ???
    case Expr.TryWith(exp, eff, rules, tvar, loc) => ???
    case Expr.Do(op, args, tvar, loc) => ???
    case Expr.Resume(exp, argTvar, retTvar, loc) => ???
    case Expr.InvokeConstructor(constructor, args, loc) => ???
    case Expr.InvokeMethod(method, clazz, exp, args, loc) => ???
    case Expr.InvokeStaticMethod(method, args, loc) => ???
    case Expr.GetField(field, clazz, exp, loc) => ???
    case Expr.PutField(field, clazz, exp1, exp2, loc) => ???
    case Expr.GetStaticField(field, loc) => ???
    case Expr.PutStaticField(field, exp, loc) => ???
    case Expr.NewObject(name, clazz, methods, loc) => ???
    case Expr.NewChannel(exp1, exp2, tvar, loc) => ???
    case Expr.GetChannel(exp, tpe, loc) => ???
    case Expr.PutChannel(exp1, exp2, loc) => ???
    case Expr.SelectChannel(rules, default, tpe, loc) => ???
    case Expr.Spawn(exp1, exp2, loc) => ???
    case Expr.ParYield(frags, exp, loc) => ???
    case Expr.Lazy(exp, loc) => ???
    case Expr.Force(exp, tpe, loc) => ???
    case Expr.FixpointConstraintSet(cs, tpe, loc) => ???
    case Expr.FixpointLambda(pparams, exp, tpe, loc) => ???
    case Expr.FixpointMerge(exp1, exp2, loc) => ???
    case Expr.FixpointSolve(exp, loc) => ???
    case Expr.FixpointFilter(pred, exp, tpe, loc) => ???
    case Expr.FixpointInject(exp, pred, tpe, loc) => ???
    case Expr.FixpointProject(pred, exp1, exp2, tpe, loc) => ???
    case Expr.Error(m, tpe, eff) => ???
  }
}
