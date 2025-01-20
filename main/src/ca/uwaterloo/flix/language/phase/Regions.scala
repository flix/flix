/*
 *  Copyright 2022 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.{CofiniteEffSet, InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The region phase ensures that regions do not escape outside of their scope.
  *
  * It does so by keep tracking of every region variable in scope and ensure that every
  * rigid Boolean type variable that occurs anywhere belongs is in scope.
  */
object Regions {

  def run(root: Root)(implicit flix: Flix): (Root, List[TypeError]) = flix.phaseNew("Regions") {
    implicit val sctx: SharedContext = SharedContext.mk()
    ParOps.parMapValues(root.defs)(visitDef)
    ParOps.parMapValues(root.sigs)(visitSig)
    ParOps.parMapValues(root.instances)(visitInstanceList)
    (root, sctx.errors.asScala.toList)
  }

  private def visitDef(defn: Def)(implicit sctx: SharedContext, flix: Flix): Def = {
    visitExp(defn.exp)(Nil, sctx, flix)
    defn
  }

  private def visitSig(sig: Sig)(implicit sctx: SharedContext, flix: Flix): Sig = {
    sig.exp.map(visitExp(_)(Nil, sctx, flix)).getOrElse(Nil)
    sig
  }

  private def visitInstanceList(insts: List[Instance])(implicit sctx: SharedContext, flix: Flix): List[Instance] = {
    insts.foreach(visitInstance)
    insts
  }

  private def visitInstance(ins: Instance)(implicit sctx: SharedContext, flix: Flix): Unit =
    ins.defs.foreach(visitDef)

  private def visitExp(exp0: Expr)(implicit scope: List[Type.Var], sctx: SharedContext, flix: Flix): Unit = exp0 match {
    case Expr.Cst(_, _, _) => ()

    case Expr.Var(_, tpe, loc) => checkType(tpe, loc)

    case Expr.Hole(_, _, _, _) => ()

    case Expr.HoleWithExp(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.OpenAs(_, exp, tpe, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Use(_, _, exp, _) => visitExp(exp)

    case Expr.Lambda(_, exp, tpe, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.ApplyClo(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.ApplyDef(_, exps, _, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.ApplyLocalDef(_, exps, _, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.ApplySig(_, exps, _, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.Unary(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Binary(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.Let(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.LocalDef(_, _, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.Region(_, _) => ()

    case Expr.Scope(_, regionVar, exp, tpe, _, loc) =>
      visitExp(exp)(regionVar :: scope, sctx, flix)
      checkType(tpe, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      checkType(tpe, loc)

    case Expr.Stm(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.Discard(exp, _, _) => visitExp(exp)

    case Expr.Match(exp, rules, tpe, _, loc) =>
      visitExp(exp)
      rules.foreach{ case MatchRule(_, guard, body) =>
        guard.foreach(visitExp)
        visitExp(body)
      }
      checkType(tpe, loc)

    case Expr.TypeMatch(exp, rules, tpe, _, loc) =>
      visitExp(exp)
      rules.foreach{ case TypeMatchRule(_, _, body) => visitExp(body) }
      checkType(tpe, loc)

    case Expr.RestrictableChoose(_, exp, rules, tpe, _, loc) =>
      visitExp(exp)
      rules.foreach{ case RestrictableChooseRule(_, exp) => visitExp(exp) }
      checkType(tpe, loc)

    case Expr.Tag(_, exps, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.RestrictableTag(_, exps, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.Tuple(elms, tpe, _, loc) =>
      elms.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.RecordSelect(exp, _, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.RecordExtend(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.RecordRestrict(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.ArrayLit(exps, exp, tpe, _, loc) =>
      exps.foreach(visitExp)
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      checkType(tpe, loc)

    case Expr.ArrayLoad(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.StructNew(_, fields, region, tpe, _, loc) =>
      fields.map { case (_, v) => v }.foreach(visitExp)
      visitExp(region)
      checkType(tpe, loc)

    case Expr.StructGet(e, _, tpe, _, loc) =>
      visitExp(e)
      checkType(tpe, loc)

    case Expr.StructPut(e1, _, e2, tpe, _, loc) =>
      visitExp(e1)
      visitExp(e2)
      checkType(tpe, loc)

    case Expr.VectorLit(exps, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.UncheckedCast(exp, _, _, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Unsafe(exp, runEff, _, _, loc) =>
      checkType(runEff, loc)
      visitExp(exp)

    case Expr.Without(exp, _, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.TryCatch(exp, rules, tpe, _, loc) =>
      rules.foreach{ case CatchRule(_, _, e) => visitExp(e) }
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Throw(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Handler(_, rules, _, _, _, tpe, loc) =>
      rules.foreach{ case HandlerRule(_, _, e) => visitExp(e) }
      checkType(tpe, loc)

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Do(_, exps, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.InvokeConstructor(_, exps, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.InvokeMethod(_, exp, exps, tpe, _, loc) =>
      visitExp(exp)
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.InvokeStaticMethod(_, exps, tpe, _, loc) =>
      exps.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.GetField(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.PutField(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.GetStaticField(_, _, _, _) => ()

    case Expr.PutStaticField(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.NewObject(_, _, tpe, _, methods, loc) =>
      methods.foreach(visitJvmMethod)
      checkType(tpe, loc)

    case Expr.NewChannel(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.GetChannel(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.PutChannel(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.SelectChannel(rules, default, tpe, _, loc) =>
      rules.foreach{
        case SelectChannelRule(_, chan, exp) =>
          visitExp(chan)
          visitExp(exp)
      }
      default.foreach(visitExp)
      checkType(tpe, loc)

    case Expr.Spawn(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.ParYield(frags, exp, tpe, _, loc) =>
      frags.foreach{
        case ParYieldFragment(_, e, _) => visitExp(e)
      }
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Lazy(exp, tpe, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Force(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      () // TODO

    case Expr.FixpointLambda(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.FixpointMerge(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      checkType(tpe, loc)

    case Expr.FixpointSolve(exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.FixpointFilter(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.FixpointInject(exp, _, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.FixpointProject(_, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)

    case Expr.Error(_, _, _) => ()
  }

  def visitJvmMethod(method: JvmMethod)(implicit scope: List[Type.Var], sctx: SharedContext, flix: Flix): Unit = method match {
    case JvmMethod(_, _, exp, tpe, _, loc) =>
      visitExp(exp)
      checkType(tpe, loc)
  }

  /**
    * Ensures that no region escapes inside `tpe`.
    */
  private def checkType(tpe: Type, loc: SourceLocation)(implicit scope: List[Type.Var], sctx: SharedContext, flix: Flix): Unit = {
    // Compute the region variables that escape.
    // We should minimize `tpe`, but we do not because of the performance cost.
    val regs = regionVarsOf(tpe)
    for (reg <- regs -- scope) {
      if (essentialTo(reg, tpe)) {
        sctx.errors.add(TypeError.RegionVarEscapes(reg, tpe, loc))
      }
    }
  }

  /**
    * Returns true iff the type variable `tvar` is essential to the type `tpe`.
    *
    * A type variable is essential if its ascription has a bearing on the resulting value.
    * For example, in the type `a and (not a)`, `a` is not essential since the result is always `false`.
    */
  def essentialTo(tvar: Type.Var, tpe: Type)(implicit flix: Flix): Boolean = {
    if (!tpe.typeVars.contains(tvar)) {
      // Case 1: The type variable is not present in the type. It cannot be essential.
      false
    } else {
      // Case 2: The type variable is present in the type. Check if it is essential to any of the booleans.
      boolTypesOf(tpe).exists(essentialToBool(tvar, _))
    }
  }

  /**
    * Returns true iff the type variable `tvar` is essential to the boolean formula `tpe`.
    * Assumes that `tvar` is present in the type.
    */
  def essentialToBool(tvar: Type.Var, tpe: Type)(implicit flix: Flix): Boolean = {
    // t0 = tpe[tvar -> False]
    val t0 = Substitution.singleton(tvar.sym, Type.Univ).apply(tpe)

    // t1 = tpe[tvar -> True]
    val t1 = Substitution.singleton(tvar.sym, Type.Pure).apply(tpe)

    // tvar is essential if t0 != t1
    !sameType(t0, t1)
  }

  /**
    * Extracts all the boolean formulas from the given type `t0`.
    */
  private def boolTypesOf(t0: Type): List[Type] = t0 match {
    case t if t.kind == Kind.Eff => List(t)
    case _: Type.Var => Nil
    case _: Type.Cst => Nil
    case Type.Apply(tpe1, tpe2, _) => boolTypesOf(tpe1) ::: boolTypesOf(tpe2)
    case Type.Alias(_, _, tpe, _) => boolTypesOf(tpe)
    case Type.JvmToType(tpe, _) => boolTypesOf(tpe)
    case Type.JvmToEff(tpe, _) => boolTypesOf(tpe)
    case Type.UnresolvedJvmType(member, _) => member.getTypeArguments.flatMap(boolTypesOf)

    // TODO CONSTR-SOLVER-2 Hack! We should visit the argument, but since we don't reduce, we get false positives here.
    case Type.AssocType(_, _, _, _) => Nil
  }

  /**
    * Returns true iff the two types denote the same Boolean function, using the same variables.
    */
  private def sameType(t1: Type, t2: Type)(implicit flix: Flix): Boolean = {
    val tvars = t1.typeVars ++ t2.typeVars

    /**
      * Evaluates the given boolean formula,
      * where `trueVars` are the variables ascribed the value TRUE,
      * and all other variables are ascribed the value FALSE.
      */
    def eval(tpe: Type, trueVars: SortedSet[Type.Var]): CofiniteEffSet = tpe match {
      case Type.Pure => CofiniteEffSet.empty
      case Type.Univ => CofiniteEffSet.universe
      case Type.Cst(TypeConstructor.Effect(sym), _) => CofiniteEffSet.mkSet(sym)
      case Type.Apply(Type.Complement, x, _) => CofiniteEffSet.complement(eval(x, trueVars))
      case Type.Apply(Type.Apply(Type.Union, x1, _), x2, _) => CofiniteEffSet.union(eval(x1, trueVars), eval(x2, trueVars))
      case Type.Apply(Type.Apply(Type.Intersection, x1, _), x2, _) => CofiniteEffSet.intersection(eval(x1, trueVars), eval(x2, trueVars))
      case Type.Apply(Type.Apply(Type.Difference, x1, _), x2, _) => CofiniteEffSet.difference(eval(x1, trueVars), eval(x2, trueVars))
      case Type.Apply(Type.Apply(Type.SymmetricDiff, x1, _), x2, _) => CofiniteEffSet.xor(eval(x1, trueVars), eval(x2, trueVars))
      case tvar: Type.Var => if (trueVars.contains(tvar)) CofiniteEffSet.universe else CofiniteEffSet.empty
      case _ => throw InternalCompilerException(s"unexpected type $tpe", tpe.loc)
    }

    val subsets = tvars.subsets()
    subsets.forall(trueVars => eval(t1, trueVars) == eval(t2, trueVars))
  }

  /**
    * Returns all region variables in the given type `tpe`.
    */
  private def regionVarsOf(tpe: Type): SortedSet[Type.Var] = tpe.typeVars.filter {
    case tvar =>
      val isBool = tvar.sym.kind == Kind.Eff
      val isRegion = tvar.sym.isRegion
      isBool && isRegion
  }

  /**
   * Companion object for [[SharedContext]]
   */
  private object SharedContext {

    /**
     * Returns a fresh shared context.
     */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
   * A global shared context. Must be thread-safe.
   *
   * @param errors the [[TypeError]]s in the AST, if any.
   */
  private case class SharedContext(errors: ConcurrentLinkedQueue[TypeError])
}
