/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.util.collection.ListMap

/**
  * Computes a graph of uses / occurrences of functions and signatures, i.e., [[Symbol.DefnSym]] and [[Symbol.SigSym]].
  *
  * The edge `f -> g` exists if `g` is either a [[Symbol.DefnSym]] or [[Symbol.SigSym]] that occurs
  * in the definition of `f` and `f` is either a function or signature.
  */
object UseGraph {

  /**
    * Computes a graph of uses / occurrences of functions and signatures, i.e., [[Symbol.DefnSym]] and [[Symbol.SigSym]].
    *
    * The edge `f -> g` exists if `g` is either a [[Symbol.DefnSym]] or [[Symbol.SigSym]] that occurs
    * in the definition of `f` and `f` is either a function or signature.
    */
  def computeGraph(root: TypedAst.Root): ListMap[UsedSym, UsedSym] = {
    val defUses = root.defs.values.map(visitDef).foldLeft(ListMap.empty[UsedSym, UsedSym])(_ ++ _)
    val sigUses = root.sigs.values.map(visitSig).foldLeft(ListMap.empty[UsedSym, UsedSym])(_ ++ _)
    defUses ++ sigUses
  }

  /** Returns the all the uses of functions and signatures in `defn0`. */
  private def visitDef(defn0: TypedAst.Def): ListMap[UsedSym, UsedSym] = defn0 match {
    case TypedAst.Def(sym, _, exp, _) => visitExp(exp)(UsedSym.DefnSym(sym))
  }

  /** Returns the all the uses of functions and signatures in `sig0`. */
  private def visitSig(sig0: TypedAst.Sig): ListMap[UsedSym, UsedSym] = sig0 match {
    case TypedAst.Sig(sym, _, exp, _) => exp.map(visitExp(_)(UsedSym.SigSym(sym))).getOrElse(ListMap.empty)
  }

  /** Returns the all the uses of functions and signatures in `exp0`. */
  private def visitExp(exp0: Expr)(implicit sym0: UsedSym): ListMap[UsedSym, UsedSym] = exp0 match {
    case Expr.Cst(_, _, _) =>
      ListMap.empty

    case Expr.Var(_, _, _) =>
      ListMap.empty

    case Expr.Hole(_, _, _, _, _) =>
      ListMap.empty

    case Expr.HoleWithExp(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ApplyDef(SymUse.DefSymUse(sym, _), exps, _, _, _, _, _) =>
      visitExps(exps) + (sym0 -> UsedSym.DefnSym(sym))

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.ApplyOp(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.ApplySig(SymUse.SigSymUse(sym, _), exps, _, _, _, _, _, _) =>
      visitExps(exps) + (sym0 -> UsedSym.SigSym(sym))

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Region(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp)) ++ visitExps(rules.flatMap(_.guard))

    case Expr.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(r => r.exp))

    case Expr.Tag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.ExtTag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.Tuple(exps, _, _, _) =>
      visitExps(exps)

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.StructNew(_, fields, exp, _, _, _) =>
      visitExps(fields.map { case (_, e) => e }) ++ exp.map(visitExp).getOrElse(ListMap.empty)

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.Unsafe(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      visitExps(rules.map(_.exp))

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.InvokeSuperConstructor(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      ListMap.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, constructors, methods, _) =>
      visitExps(constructors.map(_.exp)) ++ visitExps(methods.map(_.exp))

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(selects, optExp, _, _, _) =>
      visitExps(selects.map(_.exp)) ++ visitExps(selects.map(_.chan)) ++ optExp.map(visitExp).getOrElse(ListMap.empty)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      visitExps(frags.map(f => f.exp)) ++ visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(constrs, _, _) =>
      constrs.map(visitFixpointConstraint).foldLeft(ListMap.empty[UsedSym, UsedSym])(_ ++ _)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
      visitExps(exps) ++ visitPredicateHead(select)

    case Expr.FixpointQueryWithSelect(exps1, exp, exps2, from, exps3, _, _, _, _) =>
      visitExps(exps1) ++
        visitExp(exp) ++
        visitExps(exps2) ++
        from.map(visitPredicateBody).foldLeft(ListMap.empty[UsedSym, UsedSym])(_ ++ _) ++
        visitExps(exps3)

    case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      visitExps(exps)

    case Expr.FixpointInjectInto(exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.Error(_, _, _) =>
      ListMap.empty

  }

  /** Returns the all the uses of functions and signatures in `exps`. */
  private def visitExps(exps: List[Expr])(implicit sym0: UsedSym): ListMap[UsedSym, UsedSym] = {
    exps.map(visitExp).foldLeft(ListMap.empty[UsedSym, UsedSym])(_ ++ _)
  }

  /** Returns the all the uses of functions and signatures in `constr0`. */
  private def visitFixpointConstraint(constr0: TypedAst.Constraint)(implicit sym0: UsedSym): ListMap[UsedSym, UsedSym] = constr0 match {
    case TypedAst.Constraint(_, head, body, _) =>
      visitPredicateHead(head) ++ body.map(visitPredicateBody).foldLeft(ListMap.empty[UsedSym, UsedSym])(_ ++ _)
  }

  /** Returns the all the uses of functions and signatures in `head0`. */
  private def visitPredicateHead(head0: TypedAst.Predicate.Head)(implicit sym0: UsedSym): ListMap[UsedSym, UsedSym] = head0 match {
    case TypedAst.Predicate.Head.Atom(_, _, exps, _, _) => visitExps(exps)
  }

  /** Returns the all the uses of functions and signatures in `body0`. */
  private def visitPredicateBody(body0: TypedAst.Predicate.Body)(implicit sym0: UsedSym): ListMap[UsedSym, UsedSym] = body0 match {
    case TypedAst.Predicate.Body.Atom(_, _, _, _, _, _, _) => ListMap.empty
    case TypedAst.Predicate.Body.Functional(_, exp, _) => visitExp(exp)
    case TypedAst.Predicate.Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Represents a used symbol.
    */
  sealed trait UsedSym

  object UsedSym {

    case class DefnSym(sym: Symbol.DefnSym) extends UsedSym

    case class SigSym(sym: Symbol.SigSym) extends UsedSym

  }

}
