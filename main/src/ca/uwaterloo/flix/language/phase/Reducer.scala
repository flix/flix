/*
 * Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Objectives of this phase:
  * 1. Collect a list of the local parameters of each def
  * 2. Collect a set of all anonymous class / new object expressions
  * 3. Collect a flat set of all types of the program, i.e., if `List[String]` is
  *   in the list, so is `String`.
  *
  * This is actually achieved as two pseudo phases, first 1. and 2. and then 3.
  * operating on a new root.
  */
object Reducer {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Reducer") {
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue)

    val newDefs = ParOps.parMapValues(root.defs)(visitDef)

    val newRoot = root.copy(defs = newDefs, anonClasses = ctx.anonClasses.asScala.toList)
    val types = typesOf(newRoot)
    newRoot.copy(types = types)
  }

  private def visitDef(d: Def)(implicit ctx: SharedContext): Def = d match {
    case Def(ann, mod, sym, cparams, fparams, lparams, pcPoints, exp, tpe, originalTpe, purity, loc) =>
      implicit val lctx: LocalContext = LocalContext.mk()
      assert(lparams.isEmpty, s"Unexpected def local params before Reducer: $lparams")
      val e = visitExpr(exp)
      val ls = lctx.lparams.toList
      Def(ann, mod, sym, cparams, fparams, ls, pcPoints, e, tpe, originalTpe, purity, loc)
  }

  private def visitExpr(exp0: Expr)(implicit lctx: LocalContext, ctx: SharedContext): Expr = exp0 match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      Expr.Var(sym, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val es = exps.map(visitExpr)
      Expr.ApplyClo(e, es, ct, tpe, purity, loc)

    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

    case Expr.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      Expr.ApplySelfTail(sym, formals, es, tpe, purity, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches map {
        case (label, body) => label -> visitExpr(body)
      }
      Expr.Branch(e, bs, tpe, purity, loc)

    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)

    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      lctx.lparams.addOne(LocalParam(sym, exp1.tpe))
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      Expr.Let(sym, e1, e2, tpe, purity, loc)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      lctx.lparams.addOne(LocalParam(varSym, exp1.tpe))
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      Expr.Stmt(e1, e2, tpe, purity, loc)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      lctx.lparams.addOne(LocalParam(sym, MonoType.Region))
      val e = visitExpr(exp)
      Expr.Scope(sym, e, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules map {
        case CatchRule(sym, clazz, body) =>
          lctx.lparams.addOne(LocalParam(sym, MonoType.Object))
          val b = visitExpr(body)
          CatchRule(sym, clazz, b)
      }
      Expr.TryCatch(e, rs, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map {
        case HandlerRule(op, fparams, exp) =>
          val e = visitExpr(exp)
          HandlerRule(op, fparams, e)
      }
      Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      Expr.Do(op, es, tpe, purity, loc)

    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val specs = methods.map {
        case JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val c = visitExpr(clo)
          JvmMethod(ident, fparams, c, retTpe, purity, loc)
      }
      ctx.anonClasses.add(AnonClass(name, clazz, tpe, specs, loc))

      Expr.NewObject(name, clazz, tpe, purity, specs, loc)

  }

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = LocalContext(mutable.ArrayBuffer.empty)
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    * @param lparams the bound variales in the def.
    */
  private case class LocalContext(lparams: mutable.ArrayBuffer[LocalParam])

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private case class SharedContext(anonClasses: ConcurrentLinkedQueue[AnonClass])

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  private def typesOf(root: Root)(implicit flix: Flix): Set[MonoType] = {
    /**
      * Returns the set of types which occur in the given definition `defn0`.
      */
    def visitDefn(defn: Def): Set[MonoType] = {
      // Compute the types in the captured formal parameters.
      val cParamTypes = defn.cparams.foldLeft(Set.empty[MonoType]) {
        case (sacc, FormalParam(_, _, tpe, _)) => sacc + tpe
      }

      // Compute the types in the expression.
      val expressionTypes = visitExp(defn.expr)

      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`

      // Return the types in the defn.
      cParamTypes ++ expressionTypes + defn.arrowType + defn.unboxedType.tpe
    }

    def visitExp(exp0: Expr): Set[MonoType] = (exp0 match {
      case Expr.Cst(_, tpe, _) => Set(tpe)

      case Expr.Var(_, tpe, _) => Set(tpe)

      case Expr.ApplyClo(exp, exps, _, tpe, _, _) => visitExp(exp) ++ visitExps(exps) ++ Set(tpe)

      case Expr.ApplyDef(_, exps, _, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case Expr.ApplySelfTail(_, _, exps, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expr.Branch(exp, branches, _, _, _) =>
        val exps = branches.map {
          case (_, e) => e
        }
        visitExp(exp) ++ visitExps(exps)

      case Expr.JumpTo(_, _, _, _) => Set.empty

      case Expr.Let(_, exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expr.LetRec(_, _, _, exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expr.Stmt(exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expr.Scope(_, exp, _, _, _) => visitExp(exp)

      case Expr.TryCatch(exp, rules, _, _, _) => visitExp(exp) ++ visitExps(rules.map(_.exp))

      case Expr.TryWith(exp, _, rules, _, _, _) => visitExp(exp) ++ visitExps(rules.map(_.exp))

      case Expr.Do(_, exps, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case Expr.NewObject(_, _, _, _, methods, _) => visitExps(methods.map(_.exp))

      case Expr.ApplyAtomic(_, exps, tpe, _, _) => visitExps(exps) + tpe

    }) ++ Set(exp0.tpe)

    def visitExps(exps: Iterable[Expr]): Set[MonoType] = {
      exps.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }
    }

    assert(root.types.isEmpty, s"Unexpected root types before Reducer: ${root.types}")

    // Visit every definition.
    val defTypes = ParOps.parAgg(root.defs.values, Set.empty[MonoType])({
      case (sacc, defn) => sacc ++ visitDefn(defn)
    }, _ ++ _)

    val effectTypes = root.effects.foldLeft(Set.empty[MonoType]) {
      case (sacc, (_, e)) =>
        val opTypes = e.ops.map{
          case op =>
            val paramTypes = op.fparams.map(_.tpe)
            val resType = op.tpe
            val continuationType = MonoType.Object
            val correctedFunctionType: MonoType = MonoType.Arrow(paramTypes :+ continuationType, resType)
            Set(correctedFunctionType)
        }.foldLeft(Set.empty[MonoType]){case (acc, cur) => acc.union(cur)}
        sacc ++ opTypes
    }

    val result = defTypes ++ effectTypes

    nestedTypesOf(Set.empty, Queue.from(result))
  }

  /**
    * Returns all the type components of the given `types`.
    *
    * For example, if the types is just the type `Array[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Array[(Bool, Char, Int)]`
    * (and the types in `acc`).
    */
  @tailrec
  def nestedTypesOf(acc: Set[MonoType], types: Queue[MonoType]): Set[MonoType] = {
    import MonoType._
    types.dequeueOption match {
      case Some((tpe, taskList)) =>
        val taskList1 = tpe match {
          case Unit | Bool | Char | Float32 | Float64 | BigDecimal | Int8 | Int16 |
               Int32 | Int64 | BigInt | String | Regex | Region | Enum(_) |
               RecordEmpty | Native(_) => taskList
          case Array(elm) => taskList.enqueue(elm)
          case Lazy(elm) => taskList.enqueue(elm)
          case Ref(elm) => taskList.enqueue(elm)
          case Tuple(elms) => taskList.enqueueAll(elms)
          case Arrow(targs, tresult) => taskList.enqueueAll(targs).enqueue(tresult)
          case RecordExtend(_, value, rest) => taskList.enqueue(value).enqueue(rest)
        }
        nestedTypesOf(acc + tpe, taskList1)
      case None => acc
    }
  }

}
