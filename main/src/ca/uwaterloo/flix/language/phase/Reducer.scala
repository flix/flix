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
import ca.uwaterloo.flix.language.ast.Ast.ExpPosition
import ca.uwaterloo.flix.language.ast.{MonoType, Purity}
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
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
  */
object Reducer {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Reducer") {
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue, new ConcurrentHashMap())

    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    val defTypes = ctx.defTypes.keys.asScala.toSet

    val effectTypes = root.effects.values.toSet.flatMap(typesOfEffect)

    val types = nestedTypesOf(Set.empty, Queue.from(defTypes ++ effectTypes))

    root.copy(defs = newDefs, anonClasses = ctx.anonClasses.asScala.toList, types = types)
  }

  private def visitDef(d: Def)(implicit ctx: SharedContext): Def = d match {
    case Def(ann, mod, sym, cparams, fparams, lparams, _, exp, tpe, unboxedType, purity, loc) =>
      implicit val lctx: LocalContext = LocalContext.mk()
      assert(lparams.isEmpty, s"Unexpected def local params before Reducer: $lparams")
      val e = visitExpr(exp)
      val ls = lctx.lparams.toList
      val pcPoints = lctx.pcPoints

      // Compute the types in the captured formal parameters.
      val cParamTypes = cparams.foldLeft(Set.empty[MonoType]) {
        case (sacc, FormalParam(_, _, tpe, _)) => sacc + tpe
      }

      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`
      ctx.defTypes.put(d.arrowType, ())
      ctx.defTypes.put(unboxedType.tpe, ())
      cParamTypes.foreach(t => ctx.defTypes.put(t, ()))

      Def(ann, mod, sym, cparams, fparams, ls, pcPoints, e, tpe, unboxedType, purity, loc)
  }

  private def visitExpr(exp0: Expr)(implicit lctx: LocalContext, ctx: SharedContext): Expr = {
    ctx.defTypes.put(exp0.tpe, ())
    exp0 match {
      case Expr.Cst(cst, tpe, loc) =>
        Expr.Cst(cst, tpe, loc)

      case Expr.Var(sym, tpe, loc) =>
        Expr.Var(sym, tpe, loc)

      case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) lctx.pcPoints += 1
        val e = visitExpr(exp)
        val es = exps.map(visitExpr)
        Expr.ApplyClo(e, es, ct, tpe, purity, loc)

      case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) lctx.pcPoints += 1
        val es = exps.map(visitExpr)
        Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

      case Expr.ApplySelfTail(sym, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        Expr.ApplySelfTail(sym, es, tpe, purity, loc)

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

      case Expr.TryWith(exp, effUse, rules, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail) lctx.pcPoints += 1
        val e = visitExpr(exp)
        val rs = rules.map {
          case HandlerRule(op, fparams, exp) =>
            val e = visitExpr(exp)
            HandlerRule(op, fparams, e)
        }
        Expr.TryWith(e, effUse, rs, ct, tpe, purity, loc)

      case Expr.Do(op, exps, tpe, purity, loc) =>
        lctx.pcPoints += 1
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
  }

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = LocalContext(mutable.ArrayBuffer.empty, 0)
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    *
    * @param lparams the bound variables in the def.
    */
  private case class LocalContext(lparams: mutable.ArrayBuffer[LocalParam], var pcPoints: Int)

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private case class SharedContext(anonClasses: ConcurrentLinkedQueue[AnonClass], defTypes: ConcurrentHashMap[MonoType, Unit])

  /**
    * Returns all types contained in the given `Effect`.
    */
  private def typesOfEffect(e: Effect): Set[MonoType] = e.ops.toSet.map {
    op: Op =>
      val paramTypes = op.fparams.map(_.tpe)
      val resType = op.tpe
      val continuationType = MonoType.Object
      val correctedFunctionType = MonoType.Arrow(paramTypes :+ continuationType, resType)
      correctedFunctionType
  }

  /**
    * Returns all the type components of the given `types`.
    *
    * For example, if the types is just the type `Array[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Array[(Bool, Char, Int)]`
    * (and the types in `acc`).
    */
  @tailrec
  private def nestedTypesOf(acc: Set[MonoType], types: Queue[MonoType]): Set[MonoType] = {
    import MonoType._
    types.dequeueOption match {
      case Some((tpe, taskList)) =>
        val taskList1 = tpe match {
          case Void | AnyType | Unit | Bool | Char | Float32 | Float64 | BigDecimal | Int8 | Int16 |
               Int32 | Int64 | BigInt | String | Regex | Region | Enum(_) | RecordEmpty |
               Native(_) => taskList
          case Array(elm) => taskList.enqueue(elm)
          case Lazy(elm) => taskList.enqueue(elm)
          case Ref(elm) => taskList.enqueue(elm)
          case Tuple(elms) => taskList.enqueueAll(elms)
          case Struct(_, _, targs) => taskList.enqueueAll(targs)
          case Arrow(targs, tresult) => taskList.enqueueAll(targs).enqueue(tresult)
          case RecordExtend(_, value, rest) => taskList.enqueue(value).enqueue(rest)
        }
        nestedTypesOf(acc + tpe, taskList1)
      case None => acc
    }
  }

}
