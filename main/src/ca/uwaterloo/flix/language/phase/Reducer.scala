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
import ca.uwaterloo.flix.language.ast.shared.ExpPosition
import ca.uwaterloo.flix.language.ast.{Purity, ReducedAst, SimpleType, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * Objectives of this phase:
  *   - Collect a list of the local parameters of each def
  *   - Collect a set of all anonymous class / new object expressions
  *   - Collect a flat set of all types of the program, i.e., if `List[String]` is
  *     in the list, so is `String`.
  *   - Assign a local variable stack index to each variable symbol.
  */
object Reducer {

  def run(root: ReducedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("Reducer") {
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue, new ConcurrentHashMap())

    val newDefs = ParOps.parMapValues(root.defs)(visitDef(_)(root, ctx))
    val defTypes = ctx.defTypes.keys.asScala.toSet

    // This is an over approximation of the types in enums and structs since they are erased.
    val enumTypes = SimpleType.ErasedTypes
    val structTypes = SimpleType.ErasedTypes
    val effectTypes = root.effects.values.toSet.flatMap(typesOfEffect)

    val types = nestedTypesOf(Set.empty, Queue.from(defTypes ++ enumTypes ++ structTypes ++ effectTypes))

    root.copy(defs = newDefs, anonClasses = ctx.anonClasses.asScala.toList, types = types)
  }

  private def visitDef(d: ReducedAst.Def)(implicit root: ReducedAst.Root, ctx: SharedContext): ReducedAst.Def = d match {
    case ReducedAst.Def(ann, mod, sym, cparams, fparams, lparams, _, exp, tpe, unboxedType, loc) =>
      implicit val lctx: LocalContext = LocalContext.mk(exp.purity)
      assert(lparams.isEmpty, s"Unexpected def local params before Reducer: $lparams")

      val e = visitExpr(exp)
      val ls = lctx.lparams.toList

      // Set variable offsets - the variable order is important and must match the assumptions of the backend.
      var varOffset = 0
      for (param <- d.cparams ::: d.fparams ::: ls) {
        varOffset = setOffsetAndIncrement(param.sym, param.tpe, varOffset)
      }

      val pcPoints = lctx.getPcPoints

      // Compute the types in the captured formal parameters.
      val cParamTypes = cparams.foldLeft(Set.empty[SimpleType]) {
        case (sacc, ReducedAst.FormalParam(_, paramTpe)) => sacc + paramTpe
      }

      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`
      ctx.defTypes.put(d.arrowType, ())
      ctx.defTypes.put(unboxedType.tpe, ())
      cParamTypes.foreach(t => ctx.defTypes.put(t, ()))

      ReducedAst.Def(ann, mod, sym, cparams, fparams, ls, pcPoints, e, tpe, unboxedType, loc)
  }

  private def visitExpr(exp0: ReducedAst.Expr)(implicit lctx: LocalContext, root: ReducedAst.Root, ctx: SharedContext): ReducedAst.Expr = {
    ctx.defTypes.put(exp0.tpe, ())
    exp0 match {
      case ReducedAst.Expr.Cst(cst, loc) =>
        ReducedAst.Expr.Cst(cst, loc)

      case ReducedAst.Expr.Var(sym, tpe, loc) =>
        ReducedAst.Expr.Var(sym, tpe, loc)

      case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        ReducedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case ReducedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) lctx.addPcPoint()
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        ReducedAst.Expr.ApplyClo(e1, e2, ct, tpe, purity, loc)

      case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val defn = root.defs(sym)
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(defn.expr.purity)) lctx.addPcPoint()
        val es = exps.map(visitExpr)
        ReducedAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

      case ReducedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
        lctx.addPcPoint()
        val es = exps.map(visitExpr)
        ReducedAst.Expr.ApplyOp(sym, es, tpe, purity, loc)

      case ReducedAst.Expr.ApplySelfTail(sym, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        ReducedAst.Expr.ApplySelfTail(sym, es, tpe, purity, loc)

      case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val e3 = visitExpr(exp3)
        ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val bs = branches map {
          case (label, body) => label -> visitExpr(body)
        }
        ReducedAst.Expr.Branch(e, bs, tpe, purity, loc)

      case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        ReducedAst.Expr.JumpTo(sym, tpe, purity, loc)

      case ReducedAst.Expr.Let(sym, exp1, exp2, loc) =>
        lctx.lparams.addOne(ReducedAst.LocalParam(sym, exp1.tpe))
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        ReducedAst.Expr.Let(sym, e1, e2, loc)

      case ReducedAst.Expr.Stmt(exp1, exp2, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        ReducedAst.Expr.Stmt(e1, e2, loc)

      case ReducedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
        lctx.lparams.addOne(ReducedAst.LocalParam(sym, SimpleType.Region))
        val e = visitExpr(exp)
        ReducedAst.Expr.Region(sym, e, tpe, purity, loc)

      case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules map {
          case ReducedAst.CatchRule(sym, clazz, body) =>
            lctx.lparams.addOne(ReducedAst.LocalParam(sym, SimpleType.Object))
            val b = visitExpr(body)
            ReducedAst.CatchRule(sym, clazz, b)
        }
        ReducedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

      case ReducedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail) lctx.addPcPoint()
        val e = visitExpr(exp)
        val rs = rules.map {
          case ReducedAst.HandlerRule(op, fparams, body) =>
            val b = visitExpr(body)
            ReducedAst.HandlerRule(op, fparams, b)
        }
        ReducedAst.Expr.RunWith(e, effUse, rs, ct, tpe, purity, loc)

      case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
        val specs = methods.map {
          case ReducedAst.JvmMethod(ident, fparams, clo, retTpe, methPurity, methLoc) =>
            val c = visitExpr(clo)
            ReducedAst.JvmMethod(ident, fparams, c, retTpe, methPurity, methLoc)
        }
        ctx.anonClasses.add(ReducedAst.AnonClass(name, clazz, tpe, specs, loc))

        ReducedAst.Expr.NewObject(name, clazz, tpe, purity, specs, loc)

    }
  }

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(purity: Purity): LocalContext = LocalContext(mutable.ArrayBuffer.empty, 0, Purity.isControlImpure(purity))
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    *
    * @param lparams the bound variables in the def.
    */
  private case class LocalContext(lparams: mutable.ArrayBuffer[ReducedAst.LocalParam], private var pcPoints: Int, private val isControlImpure: Boolean) {

    /**
      * Adds n to the private [[pcPoints]] field.
      */
    def addPcPoint(): Unit = {
      if (isControlImpure) {
        pcPoints += 1
      }
    }

    /**
      * Returns the pcPoints field.
      */
    def getPcPoints: Int = pcPoints
  }

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private case class SharedContext(anonClasses: ConcurrentLinkedQueue[ReducedAst.AnonClass], defTypes: ConcurrentHashMap[SimpleType, Unit])

  /**
    * Returns all types contained in the given `Effect`.
    */
  private def typesOfEffect(e: ReducedAst.Effect): Set[SimpleType] = {
    e.ops.toSet.map(extractFunctionType)
  }

  /**
    * Returns the function type based `op` represents.
    */
  private def extractFunctionType(op: ReducedAst.Op): SimpleType = {
    val paramTypes = op.fparams.map(_.tpe)
    val resType = op.tpe
    val continuationType = SimpleType.Object
    val correctedFunctionType = SimpleType.mkArrow(paramTypes :+ continuationType, resType)
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
  private def nestedTypesOf(acc: Set[SimpleType], types: Queue[SimpleType]): Set[SimpleType] = {
    import SimpleType.*
    types.dequeueOption match {
      case Some((tpe, taskList)) =>
        val taskList1 = tpe match {
          case Void | AnyType | Unit | Bool | Char | Float32 | Float64 | BigDecimal | Int8 | Int16 |
               Int32 | Int64 | BigInt | String | Regex | Region | RecordEmpty | ExtensibleEmpty |
               Native(_) | Null => taskList
          case Array(elm) => taskList.enqueue(elm)
          case Lazy(elm) => taskList.enqueue(elm)
          case Tuple(elms) => taskList.enqueueAll(elms)
          case Enum(_, targs) => taskList.enqueueAll(targs)
          case Struct(_, targs) => taskList.enqueueAll(targs)
          case Arrow(targs, tresult) => taskList.enqueueAll(targs).enqueue(tresult)
          case RecordExtend(_, value, rest) => taskList.enqueue(value).enqueue(rest)
          case ExtensibleExtend(_, targs, rest) => taskList.enqueueAll(targs).enqueue(rest)
        }
        nestedTypesOf(acc + tpe, taskList1)
      case None => acc
    }
  }

  /** Assigns a stack offset to `sym` and returns the next available stack offset. */
  private def setOffsetAndIncrement(sym: Symbol.VarSym, tpe: SimpleType, offset: Int): Int = {
    sym.setStackOffset(offset)

    // 64-bit values take up two slots in the offsets.
    val stackSize = tpe match {
      case SimpleType.Float64 => 2
      case SimpleType.Int64 => 2
      case _ => 1
    }
    offset + stackSize
  }

}
