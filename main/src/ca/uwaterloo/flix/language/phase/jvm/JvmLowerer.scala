/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.ExpPosition
import ca.uwaterloo.flix.language.ast.{JvmAst, LoweredAst, Purity, SimpleType, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * JVM-specific lowering from [[LoweredAst]] to [[JvmAst]].
  *
  * Objectives:
  *   - Assign JVM local variable offsets.
  *   - Collect a list of anonymous class / new object expressions.
  *   - Collect a flat set of all nested types used by the program.
  *
  * Notes:
  *   - The existing JVM backend depends on `pcPoints` being consistent with the traversal that introduces pc points.
  *   - We cross-check `pcPoints` and `lparams` against the values computed earlier in [[ca.uwaterloo.flix.language.phase.Lowerer]].
  */
object JvmLowerer {

  def run(root: LoweredAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("JvmLowerer") {
    implicit val r: LoweredAst.Root = root
    implicit val ctx: SharedContext = new SharedContext()

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    val types = allTypes(root, ctx.getDefTypes)
    val anonClasses = ctx.getAnonClasses

    JvmAst.Root(defs, enums, structs, effects, types, anonClasses, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /** Returns all types of `root`. */
  private def allTypes(root: LoweredAst.Root, defTypes: Set[SimpleType]): Set[SimpleType] = {
    // This is an over approximation of the types in enums and structs since they are erased.
    val enumTypes = SimpleType.ErasedTypes
    val structTypes = SimpleType.ErasedTypes
    val effectTypes = root.effects.values.toSet.flatMap(typesOfEffect)
    nestedTypesOf(Set.empty, Queue.from(defTypes ++ enumTypes ++ structTypes ++ effectTypes))
  }

  private def visitDef(d: LoweredAst.Def)(implicit root: LoweredAst.Root, ctx: SharedContext): JvmAst.Def = d match {
    case LoweredAst.Def(ann, mod, sym, cparams0, fparams0, lparams0, pcPoints0, exp, tpe, unboxedType0, _, _, _, loc) =>
      implicit val lctx: LocalContext = new LocalContext(isControlImpure = Purity.isControlImpure(exp.purity))

      // It is important to visit parameters and variables in the order the backend expects: cparams, fparams, then lparams.
      val cparams = cparams0.map(visitOffsetFormalParam)
      val fparams = fparams0.map(visitOffsetFormalParam)
      val e = visitExpr(exp)
      // `ls` is initialized based on the context mutation of `visitExpr`
      val ls = lctx.lparams.toList
      val unboxedType = JvmAst.UnboxedType(unboxedType0.tpe)

      // Add all types.
      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`
      ctx.addDefType(d.arrowType)
      ctx.addDefType(unboxedType.tpe)
      // Compute the types in the captured formal parameters.
      for (cp <- cparams) {
        ctx.addDefType(cp.tpe)
      }

      val pcPointsComputed = lctx.getPcPoints
      if (pcPointsComputed != pcPoints0) {
        throw InternalCompilerException(s"Mismatched pcPoints for '$sym'. Expected: $pcPoints0. Actual: $pcPointsComputed.", loc)
      }

      val lparamsComputed = ls.map(lp => LoweredAst.LocalParam(lp.sym, lp.tpe))
      if (lparamsComputed != lparams0) {
        throw InternalCompilerException(s"Mismatched lparams for '$sym'.", loc)
      }

      JvmAst.Def(ann, mod, sym, cparams, fparams, ls, pcPoints0, e, tpe, unboxedType, loc)
  }

  private def visitEnum(enm: LoweredAst.Enum): JvmAst.Enum = {
    val cases = MapOps.mapValues(enm.cases)(visitCase)
    JvmAst.Enum(enm.ann, enm.mod, enm.sym, cases, enm.loc)
  }

  private def visitCase(caze: LoweredAst.Case): JvmAst.Case =
    JvmAst.Case(caze.sym, caze.tpes, caze.loc)

  private def visitStruct(struct: LoweredAst.Struct): JvmAst.Struct = {
    val fields = struct.fields.map(visitStructField)
    JvmAst.Struct(struct.ann, struct.mod, struct.sym, fields, struct.loc)
  }

  private def visitStructField(field: LoweredAst.StructField): JvmAst.StructField =
    JvmAst.StructField(field.sym, field.tpe, field.loc)

  private def visitEffect(effect: LoweredAst.Effect): JvmAst.Effect = {
    val ops = effect.ops.map(visitOp)
    JvmAst.Effect(effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  private def visitOp(op: LoweredAst.Op): JvmAst.Op = {
    val fparams = op.fparams.map(visitFormalParam)
    JvmAst.Op(op.sym, op.ann, op.mod, fparams, op.tpe, op.purity, op.loc)
  }

  private def visitExpr(exp0: LoweredAst.Expr)(implicit lctx: LocalContext, root: LoweredAst.Root, ctx: SharedContext): JvmAst.Expr = {
    ctx.addDefType(exp0.tpe)
    exp0 match {
      case LoweredAst.Expr.Cst(cst, loc) =>
        JvmAst.Expr.Cst(cst, loc)

      case LoweredAst.Expr.NativeImport(_, _, _, loc) =>
        throw InternalCompilerException("Unexpected native import on JVM target.", loc)

      case LoweredAst.Expr.WasmImport(_, _, _, loc) =>
        throw InternalCompilerException("Unexpected wasm import on JVM target.", loc)

      case LoweredAst.Expr.Var(sym, tpe, loc) =>
        val offset = getVarSymReadOffset(sym)
        JvmAst.Expr.Var(sym, offset, tpe, loc)

      case LoweredAst.Expr.ApplyAtomic(op, exps, pcPointId, tpe, purity, loc) =>
        if (pcPointId != 0) {
          throw InternalCompilerException(s"Unexpected pcPointId in ApplyAtomic: $pcPointId.", loc)
        }
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case LoweredAst.Expr.ApplyClo(exp1, exp2, ct, pcPointId, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) {
          val expectedPcPointId = lctx.newPcPointId()
          if (expectedPcPointId != pcPointId) {
            throw InternalCompilerException(s"Mismatched pcPointId in ApplyClo. Expected: $expectedPcPointId. Actual: $pcPointId.", loc)
          }
        } else if (pcPointId != 0) {
          throw InternalCompilerException(s"Unexpected pcPointId in ApplyClo: $pcPointId.", loc)
        }
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.ApplyClo(e1, e2, ct, pcPointId, tpe, purity, loc)

      case LoweredAst.Expr.ApplyDef(sym, exps, ct, pcPointId, tpe, purity, loc) =>
        val defn = root.defs(sym)
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(defn.exp.purity)) {
          val expectedPcPointId = lctx.newPcPointId()
          if (expectedPcPointId != pcPointId) {
            throw InternalCompilerException(s"Mismatched pcPointId in ApplyDef. Expected: $expectedPcPointId. Actual: $pcPointId.", loc)
          }
        } else if (pcPointId != 0) {
          throw InternalCompilerException(s"Unexpected pcPointId in ApplyDef: $pcPointId.", loc)
        }
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyDef(sym, es, ct, pcPointId, tpe, purity, loc)

      case LoweredAst.Expr.ApplyOp(sym, exps, pcPointId, tpe, purity, loc) =>
        val expectedPcPointId = lctx.newPcPointId()
        if (expectedPcPointId != pcPointId) {
          throw InternalCompilerException(s"Mismatched pcPointId in ApplyOp. Expected: $expectedPcPointId. Actual: $pcPointId.", loc)
        }
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyOp(sym, es, pcPointId, tpe, purity, loc)

      case LoweredAst.Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
        val es = actuals.map(visitExpr)
        JvmAst.Expr.ApplySelfTail(sym, es, tpe, purity, loc)

      case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val e3 = visitExpr(exp3)
        JvmAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case LoweredAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val bs = branches.map {
          case (label, body) => label -> visitExpr(body)
        }
        JvmAst.Expr.Branch(e, bs, tpe, purity, loc)

      case LoweredAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        JvmAst.Expr.JumpTo(sym, tpe, purity, loc)

      case LoweredAst.Expr.Let(sym, exp1, exp2, loc) =>
        val offset = lctx.assignOffset(sym, exp1.tpe)
        lctx.lparams.addOne(JvmAst.LocalParam(sym, offset, exp1.tpe))
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Let(sym, offset, e1, e2, loc)

      case LoweredAst.Expr.Stmt(exp1, exp2, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Stmt(e1, e2, loc)

      case LoweredAst.Expr.Region(sym, exp, pcPointId, tpe, purity, loc) =>
        if (pcPointId != 0) {
          throw InternalCompilerException(s"Unexpected pcPointId in Region: $pcPointId.", loc)
        }
        val offset = lctx.assignOffset(sym, SimpleType.Region)
        lctx.lparams.addOne(JvmAst.LocalParam(sym, offset, SimpleType.Region))
        val e = visitExpr(exp)
        JvmAst.Expr.Region(sym, offset, e, tpe, purity, loc)

      case LoweredAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules.map {
          case LoweredAst.CatchRule(sym, catchTpe, body) =>
            val offset = lctx.assignOffset(sym, SimpleType.Object)
            lctx.lparams.addOne(JvmAst.LocalParam(sym, offset, SimpleType.Object))
            val b = visitExpr(body)
            JvmAst.CatchRule(sym, offset, catchTpe, b)
        }
        JvmAst.Expr.TryCatch(e, rs, tpe, purity, loc)

      case LoweredAst.Expr.RunWith(exp, effUse, rules, ct, pcPointId, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail) {
          val expectedPcPointId = lctx.newPcPointId()
          if (expectedPcPointId != pcPointId) {
            throw InternalCompilerException(s"Mismatched pcPointId in RunWith. Expected: $expectedPcPointId. Actual: $pcPointId.", loc)
          }
        } else if (pcPointId != 0) {
          throw InternalCompilerException(s"Unexpected pcPointId in RunWith: $pcPointId.", loc)
        }
        val e = visitExpr(exp)
        val rs = rules.map {
          case LoweredAst.HandlerRule(op, fparams, body) =>
            val b = visitExpr(body)
            JvmAst.HandlerRule(op, fparams.map(visitFormalParam), b)
        }
        JvmAst.Expr.RunWith(e, effUse, rs, ct, pcPointId, tpe, purity, loc)

      case LoweredAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
        val specs = methods.map {
          case LoweredAst.JvmMethod(ident, fparams, clo, retTpe, methPurity, methLoc) =>
            val c = visitExpr(clo)
            JvmAst.JvmMethod(ident, fparams.map(visitFormalParam), c, retTpe, methPurity, methLoc)
        }
        ctx.addAnonClass(JvmAst.AnonClass(name, clazz, tpe, specs, loc))
        JvmAst.Expr.NewObject(name, clazz, tpe, purity, specs, loc)
    }
  }

  /** returns the offset of `sym` defined in `lctx`. */
  private def getVarSymReadOffset(sym: Symbol.VarSym)(implicit lctx: LocalContext): Int =
    lctx.getOffset(sym)

  /** Assigns the next offset to `fp`, mutating `lctx`. */
  private def visitOffsetFormalParam(fp: LoweredAst.FormalParam)(implicit lctx: LocalContext): JvmAst.OffsetFormalParam = {
    val offset = lctx.assignOffset(fp.sym, fp.tpe)
    JvmAst.OffsetFormalParam(fp.sym, offset, fp.tpe)
  }

  private def visitFormalParam(fp: LoweredAst.FormalParam): JvmAst.FormalParam =
    JvmAst.FormalParam(fp.sym, fp.tpe)

  /**
    * A local non-shared context. Does not need to be thread-safe.
    */
  private class LocalContext(private val isControlImpure: Boolean) {

    val lparams: mutable.ArrayBuffer[JvmAst.LocalParam] = mutable.ArrayBuffer.empty

    private var pcPoints: Int = 0

    private var nextVarOffset: Int = 0

    private val offsets: mutable.Map[Symbol.VarSym, Int] = mutable.HashMap.empty

    /**
      * Adds n to the private [[pcPoints]] field.
      */
    def newPcPointId(): Int = {
      if (isControlImpure) {
        pcPoints += 1
        pcPoints
      } else {
        0
      }
    }

    /**
      * Returns the pcPoints field.
      */
    def getPcPoints: Int = pcPoints

    /** Assigns the next offset to `sym` and returns it. */
    def assignOffset(sym: Symbol.VarSym, tpe: SimpleType): Int = {
      if (offsets.contains(sym)) throw InternalCompilerException(s"Already assigned offset to '$sym'", sym.loc)

      val offset = nextVarOffsetAndIncrement(tpe)
      offsets.put(sym, offset)
      offset
    }

    /** Returns the offset of `sym`, throwing [[InternalCompilerException]] if not found. */
    def getOffset(sym: Symbol.VarSym): Int = {
      offsets.get(sym) match {
        case Some(offset) => offset
        case None => throw InternalCompilerException(s"No offset found for '$sym'", sym.loc)
      }
    }

    /** Returns the next available offset and increments the running offset. */
    private def nextVarOffsetAndIncrement(tpe: SimpleType): Int = {
      val res = nextVarOffset
      val offset = tpe match {
        case SimpleType.Float64 => 2
        case SimpleType.Int64 => 2
        case _ => 1
      }
      nextVarOffset += offset
      res
    }

  }

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private final class SharedContext {

    private val anonClasses: ConcurrentLinkedQueue[JvmAst.AnonClass] = new ConcurrentLinkedQueue()

    def addAnonClass(clazz: JvmAst.AnonClass): Unit =
      anonClasses.add(clazz)

    def getAnonClasses: List[JvmAst.AnonClass] =
      anonClasses.asScala.toList

    private val defTypes: ConcurrentHashMap[SimpleType, Unit] = new ConcurrentHashMap()

    def addDefType(tpe: SimpleType): Unit =
      defTypes.putIfAbsent(tpe, ())

    def getDefTypes: Set[SimpleType] =
      defTypes.keySet.asScala.toSet
  }

  /**
    * Returns all types contained in the given `Effect`.
    */
  private def typesOfEffect(e: LoweredAst.Effect): Set[SimpleType] = {
    e.ops.toSet.map(extractFunctionType)
  }

  /**
    * Returns the function type based `op` represents.
    */
  private def extractFunctionType(op: LoweredAst.Op): SimpleType = {
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
               Int32 | Int64 | BigInt | String | Regex | StringBuilderHandle | RegexMatcher | ChannelHandle | ReentrantLockHandle | ConditionHandle | CyclicBarrierHandle | CountDownLatchHandle | SemaphoreHandle | Region | RecordEmpty | ExtensibleEmpty |
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

}
