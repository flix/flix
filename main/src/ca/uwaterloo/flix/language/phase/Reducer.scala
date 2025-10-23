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
import ca.uwaterloo.flix.language.ast.{JvmAst, Purity, ReducedAst, SimpleType, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}
import ca.uwaterloo.flix.util.collection.MapOps

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

  def run(root: ReducedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("Reducer") {
    implicit val r: ReducedAst.Root = root
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue, new ConcurrentHashMap())

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    val types = allTypes(root)
    val anonClasses = ctx.anonClasses.asScala.toList

    JvmAst.Root(defs, enums, structs, effects, types, anonClasses, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /**
    * Returns all types of `root`.
    *
    * Must be called after all expressions have been visited
    */
  private def allTypes(root: ReducedAst.Root)(implicit ctx: SharedContext): Set[SimpleType] = {
    val defTypes = ctx.defTypes.keys.asScala.toSet
    // This is an over approximation of the types in enums and structs since they are erased.
    val enumTypes = SimpleType.ErasedTypes
    val structTypes = SimpleType.ErasedTypes
    val effectTypes = root.effects.values.toSet.flatMap(typesOfEffect)
    nestedTypesOf(Set.empty, Queue.from(defTypes ++ enumTypes ++ structTypes ++ effectTypes))
  }

  private def visitDef(d: ReducedAst.Def)(implicit root: ReducedAst.Root, ctx: SharedContext): JvmAst.Def = d match {
    case ReducedAst.Def(ann, mod, sym, cparams0, fparams0, exp, tpe, unboxedType0, loc) =>
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
      ctx.defTypes.put(d.arrowType, ())
      ctx.defTypes.put(unboxedType.tpe, ())
      // Compute the types in the captured formal parameters.
      for (cp <- cparams) {
        ctx.defTypes.put(cp.tpe, ())
      }

      val pcPoints = lctx.getPcPoints

      JvmAst.Def(ann, mod, sym, cparams, fparams, ls, pcPoints, e, tpe, unboxedType, loc)
  }

  private def visitEnum(enm: ReducedAst.Enum): JvmAst.Enum = {
    val tparams = enm.tparams.map(visitTypeParam)
    val cases = MapOps.mapValues(enm.cases)(visitCase)
    JvmAst.Enum(enm.ann, enm.mod, enm.sym, tparams, cases, enm.loc)
  }

  private def visitCase(caze: ReducedAst.Case): JvmAst.Case =
    JvmAst.Case(caze.sym, caze.tpes, caze.loc)

  private def visitStruct(struct: ReducedAst.Struct): JvmAst.Struct = {
    val tparams = struct.tparams.map(visitTypeParam)
    val fields = struct.fields.map(visitStructField)
    JvmAst.Struct(struct.ann, struct.mod, struct.sym, tparams, fields, struct.loc)
  }

  private def visitStructField(field: ReducedAst.StructField): JvmAst.StructField =
    JvmAst.StructField(field.sym, field.tpe, field.loc)

  private def visitEffect(effect: ReducedAst.Effect): JvmAst.Effect = {
    val ops = effect.ops.map(visitOp)
    JvmAst.Effect(effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  private def visitOp(op: ReducedAst.Op): JvmAst.Op = {
    val fparams = op.fparams.map(visitFormalParam)
    JvmAst.Op(op.sym, op.ann, op.mod, fparams, op.tpe, op.purity, op.loc)
  }

  private def visitExpr(exp0: ReducedAst.Expr)(implicit lctx: LocalContext, root: ReducedAst.Root, ctx: SharedContext): JvmAst.Expr = {
    ctx.defTypes.put(exp0.tpe, ())
    exp0 match {
      case ReducedAst.Expr.Cst(cst, loc) =>
        JvmAst.Expr.Cst(cst, loc)

      case ReducedAst.Expr.Var(sym, tpe, loc) =>
        val offset = getVarSymReadOffset(sym)
        JvmAst.Expr.Var(offset, sym, tpe, loc)

      case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case ReducedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) lctx.addPcPoint()
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.ApplyClo(e1, e2, ct, tpe, purity, loc)

      case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val defn = root.defs(sym)
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(defn.exp.purity)) lctx.addPcPoint()
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

      case ReducedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
        lctx.addPcPoint()
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyOp(sym, es, tpe, purity, loc)

      case ReducedAst.Expr.ApplySelfTail(sym, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplySelfTail(sym, es, tpe, purity, loc)

      case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val e3 = visitExpr(exp3)
        JvmAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val bs = branches map {
          case (label, body) => label -> visitExpr(body)
        }
        JvmAst.Expr.Branch(e, bs, tpe, purity, loc)

      case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        JvmAst.Expr.JumpTo(sym, tpe, purity, loc)

      case ReducedAst.Expr.Let(sym, exp1, exp2, loc) =>
        val offset = lctx.assignOffset(sym, exp1.tpe)
        lctx.lparams.addOne(JvmAst.LocalParam(offset, sym, exp1.tpe))
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Let(offset, sym, e1, e2, loc)

      case ReducedAst.Expr.Stmt(exp1, exp2, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Stmt(e1, e2, loc)

      case ReducedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
        val offset = lctx.assignOffset(sym, SimpleType.Region)
        lctx.lparams.addOne(JvmAst.LocalParam(offset, sym, SimpleType.Region))
        val e = visitExpr(exp)
        JvmAst.Expr.Region(offset, sym, e, tpe, purity, loc)

      case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules map {
          case ReducedAst.CatchRule(sym, clazz, body) =>
            val offset = lctx.assignOffset(sym, SimpleType.Object)
            lctx.lparams.addOne(JvmAst.LocalParam(offset, sym, SimpleType.Object))
            val b = visitExpr(body)
            JvmAst.CatchRule(offset, sym, clazz, b)
        }
        JvmAst.Expr.TryCatch(e, rs, tpe, purity, loc)

      case ReducedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail) lctx.addPcPoint()
        val e = visitExpr(exp)
        val rs = rules.map {
          case ReducedAst.HandlerRule(op, fparams, body) =>
            val b = visitExpr(body)
            JvmAst.HandlerRule(op, fparams.map(visitFormalParam), b)
        }
        JvmAst.Expr.RunWith(e, effUse, rs, ct, tpe, purity, loc)

      case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
        val specs = methods.map {
          case ReducedAst.JvmMethod(ident, fparams, clo, retTpe, methPurity, methLoc) =>
            val c = visitExpr(clo)
            JvmAst.JvmMethod(ident, fparams.map(visitFormalParam), c, retTpe, methPurity, methLoc)
        }
        ctx.anonClasses.add(JvmAst.AnonClass(name, clazz, tpe, specs, loc))

        JvmAst.Expr.NewObject(name, clazz, tpe, purity, specs, loc)

    }
  }

  /** returns the offset of `sym` defined in `lctx`. */
  private def getVarSymReadOffset(sym: Symbol.VarSym)(implicit lctx: LocalContext): Int =
    lctx.getOffset(sym)

  /** Assigns the next offset to `fp`, mutating `lctx`. */
  private def visitOffsetFormalParam(fp: ReducedAst.FormalParam)(implicit lctx: LocalContext): JvmAst.OffsetFormalParam = {
    val offset = lctx.assignOffset(fp.sym, fp.tpe)
    JvmAst.OffsetFormalParam(offset, fp.sym, fp.tpe)
  }

  private def visitFormalParam(fp: ReducedAst.FormalParam): JvmAst.FormalParam =
    JvmAst.FormalParam(fp.sym, fp.tpe)

  private def visitTypeParam(tp: ReducedAst.TypeParam): JvmAst.TypeParam =
    JvmAst.TypeParam(tp.name, tp.sym)

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
    def addPcPoint(): Unit = {
      if (isControlImpure) {
        pcPoints += 1
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
  private case class SharedContext(anonClasses: ConcurrentLinkedQueue[JvmAst.AnonClass], defTypes: ConcurrentHashMap[SimpleType, Unit])

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

}
