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
import ca.uwaterloo.flix.language.ast.{ErasedAst, JvmAst, Purity, SimpleType, Symbol}
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

  def run(root: ErasedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("Reducer") {
    implicit val r: ErasedAst.Root = root
    implicit val ctx: SharedContext = new SharedContext()

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    val types = allTypes(effects, ctx.getDefTypes)
    val erasedFunctionTypes = ctx.getErasedFunctionTypes
    val anonClasses = ctx.getAnonClasses

    JvmAst.Root(defs, enums, structs, effects, erasedFunctionTypes, types, anonClasses, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /** Returns all types of `root`. */
  private def allTypes(effects: Map[Symbol.EffSym, JvmAst.Effect], defTypes: Set[SimpleType]): Set[SimpleType] = {
    // This is an over approximation of the types in enums and structs since they are erased.
    val enumTypes = SimpleType.ErasedTypes
    val structTypes = SimpleType.ErasedTypes
    val effectTypes = effects.values.toSet.flatMap(typesOfEffect)
    nestedTypesOf(Set.empty, Queue.from(defTypes ++ enumTypes ++ structTypes ++ effectTypes))
  }

  private def visitDef(d: ErasedAst.Def)(implicit root: ErasedAst.Root, ctx: SharedContext): JvmAst.Def = d match {
    case ErasedAst.Def(ann, mod, sym, cparams0, fparams0, exp, tpe, unboxedType0, loc) =>
      implicit val lctx: LocalContext = new LocalContext(isControlImpure = Purity.isControlImpure(exp.purity))

      // It is important to visit parameters and variables in the order the backend expects: cparams, fparams, then lparams.
      val cparams = cparams0.map(visitOffsetFormalParam)
      val fparams = fparams0.map(visitOffsetFormalParam)
      val e = visitExpr(exp)
      // `ls` is initialized based on the context mutation of `visitExpr`
      val ls = lctx.lparams.toList
      val unboxedType = JvmAst.UnboxedType(visitType(unboxedType0.tpe))

      // Add all types.
      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`
      ctx.addDefType(d.arrowType)
      ctx.addArrow(d.arrowType)
      ctx.addDefType(unboxedType.tpe)
      // Compute the types in the captured formal parameters.
      for (cp <- cparams) {
        ctx.addDefType(cp.tpe)
      }

      val pcPoints = lctx.getPcPoints

      JvmAst.Def(ann, mod, sym, cparams, fparams, ls, pcPoints, e, tpe, unboxedType, loc)
  }

  private def visitEnum(enm: ErasedAst.Enum): JvmAst.Enum = {
    val cases = MapOps.mapValues(enm.cases)(visitCase)
    JvmAst.Enum(enm.ann, enm.mod, enm.sym, cases, enm.loc)
  }

  private def visitCase(caze: ErasedAst.Case): JvmAst.Case =
    JvmAst.Case(caze.sym, caze.tpes, caze.loc)

  private def visitStruct(struct: ErasedAst.Struct): JvmAst.Struct = {
    val fields = struct.fields.map(visitStructField)
    JvmAst.Struct(struct.ann, struct.mod, struct.sym, fields, struct.loc)
  }

  private def visitStructField(field: ErasedAst.StructField): JvmAst.StructField =
    JvmAst.StructField(field.sym, field.tpe, field.loc)

  private def visitEffect(effect: ErasedAst.Effect)(implicit ctx: SharedContext): JvmAst.Effect = {
    val ops = effect.ops.map(visitOp)
    JvmAst.Effect(effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  private def visitOp(op: ErasedAst.Op)(implicit ctx: SharedContext): JvmAst.Op = {
    val fparams = op.fparams.map(visitFormalParam)
    val t = visitType(op.tpe)
    JvmAst.Op(op.sym, op.ann, op.mod, fparams, t, op.purity, op.loc)
  }

  private def visitExpr(exp0: ErasedAst.Expr)(implicit lctx: LocalContext, root: ErasedAst.Root, ctx: SharedContext): JvmAst.Expr = {
    ctx.addDefType(exp0.tpe)
    exp0 match {
      case ErasedAst.Expr.Cst(cst, loc) =>
        // Since `visitType(tpe) = tpe` for all types of constants, its okay not to store the new type.
        visitType(exp0.tpe)
        JvmAst.Expr.Cst(cst, loc)

      case ErasedAst.Expr.Var(sym, tpe, loc) =>
        val offset = getVarSymReadOffset(sym)
        val t = visitType(tpe)
        JvmAst.Expr.Var(sym, offset, t, loc)

      case ErasedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        val t = visitType(tpe)
        JvmAst.Expr.ApplyAtomic(op, es, t, purity, loc)

      case ErasedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) lctx.addPcPoint()
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val t = visitType(tpe)
        JvmAst.Expr.ApplyClo(e1, e2, ct, t, purity, loc)

      case ErasedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val defn = root.defs(sym)
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(defn.exp.purity)) lctx.addPcPoint()
        val es = exps.map(visitExpr)
        val t = visitType(tpe)
        JvmAst.Expr.ApplyDef(sym, es, ct, t, purity, loc)

      case ErasedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
        lctx.addPcPoint()
        val es = exps.map(visitExpr)
        val t = visitType(tpe)
        JvmAst.Expr.ApplyOp(sym, es, t, purity, loc)

      case ErasedAst.Expr.ApplySelfTail(sym, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        val t = visitType(tpe)
        JvmAst.Expr.ApplySelfTail(sym, es, t, purity, loc)

      case ErasedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val e3 = visitExpr(exp3)
        val t = visitType(tpe)
        JvmAst.Expr.IfThenElse(e1, e2, e3, t, purity, loc)

      case ErasedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val bs = branches map {
          case (label, body) => label -> visitExpr(body)
        }
        val t = visitType(tpe)
        JvmAst.Expr.Branch(e, bs, t, purity, loc)

      case ErasedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        val t = visitType(tpe)
        JvmAst.Expr.JumpTo(sym, t, purity, loc)

      case ErasedAst.Expr.Let(sym, exp1, exp2, loc) =>
        val offset = lctx.assignOffset(sym, exp1.tpe)
        lctx.lparams.addOne(JvmAst.LocalParam(sym, offset, visitType(exp1.tpe)))
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Let(sym, offset, e1, e2, loc)

      case ErasedAst.Expr.Stmt(exp1, exp2, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Stmt(e1, e2, loc)

      case ErasedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
        val offset = lctx.assignOffset(sym, SimpleType.Region)
        lctx.lparams.addOne(JvmAst.LocalParam(sym, offset, SimpleType.Region))
        val e = visitExpr(exp)
        val t = visitType(tpe)
        JvmAst.Expr.Region(sym, offset, e, t, purity, loc)

      case ErasedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules map {
          case ErasedAst.CatchRule(sym, clazz, body) =>
            val offset = lctx.assignOffset(sym, SimpleType.Object)
            lctx.lparams.addOne(JvmAst.LocalParam(sym, offset, SimpleType.Object))
            val b = visitExpr(body)
            JvmAst.CatchRule(sym, offset, clazz, b)
        }
        val t = visitType(tpe)
        JvmAst.Expr.TryCatch(e, rs, t, purity, loc)

      case ErasedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail) lctx.addPcPoint()
        val e = visitExpr(exp)
        val rs = rules.map {
          case ErasedAst.HandlerRule(op, fparams, body) =>
            val b = visitExpr(body)
            JvmAst.HandlerRule(op, fparams.map(visitFormalParam), b)
        }
        val t = visitType(tpe)
        JvmAst.Expr.RunWith(e, effUse, rs, ct, t, purity, loc)

      case ErasedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
        val specs = methods.map {
          case ErasedAst.JvmMethod(ident, fparams, clo, retTpe, methPurity, methLoc) =>
            val c = visitExpr(clo)
            val rt = visitType(retTpe)
            JvmAst.JvmMethod(ident, fparams.map(visitFormalParam), c, rt, methPurity, methLoc)
        }
        val t = visitType(tpe)
        ctx.addAnonClass(JvmAst.AnonClass(name, clazz, t, specs, loc))

        JvmAst.Expr.NewObject(name, clazz, t, purity, specs, loc)

    }
  }

  private def visitType(tpe0: SimpleType)(implicit ctx: SharedContext): SimpleType = tpe0 match {
    case SimpleType.Void => SimpleType.Void
    case SimpleType.AnyType => SimpleType.AnyType
    case SimpleType.Unit => SimpleType.Unit
    case SimpleType.Bool => SimpleType.Bool
    case SimpleType.Char => SimpleType.Char
    case SimpleType.Float32 => SimpleType.Float32
    case SimpleType.Float64 => SimpleType.Float64
    case SimpleType.BigDecimal => SimpleType.BigDecimal
    case SimpleType.Int8 => SimpleType.Int8
    case SimpleType.Int16 => SimpleType.Int16
    case SimpleType.Int32 => SimpleType.Int32
    case SimpleType.Int64 => SimpleType.Int64
    case SimpleType.BigInt => SimpleType.BigInt
    case SimpleType.String => SimpleType.String
    case SimpleType.Regex => SimpleType.Regex
    case SimpleType.Region => SimpleType.Region
    case SimpleType.Null => SimpleType.Null
    case SimpleType.Array(tpe) => SimpleType.Array(visitType(tpe))
    case SimpleType.Lazy(tpe) => SimpleType.Lazy(visitType(tpe))
    case SimpleType.Tuple(tpes) => SimpleType.Tuple(tpes.map(visitType))
    case SimpleType.Enum(sym, targs) => SimpleType.Enum(sym, targs.map(visitType))
    case SimpleType.Struct(sym, targs) => SimpleType.Struct(sym, targs.map(visitType))
    case SimpleType.Arrow(targs, result) =>
      val tpe = SimpleType.Arrow(targs.map(visitType), visitType(result))
      ctx.addArrow(tpe)
      tpe
    case SimpleType.RecordEmpty => SimpleType.RecordEmpty
    case SimpleType.RecordExtend(label, value, rest) => SimpleType.RecordExtend(label, visitType(value), visitType(rest))
    case SimpleType.ExtensibleEmpty => SimpleType.ExtensibleEmpty
    case SimpleType.ExtensibleExtend(cons, tpes, rest) => SimpleType.ExtensibleExtend(cons, tpes.map(visitType), visitType(rest))
    case SimpleType.Native(clazz) => SimpleType.Native(clazz)
  }

  /** returns the offset of `sym` defined in `lctx`. */
  private def getVarSymReadOffset(sym: Symbol.VarSym)(implicit lctx: LocalContext): Int =
    lctx.getOffset(sym)

  /** Assigns the next offset to `fp`, mutating `lctx`. */
  private def visitOffsetFormalParam(fp: ErasedAst.FormalParam)(implicit lctx: LocalContext, ctx: SharedContext): JvmAst.OffsetFormalParam = {
    val t = visitType(fp.tpe)
    val offset = lctx.assignOffset(fp.sym, t)
    JvmAst.OffsetFormalParam(fp.sym, offset, t)
  }

  private def visitFormalParam(fp: ErasedAst.FormalParam)(implicit ctx: SharedContext): JvmAst.FormalParam = {
    val t = visitType(fp.tpe)
    JvmAst.FormalParam(fp.sym, t)
  }

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
  private final class SharedContext {

    private val anonClasses: ConcurrentLinkedQueue[JvmAst.AnonClass] = new ConcurrentLinkedQueue()

    private val defTypes: ConcurrentHashMap[SimpleType, Unit] = new ConcurrentHashMap()

    private val erasedFunctionTypes: ConcurrentHashMap[SimpleType.Arrow, Unit] = new ConcurrentHashMap()

    def addAnonClass(clazz: JvmAst.AnonClass): Unit =
      anonClasses.add(clazz)

    def getAnonClasses: List[JvmAst.AnonClass] =
      anonClasses.asScala.toList

    def addDefType(tpe: SimpleType): Unit =
      defTypes.putIfAbsent(tpe, ())

    def getDefTypes: Set[SimpleType] =
      defTypes.keySet.asScala.toSet

    /** The erased arrow type of `tpe` is added. */
    def addArrow(tpe: SimpleType.Arrow): Unit =
      erasedFunctionTypes.putIfAbsent(SimpleType.Arrow(tpe.targs.map(SimpleType.erase), SimpleType.erase(tpe.result)), ())

    def getErasedFunctionTypes: Set[SimpleType.Arrow] =
      erasedFunctionTypes.keySet.asScala.toSet

  }

  /**
    * Returns all types contained in the given `Effect`.
    */
  private def typesOfEffect(e: JvmAst.Effect): Set[SimpleType] = {
    e.ops.toSet.map(extractFunctionType)
  }

  /**
    * Returns the function type based `op` represents.
    */
  private def extractFunctionType(op: JvmAst.Op): SimpleType = {
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
