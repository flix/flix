/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.{ExpPosition, JMethod}
import ca.uwaterloo.flix.language.ast.{AtomicOp, ErasedAst, JvmAst, Purity, SimpleType, Symbol}
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

  /** Reduces `root`, assigning variable offsets and collecting pc points, anonymous classes, and types. */
  def run(root: ErasedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("Reducer") {
    implicit val r: ErasedAst.Root = root
    implicit val sctx: SharedContext = new SharedContext()

    val defs = ParOps.parMapValues(root.defs)(defn => flix.profile(defn.sym, defn.loc)(visitDef(defn)))
    val clos = ParOps.parMapValues(root.clos)(clo => flix.profile(clo.sym, clo.loc)(visitClo(clo)))
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    val types = allTypes(root, sctx.getTypes)
    val anonClasses = sctx.getAnonClasses

    JvmAst.Root(defs, clos, enums, structs, effects, types, anonClasses, root.mainEntryPoint, root.sources)
  }

  /** Reduces `defn0`, assigning offsets to its formal parameters and local variables. */
  private def visitDef(defn0: ErasedAst.Def)(implicit root: ErasedAst.Root, sctx: SharedContext): JvmAst.Def = defn0 match {
    case ErasedAst.Def(ann, mod, sym, fparams0, exp, tpe, unboxedType0, loc) =>
      implicit val lctx: LocalContext = new LocalContext(isControlImpure = Purity.isControlImpure(exp.purity))

      // It is important to visit parameters and variables in the order the backend expects: fparams, then lparams.
      val fparams = fparams0.map(visitOffsetFormalParam)
      val e = visitExpr(exp)
      // The local parameters are only known once `visitExpr` has populated `lctx`.
      val lparams = lctx.getLocalParams
      val pcPoints = lctx.getPcPoints
      val unboxedType = JvmAst.UnboxedType(unboxedType0.tpe)
      val defn = JvmAst.Def(ann, mod, sym, fparams, lparams, pcPoints, e, tpe, unboxedType, loc)

      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`.
      sctx.addType(defn.arrowType)
      sctx.addType(unboxedType.tpe)

      defn
  }

  /** Reduces `clo0`, assigning offsets to its captured parameters, formal parameters, and local variables. */
  private def visitClo(clo0: ErasedAst.Clo)(implicit root: ErasedAst.Root, sctx: SharedContext): JvmAst.Clo = clo0 match {
    case ErasedAst.Clo(sym, cparams0, fparams0, exp, tpe, loc) =>
      implicit val lctx: LocalContext = new LocalContext(isControlImpure = Purity.isControlImpure(exp.purity))

      // It is important to visit parameters and variables in the order the backend expects: cparams, fparams, then lparams.
      val cparams = cparams0.map(visitOffsetFormalParam)
      val fparams = fparams0.map(visitOffsetFormalParam)
      val e = visitExpr(exp)
      // The local parameters are only known once `visitExpr` has populated `lctx`.
      val lparams = lctx.getLocalParams
      val pcPoints = lctx.getPcPoints
      val clo = JvmAst.Clo(sym, cparams, fparams, lparams, pcPoints, e, tpe, loc)

      // `clo.fparams` and `clo.tpe` are both included in `clo.arrowType`, but the captured parameters are not.
      sctx.addType(clo.arrowType)
      for (cparam <- cparams) {
        sctx.addType(cparam.tpe)
      }

      clo
  }

  /** Reduces `enm`. */
  private def visitEnum(enm: ErasedAst.Enum): JvmAst.Enum = {
    val cases = MapOps.mapValues(enm.cases)(visitCase)
    JvmAst.Enum(enm.ann, enm.mod, enm.sym, cases, enm.loc)
  }

  /** Reduces `caze`. */
  private def visitCase(caze: ErasedAst.Case): JvmAst.Case =
    JvmAst.Case(caze.sym, caze.tpes, caze.loc)

  /** Reduces `struct`. */
  private def visitStruct(struct: ErasedAst.Struct): JvmAst.Struct = {
    val fields = struct.fields.map(visitStructField)
    JvmAst.Struct(struct.ann, struct.mod, struct.sym, fields, struct.loc)
  }

  /** Reduces `field`. */
  private def visitStructField(field: ErasedAst.StructField): JvmAst.StructField =
    JvmAst.StructField(field.sym, field.tpe, field.loc)

  /** Reduces `effect`. */
  private def visitEffect(effect: ErasedAst.Effect): JvmAst.Effect = {
    val ops = effect.ops.map(visitOp)
    JvmAst.Effect(effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  /** Reduces `op`. */
  private def visitOp(op: ErasedAst.Op): JvmAst.Op = {
    val fparams = op.fparams.map(visitFormalParam)
    JvmAst.Op(op.sym, op.ann, op.mod, fparams, op.tpe, op.purity, op.loc)
  }

  /** Reduces `exp0`, recording its type, pc points, and local variables in the contexts. */
  private def visitExpr(exp0: ErasedAst.Expr)(implicit lctx: LocalContext, root: ErasedAst.Root, sctx: SharedContext): JvmAst.Expr = {
    sctx.addType(exp0.tpe)
    exp0 match {
      case ErasedAst.Expr.Cst(cst, loc) =>
        JvmAst.Expr.Cst(cst, loc)

      case ErasedAst.Expr.Var(sym, tpe, loc) =>
        val offset = lctx.getOffset(sym)
        JvmAst.Expr.Var(sym, offset, tpe, loc)

      case ErasedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        op match {
          case AtomicOp.InvokeSuperMethod(sym, method) => sctx.addSuperMethod(sym, method)
          case _ => ()
        }
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case ErasedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(purity)) lctx.addPcPoint()
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.ApplyClo(e1, e2, ct, tpe, purity, loc)

      case ErasedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val defn = root.defs(sym)
        if (ct == ExpPosition.NonTail && Purity.isControlImpure(defn.exp.purity)) lctx.addPcPoint()
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

      case ErasedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
        lctx.addPcPoint()
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplyOp(sym, es, tpe, purity, loc)

      case ErasedAst.Expr.ApplySelfTail(sym, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        JvmAst.Expr.ApplySelfTail(sym, es, tpe, purity, loc)

      case ErasedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val e3 = visitExpr(exp3)
        JvmAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case ErasedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val bs = branches.map {
          case (label, body) => label -> visitExpr(body)
        }
        JvmAst.Expr.Branch(e, bs, tpe, purity, loc)

      case ErasedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        JvmAst.Expr.JumpTo(sym, tpe, purity, loc)

      case ErasedAst.Expr.Switch(exp, enumSym, cases, defaultExp, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val cs = cases.map {
          case (sym, body) => sym -> visitExpr(body)
        }
        val d = visitExpr(defaultExp)
        JvmAst.Expr.Switch(e, enumSym, cs, d, tpe, purity, loc)

      case ErasedAst.Expr.Let(sym, exp1, exp2, loc) =>
        val offset = lctx.assignOffset(sym, exp1.tpe)
        lctx.addLocalParam(JvmAst.LocalParam(sym, offset, exp1.tpe))
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        JvmAst.Expr.Let(sym, offset, e1, e2, loc)

      case ErasedAst.Expr.Stm(exps, exp, loc) =>
        val es = exps.map(visitExpr)
        val e = visitExpr(exp)
        JvmAst.Expr.Stm(es, e, loc)

      case ErasedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
        val offset = lctx.assignOffset(sym, SimpleType.Region)
        lctx.addLocalParam(JvmAst.LocalParam(sym, offset, SimpleType.Region))
        val e = visitExpr(exp)
        JvmAst.Expr.Region(sym, offset, e, tpe, purity, loc)

      case ErasedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules.map {
          case ErasedAst.CatchRule(sym, clazz, body) =>
            val offset = lctx.assignOffset(sym, SimpleType.Object)
            lctx.addLocalParam(JvmAst.LocalParam(sym, offset, SimpleType.Object))
            val b = visitExpr(body)
            JvmAst.CatchRule(sym, offset, clazz, b)
        }
        JvmAst.Expr.TryCatch(e, rs, tpe, purity, loc)

      case ErasedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
        if (ct == ExpPosition.NonTail) lctx.addPcPoint()
        val e = visitExpr(exp)
        val rs = rules.map {
          case ErasedAst.HandlerRule(op, fparams, body) =>
            val b = visitExpr(body)
            JvmAst.HandlerRule(op, fparams.map(visitFormalParam), b)
        }
        JvmAst.Expr.RunWith(e, effUse, rs, ct, tpe, purity, loc)

      case ErasedAst.Expr.NewObject(sym, clazz, tpe, purity, constructors, methods, loc) =>
        val cs = constructors.map {
          case ErasedAst.JvmConstructor(clo, retTpe, cnsPurity, cnsLoc) =>
            val c = visitExpr(clo)
            JvmAst.JvmConstructor(c, retTpe, cnsPurity, cnsLoc)
        }
        val ms = methods.map {
          case ErasedAst.JvmMethod(ann, ident, fparams, clo, retTpe, methPurity, javaSig, methLoc) =>
            val c = visitExpr(clo)
            JvmAst.JvmMethod(ann, ident, fparams.map(visitFormalParam), c, retTpe, methPurity, javaSig, methLoc)
        }
        sctx.addAnonClass(JvmAst.AnonClass(sym, clazz, tpe, cs, ms, Nil, loc))
        JvmAst.Expr.NewObject(sym, clazz, tpe, purity, cs, ms, loc)
    }
  }

  /** Assigns the next offset to `fp`, mutating `lctx`. */
  private def visitOffsetFormalParam(fp: ErasedAst.FormalParam)(implicit lctx: LocalContext): JvmAst.OffsetFormalParam = {
    val offset = lctx.assignOffset(fp.sym, fp.tpe)
    JvmAst.OffsetFormalParam(fp.sym, offset, fp.tpe)
  }

  /** Reduces `fp` without assigning an offset. */
  private def visitFormalParam(fp: ErasedAst.FormalParam): JvmAst.FormalParam =
    JvmAst.FormalParam(fp.sym, fp.tpe)

  /** Returns `types` and all types of `root`, including their nested component types. */
  private def allTypes(root: ErasedAst.Root, types: Set[SimpleType]): Set[SimpleType] = {
    // The erased types over-approximate the types in enums and structs.
    val erasedTypes = SimpleType.ErasedTypes
    val effectTypes = root.effects.values.toSet.flatMap(typesOfEffect)
    nestedTypesOf(Set.empty, Queue.from(types ++ erasedTypes ++ effectTypes))
  }

  /** Returns all types contained in `effect`. */
  private def typesOfEffect(effect: ErasedAst.Effect): Set[SimpleType] =
    effect.ops.toSet.map(arrowTypeOf)

  /** Returns the arrow type of `op` with the continuation added as a final parameter. */
  private def arrowTypeOf(op: ErasedAst.Op): SimpleType =
    SimpleType.mkArrow(op.fparams.map(_.tpe) :+ SimpleType.Object, op.tpe)

  /** Returns `acc` together with every type in `queue` and all of their nested component types. */
  @tailrec
  private def nestedTypesOf(acc: Set[SimpleType], queue: Queue[SimpleType]): Set[SimpleType] = {
    import SimpleType.*
    queue.dequeueOption match {
      case Some((tpe, tail)) =>
        val tail1 = tpe match {
          case Void | AnyType | Unit | Bool | Char | Float32 | Float64 | BigDecimal | Int8 | Int16 |
               Int32 | Int64 | BigInt | String | Regex | Region | RecordEmpty | ExtensibleEmpty |
               Native(_) | Null => tail
          case Array(elm) => tail.enqueue(elm)
          case Lazy(elm) => tail.enqueue(elm)
          case Tuple(elms) => tail.enqueueAll(elms)
          case Enum(_, targs) => tail.enqueueAll(targs)
          case Struct(_, targs) => tail.enqueueAll(targs)
          case Arrow(targs, tresult) => tail.enqueueAll(targs).enqueue(tresult)
          case RecordExtend(_, value, rest) => tail.enqueue(value).enqueue(rest)
          case ExtensibleExtend(_, targs, rest) => tail.enqueueAll(targs).enqueue(rest)
        }
        nestedTypesOf(acc + tpe, tail1)
      case None => acc
    }
  }

  /** A local non-shared context. Does not need to be thread-safe. */
  private final class LocalContext(private val isControlImpure: Boolean) {

    /** The local parameters in the order their offsets were assigned. */
    private val lparams: mutable.ArrayBuffer[JvmAst.LocalParam] = mutable.ArrayBuffer.empty

    /** The number of pc points in the enclosing def. */
    private var pcPoints: Int = 0

    /** The next free local variable offset. */
    private var nextVarOffset: Int = 0

    /** The offset assigned to each variable symbol. */
    private val offsets: mutable.Map[Symbol.VarSym, Int] = mutable.HashMap.empty

    /** Records `lparam` as a local parameter. */
    def addLocalParam(lparam: JvmAst.LocalParam): Unit =
      lparams.addOne(lparam)

    /** Returns the local parameters in the order they were added. */
    def getLocalParams: List[JvmAst.LocalParam] = lparams.toList

    /** Increments the pc point counter if the enclosing def is control impure. */
    def addPcPoint(): Unit =
      if (isControlImpure) pcPoints += 1

    /** Returns the number of pc points. */
    def getPcPoints: Int = pcPoints

    /** Assigns the next offset to `sym` and returns it. */
    def assignOffset(sym: Symbol.VarSym, tpe: SimpleType): Int = {
      if (offsets.contains(sym)) throw InternalCompilerException(s"Already assigned offset to '$sym'", sym.loc)
      val offset = allocateOffset(tpe)
      offsets.put(sym, offset)
      offset
    }

    /** Returns the offset of `sym`, throwing [[InternalCompilerException]] if not found. */
    def getOffset(sym: Symbol.VarSym): Int = offsets.get(sym) match {
      case Some(offset) => offset
      case None => throw InternalCompilerException(s"No offset found for '$sym'", sym.loc)
    }

    /** Returns the next free offset and advances it by the number of slots `tpe` occupies. */
    private def allocateOffset(tpe: SimpleType): Int = {
      val offset = nextVarOffset
      val size = tpe match {
        case SimpleType.Float64 => 2
        case SimpleType.Int64 => 2
        case _ => 1
      }
      nextVarOffset += size
      offset
    }

  }

  /** A context shared across threads, backed by concurrent collections. */
  private final class SharedContext {

    /** Collects all anonymous class / new object expressions encountered during reduction. */
    private val anonClasses: ConcurrentLinkedQueue[JvmAst.AnonClass] = new ConcurrentLinkedQueue()

    /** Maps anonymous classes to the super methods they invoke, so the backend can generate `invokespecial` bridge methods. */
    private val superMethods: ConcurrentHashMap[(Symbol.AnonClassSym, JMethod), Unit] = new ConcurrentHashMap()

    /** Collects all types encountered in defs and closures (used as a set via `ConcurrentHashMap`). */
    private val types: ConcurrentHashMap[SimpleType, Unit] = new ConcurrentHashMap()

    /** Records `clazz` as an anonymous class. */
    def addAnonClass(clazz: JvmAst.AnonClass): Unit =
      anonClasses.add(clazz)

    /** Records that the anonymous class `sym` invokes `method` on its super class. */
    def addSuperMethod(sym: Symbol.AnonClassSym, method: JMethod): Unit =
      superMethods.putIfAbsent((sym, method), ())

    /** Returns all anonymous classes with their super methods attached. */
    def getAnonClasses: List[JvmAst.AnonClass] = {
      // Group the super methods by the anonymous class that invokes them. We do this
      // once, up front, so that attaching them below costs a single lookup per class.
      val methodsOf = mutable.Map.empty[Symbol.AnonClassSym, mutable.ArrayBuffer[JMethod]]
      for ((sym, method) <- superMethods.keySet.asScala) {
        val methods = methodsOf.getOrElseUpdate(sym, mutable.ArrayBuffer.empty)
        methods += method
      }

      // Attach the super methods of each anonymous class. A class that invokes none
      // already has the empty list it was constructed with.
      anonClasses.asScala.toList.map { anonClass =>
        methodsOf.get(anonClass.sym) match {
          case None => anonClass
          case Some(methods) => anonClass.copy(superMethods = methods.toList)
        }
      }
    }

    /** Records `tpe` as a type occurring in the program. */
    def addType(tpe: SimpleType): Unit =
      types.putIfAbsent(tpe, ())

    /** Returns all types recorded by [[addType]]. */
    def getTypes: Set[SimpleType] =
      types.keySet.asScala.toSet

  }

}
