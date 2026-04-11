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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.ExpPosition
import ca.uwaterloo.flix.language.ast.{AtomicOp, ErasedAst, LoweredAst, Purity, SemanticOp, SimpleType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.util.{CompilationTarget, InternalCompilerException}
import ca.uwaterloo.flix.util.ParOps

import scala.collection.mutable

/**
  * Computes backend-neutral lowered information (locals + pc-points).
  */
object Lowerer {

  def run(root: ErasedAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("Lowerer") {
    implicit val r: ErasedAst.Root = root
    val target = flix.options.target
    implicit val suspendableDefs: Map[Symbol.DefnSym, Boolean] = target match {
      case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm => computeSuspendableDefs(root, target)
      case _ => Map.empty
    }

    val defs = ParOps.parMapValues(root.defs)(visitDef(_, target))
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    LoweredAst.Root(defs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }(DebugNoOp())

  private def visitDef(d: ErasedAst.Def, target: CompilationTarget)(implicit root: ErasedAst.Root, suspendableDefs: Map[Symbol.DefnSym, Boolean]): LoweredAst.Def = d match {
    case ErasedAst.Def(ann, mod, sym, cparams0, fparams0, exp, tpe, unboxedType0, exportedSignature, nativeImportSignature, wasmImportSignature, loc) =>
      implicit val lctx: LocalContext = new LocalContext(isControlImpure = canSuspend(exp.purity, target) || suspendableDefs.getOrElse(sym, false))

      // It is important to visit parameters and variables in the order the backend expects: cparams, fparams, then lparams.
      val cparams = cparams0.map(visitFormalParam)
      val fparams = fparams0.map(visitFormalParam)
      val e = visitExpr(exp, target)

      val ls = lctx.lparams.toList
      val pcPoints = lctx.getPcPoints
      val unboxedType = LoweredAst.UnboxedType(unboxedType0.tpe)

      LoweredAst.Def(ann, mod, sym, cparams, fparams, ls, pcPoints, e, tpe, unboxedType, exportedSignature, nativeImportSignature, wasmImportSignature, loc)
  }

  private def visitEnum(enm: ErasedAst.Enum): LoweredAst.Enum = {
    val cases = enm.cases.map {
      case (sym, caze) => sym -> visitCase(caze)
    }
    LoweredAst.Enum(enm.ann, enm.mod, enm.sym, cases, enm.loc)
  }

  private def visitCase(caze: ErasedAst.Case): LoweredAst.Case =
    LoweredAst.Case(caze.sym, caze.tpes, caze.loc)

  private def visitStruct(struct: ErasedAst.Struct): LoweredAst.Struct = {
    val fields = struct.fields.map(visitStructField)
    LoweredAst.Struct(struct.ann, struct.mod, struct.sym, fields, struct.loc)
  }

  private def visitStructField(field: ErasedAst.StructField): LoweredAst.StructField =
    LoweredAst.StructField(field.sym, field.tpe, field.loc)

  private def visitEffect(effect: ErasedAst.Effect): LoweredAst.Effect = {
    val ops = effect.ops.map(visitOp)
    LoweredAst.Effect(effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  private def visitOp(op: ErasedAst.Op): LoweredAst.Op = {
    val fparams = op.fparams.map(visitFormalParam)
    LoweredAst.Op(op.sym, op.ann, op.mod, fparams, op.tpe, op.purity, op.portableSignature, op.loc)
  }

  private def visitExpr(exp0: ErasedAst.Expr, target: CompilationTarget)(implicit lctx: LocalContext, root: ErasedAst.Root, suspendableDefs: Map[Symbol.DefnSym, Boolean]): LoweredAst.Expr = exp0 match {
    case ErasedAst.Expr.Cst(cst, loc) =>
      LoweredAst.Expr.Cst(cst, loc)

    case ErasedAst.Expr.NativeImport(spec, tpe, purity, loc) =>
      LoweredAst.Expr.NativeImport(spec, tpe, purity, loc)
    case ErasedAst.Expr.WasmImport(spec, tpe, purity, loc) =>
      LoweredAst.Expr.WasmImport(spec, tpe, purity, loc)

    case ErasedAst.Expr.Var(sym, tpe, loc) =>
      LoweredAst.Expr.Var(sym, tpe, loc)

    case ErasedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val pcPointId = if (isSuspendableAtomicOp(op, target)) lctx.newPcPointId() else 0
      val es = exps.map(visitExpr(_, target))
      LoweredAst.Expr.ApplyAtomic(op, es, pcPointId, tpe, purity, loc)

    case ErasedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
      val pcPointId = if (ct == ExpPosition.NonTail && canSuspend(purity, target)) lctx.newPcPointId() else 0
      val e1 = visitExpr(exp1, target)
      val e2 = visitExpr(exp2, target)
      LoweredAst.Expr.ApplyClo(e1, e2, ct, pcPointId, tpe, purity, loc)

    case ErasedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val calleeMaySuspend = target match {
        case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm =>
          suspendableDefs.getOrElse(sym, false) || canSuspend(purity, target)
        case _ =>
          val defn = root.defs(sym)
          canSuspend(defn.exp.purity, target)
      }
      val pcPointId = if (ct == ExpPosition.NonTail && calleeMaySuspend) lctx.newPcPointId() else 0
      val es = exps.map(visitExpr(_, target))
      LoweredAst.Expr.ApplyDef(sym, es, ct, pcPointId, tpe, purity, loc)

    case ErasedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
      val pcPointId = lctx.newPcPointId()
      val es = exps.map(visitExpr(_, target))
      LoweredAst.Expr.ApplyOp(sym, es, pcPointId, tpe, purity, loc)

    case ErasedAst.Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      val es = actuals.map(visitExpr(_, target))
      LoweredAst.Expr.ApplySelfTail(sym, es, tpe, purity, loc)

    case ErasedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1, target)
      val e2 = visitExpr(exp2, target)
      val e3 = visitExpr(exp3, target)
      LoweredAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case ErasedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp, target)
      val bs = branches.map {
        case (label, body) => label -> visitExpr(body, target)
      }
      LoweredAst.Expr.Branch(e, bs, tpe, purity, loc)

    case ErasedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      LoweredAst.Expr.JumpTo(sym, tpe, purity, loc)

    case ErasedAst.Expr.Let(sym, exp1, exp2, loc) =>
      lctx.lparams.addOne(LoweredAst.LocalParam(sym, exp1.tpe))
      val e1 = visitExpr(exp1, target)
      val e2 = visitExpr(exp2, target)
      LoweredAst.Expr.Let(sym, e1, e2, loc)

    case ErasedAst.Expr.Stmt(exp1, exp2, loc) =>
      val e1 = visitExpr(exp1, target)
      val e2 = visitExpr(exp2, target)
      LoweredAst.Expr.Stmt(e1, e2, loc)

    case ErasedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
      lctx.lparams.addOne(LoweredAst.LocalParam(sym, SimpleType.Region))
      val pcPointId = target match {
        case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm if canSuspend(purity, target) => lctx.newPcPointId()
        case _ => 0
      }
      val e = visitExpr(exp, target)
      LoweredAst.Expr.Region(sym, e, pcPointId, tpe, purity, loc)

    case ErasedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp, target)
      val catchBinderTpe = target match {
        case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm => portableExnTpe(root)
        case _ => SimpleType.Object
      }
      val rs = rules.map {
        case ErasedAst.CatchRule(sym, catchTpe, body) =>
          lctx.lparams.addOne(LoweredAst.LocalParam(sym, catchBinderTpe))
          val b = visitExpr(body, target)
          LoweredAst.CatchRule(sym, catchTpe, b)
      }
      LoweredAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case ErasedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val pcPointId = if (ct == ExpPosition.NonTail) lctx.newPcPointId() else 0
      val e = visitExpr(exp, target)
      val rs = rules.map {
        case ErasedAst.HandlerRule(op, fparams, body) =>
          val b = visitExpr(body, target)
          LoweredAst.HandlerRule(op, fparams.map(visitFormalParam), b)
      }
      LoweredAst.Expr.RunWith(e, effUse, rs, ct, pcPointId, tpe, purity, loc)

    case ErasedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val specs = methods.map {
        case ErasedAst.JvmMethod(ident, fparams, clo, retTpe, methPurity, methLoc) =>
          val c = visitExpr(clo, target)
          LoweredAst.JvmMethod(ident, fparams.map(visitFormalParam), c, retTpe, methPurity, methLoc)
      }
      LoweredAst.Expr.NewObject(name, clazz, tpe, purity, specs, loc)

  }

  private def portableExnTpe(root: ErasedAst.Root): SimpleType = {
    root.enums.keysIterator.find(sym => sym.text == "Exn" && sym.namespace.isEmpty) match {
      case Some(sym) => SimpleType.mkEnum(sym, Nil)
      case None => throw InternalCompilerException("Missing enum symbol: Exn.", SourceLocation.Unknown)
    }
  }

  private def visitFormalParam(fp: ErasedAst.FormalParam): LoweredAst.FormalParam =
    LoweredAst.FormalParam(fp.sym, fp.tpe)

  /**
    * A local non-shared context. Does not need to be thread-safe.
    */
  private class LocalContext(private val isControlImpure: Boolean) {

    val lparams: mutable.ArrayBuffer[LoweredAst.LocalParam] = mutable.ArrayBuffer.empty

    private var pcPoints: Int = 0

    def newPcPointId(): Int = {
      if (isControlImpure) {
        pcPoints += 1
        pcPoints
      } else {
        0
      }
    }

    def getPcPoints: Int = pcPoints
  }

  private def canSuspend(purity: Purity, target: CompilationTarget): Boolean = target match {
    case CompilationTarget.LlvmNative => !Purity.isPure(purity)
    case CompilationTarget.LlvmWasm => !Purity.isPure(purity)
    case _ => Purity.isControlImpure(purity)
  }

  private def computeSuspendableDefs(root: ErasedAst.Root, target: CompilationTarget): Map[Symbol.DefnSym, Boolean] = {
    var result = root.defs.keysIterator.map(sym => sym -> false).toMap
    var changed = true
    while (changed) {
      changed = false
      val next = root.defs.iterator.map {
        case (sym, defn) => sym -> exprMaySuspend(defn.exp, target, result)
      }.toMap
      if (next != result) {
        result = next
        changed = true
      }
    }
    result
  }

  private def exprMaySuspend(exp: ErasedAst.Expr, target: CompilationTarget, suspendableDefs: Map[Symbol.DefnSym, Boolean]): Boolean = exp match {
    case ErasedAst.Expr.Cst(_, _) => false
    case ErasedAst.Expr.NativeImport(_, _, _, _) => false
    case ErasedAst.Expr.WasmImport(_, _, _, _) => false
    case ErasedAst.Expr.Var(_, _, _) => false

    case ErasedAst.Expr.ApplyAtomic(op, exps, _, _, _) =>
      isSuspendableAtomicOp(op, target) || exps.exists(exprMaySuspend(_, target, suspendableDefs))

    case ErasedAst.Expr.ApplyClo(exp1, exp2, _, _, purity, _) =>
      canSuspend(purity, target) ||
        exprMaySuspend(exp1, target, suspendableDefs) ||
        exprMaySuspend(exp2, target, suspendableDefs)

    case ErasedAst.Expr.ApplyDef(sym, exps, _, _, _, _) =>
      suspendableDefs.getOrElse(sym, false) || exps.exists(exprMaySuspend(_, target, suspendableDefs))

    case ErasedAst.Expr.ApplyOp(_, exps, _, _, _) =>
      true || exps.exists(exprMaySuspend(_, target, suspendableDefs))

    case ErasedAst.Expr.ApplySelfTail(sym, actuals, _, _, _) =>
      suspendableDefs.getOrElse(sym, false) || actuals.exists(exprMaySuspend(_, target, suspendableDefs))

    case ErasedAst.Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      exprMaySuspend(exp1, target, suspendableDefs) ||
        exprMaySuspend(exp2, target, suspendableDefs) ||
        exprMaySuspend(exp3, target, suspendableDefs)

    case ErasedAst.Expr.Branch(exp, branches, _, _, _) =>
      exprMaySuspend(exp, target, suspendableDefs) || branches.values.exists(exprMaySuspend(_, target, suspendableDefs))

    case ErasedAst.Expr.JumpTo(_, _, _, _) => false

    case ErasedAst.Expr.Let(_, exp1, exp2, _) =>
      exprMaySuspend(exp1, target, suspendableDefs) || exprMaySuspend(exp2, target, suspendableDefs)

    case ErasedAst.Expr.Stmt(exp1, exp2, _) =>
      exprMaySuspend(exp1, target, suspendableDefs) || exprMaySuspend(exp2, target, suspendableDefs)

    case ErasedAst.Expr.Region(_, exp, _, _, _) =>
      exprMaySuspend(exp, target, suspendableDefs)

    case ErasedAst.Expr.TryCatch(exp, rules, _, _, _) =>
      exprMaySuspend(exp, target, suspendableDefs) || rules.exists(rule => exprMaySuspend(rule.exp, target, suspendableDefs))

    case ErasedAst.Expr.RunWith(exp, _, rules, _, _, _, _) =>
      exprMaySuspend(exp, target, suspendableDefs) || rules.exists(rule => exprMaySuspend(rule.exp, target, suspendableDefs))

    case ErasedAst.Expr.NewObject(_, _, _, _, methods, _) =>
      methods.exists(m => exprMaySuspend(m.exp, target, suspendableDefs))
  }

  private def isSuspendableAtomicOp(op: AtomicOp, target: CompilationTarget): Boolean = target match {
    case CompilationTarget.LlvmWasm =>
      op match {
        case AtomicOp.Unary(sop) => isSuspendableIoOp(sop)
        case AtomicOp.ChannelGet | AtomicOp.ChannelPut | AtomicOp.ChannelSelect | AtomicOp.ReentrantLockLock | AtomicOp.ConditionAwait | AtomicOp.CyclicBarrierAwait | AtomicOp.CountDownLatchAwait | AtomicOp.SemaphoreAcquire => true
        case _ => false
      }
    case CompilationTarget.LlvmNative =>
      op match {
        case AtomicOp.Unary(SemanticOp.IoOp.SleepMillis) => true
        case AtomicOp.Unary(SemanticOp.IoOp.HttpRequest) => true
        case AtomicOp.Unary(SemanticOp.IoOp.FileWrite) => true
        case AtomicOp.Unary(SemanticOp.IoOp.FileWriteBytes) => true
        case AtomicOp.Unary(SemanticOp.IoOp.FileAppend) => true
        case AtomicOp.Unary(SemanticOp.IoOp.FileAppendBytes) => true
        case AtomicOp.Unary(SemanticOp.IoOp.TcpSocketConnect) => true
        case AtomicOp.Unary(SemanticOp.IoOp.TcpSocketRead) => true
        case AtomicOp.Unary(SemanticOp.IoOp.TcpSocketWrite) => true
        case AtomicOp.Unary(SemanticOp.IoOp.TcpServerAccept) => true
        case AtomicOp.Unary(SemanticOp.IoOp.ProcessWaitFor) => true
        case AtomicOp.Unary(SemanticOp.IoOp.ProcessWaitForTimeout) => true
        case AtomicOp.Unary(SemanticOp.IoOp.ProcessStdinWrite) => true
        case AtomicOp.Unary(SemanticOp.IoOp.ProcessStdoutRead) => true
        case AtomicOp.Unary(SemanticOp.IoOp.ProcessStderrRead) => true
        case AtomicOp.ReentrantLockLock => true
        case AtomicOp.ConditionAwait => true
        case AtomicOp.CyclicBarrierAwait => true
        case AtomicOp.CountDownLatchAwait => true
        case AtomicOp.SemaphoreAcquire => true
        case _ => false
      }
    case _ => false
  }

  private def isSuspendableIoOp(sop: SemanticOp.UnaryOp): Boolean = sop match {
    case SemanticOp.IoOp.Readln => true
    case SemanticOp.IoOp.SleepMillis => true
    case SemanticOp.IoOp.FileExists => true
    case SemanticOp.IoOp.FileIsDirectory => true
    case SemanticOp.IoOp.FileIsRegularFile => true
    case SemanticOp.IoOp.FileIsReadable => true
    case SemanticOp.IoOp.FileIsSymbolicLink => true
    case SemanticOp.IoOp.FileIsWritable => true
    case SemanticOp.IoOp.FileIsExecutable => true
    case SemanticOp.IoOp.FileAccessTime => true
    case SemanticOp.IoOp.FileCreationTime => true
    case SemanticOp.IoOp.FileModificationTime => true
    case SemanticOp.IoOp.FileSize => true
    case SemanticOp.IoOp.FileRead => true
    case SemanticOp.IoOp.FileReadLines => true
    case SemanticOp.IoOp.FileReadBytes => true
    case SemanticOp.IoOp.FileList => true
    case SemanticOp.IoOp.FileWrite => true
    case SemanticOp.IoOp.FileWriteBytes => true
    case SemanticOp.IoOp.FileAppend => true
    case SemanticOp.IoOp.FileAppendBytes => true
    case SemanticOp.IoOp.FileTruncate => true
    case SemanticOp.IoOp.FileMkDir => true
    case SemanticOp.IoOp.FileMkDirs => true
    case SemanticOp.IoOp.FileMkTempDir => true
    case SemanticOp.IoOp.TcpSocketRead => true
    case SemanticOp.IoOp.TcpSocketWrite => true
    case SemanticOp.IoOp.TcpSocketConnect => true
    case SemanticOp.IoOp.TcpSocketClose => true
    case SemanticOp.IoOp.TcpServerBind => true
    case SemanticOp.IoOp.TcpServerLocalPort => true
    case SemanticOp.IoOp.TcpServerAccept => true
    case SemanticOp.IoOp.TcpServerClose => true
    case SemanticOp.IoOp.ProcessStdinWrite => true
    case SemanticOp.IoOp.ProcessExec => true
    case SemanticOp.IoOp.ProcessExitValue => true
    case SemanticOp.IoOp.ProcessIsAlive => true
    case SemanticOp.IoOp.ProcessPid => true
    case SemanticOp.IoOp.ProcessStop => true
    case SemanticOp.IoOp.ProcessWaitFor => true
    case SemanticOp.IoOp.ProcessWaitForTimeout => true
    case SemanticOp.IoOp.ProcessStdoutRead => true
    case SemanticOp.IoOp.ProcessStderrRead => true
    case SemanticOp.IoOp.ProcessRelease => true
    case SemanticOp.IoOp.HttpRequest => true
    case _ => false
  }

}
