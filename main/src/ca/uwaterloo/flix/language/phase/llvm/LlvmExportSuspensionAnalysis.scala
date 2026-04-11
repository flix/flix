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

package ca.uwaterloo.flix.language.phase.llvm

import ca.uwaterloo.flix.language.ast.LoweredAst
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.ExportAbi
import ca.uwaterloo.flix.language.phase.ExportAbi.AbiType

/**
  * Computes conservative suspension summaries for lowered defs that are relevant to the wasm
  * export surface.
  *
  * The analysis is intentionally small:
  *   - it tracks effect ops that may escape a def after local `run with` handling,
  *   - it propagates those summaries transitively through direct def calls,
  *   - it falls back to `unknown` for dynamic closure calls.
  *
  * This is sufficient to infer a typed resume value when every escaping suspension for an export
  * resumes with the same portable ABI type. If not, the public export API must keep the generic
  * `value`-based fallback.
  */
object LlvmExportSuspensionAnalysis {

  case class Summary(ops: Set[Symbol.OpSym], unknown: Boolean) {
    def ++(that: Summary): Summary = Summary(this.ops ++ that.ops, this.unknown || that.unknown)

    def filterHandled(handled: Set[Symbol.EffSym]): Summary =
      Summary(ops.filterNot(op => handled.contains(op.eff)), unknown)
  }

  object Summary {
    val Empty: Summary = Summary(Set.empty, unknown = false)
    val UnknownOnly: Summary = Summary(Set.empty, unknown = true)
  }

  def compute(root: LoweredAst.Root): Map[Symbol.DefnSym, Summary] = {
    var summaries = root.defs.keysIterator.map(sym => sym -> Summary.Empty).toMap
    var changed = true

    while (changed) {
      val next = root.defs.view.mapValues(defn => visitExpr(defn.exp, Set.empty, summaries)).toMap
      changed = next != summaries
      summaries = next
    }

    summaries
  }

  def typedResumeType(sym: Symbol.DefnSym, summaries: Map[Symbol.DefnSym, Summary], root: LoweredAst.Root): Option[AbiType] = {
    val summary = summaries.getOrElse(sym, Summary.Empty)
    if (summary.unknown || summary.ops.isEmpty) return None

    val opDefs = root.effects.valuesIterator.flatMap(_.ops.iterator).map(op => op.sym -> op).toMap
    val resumeTypes = summary.ops.toList.map(op => opDefs(op).portableSignature.map(_.result))

    if (resumeTypes.contains(None)) None
    else {
      val distinct = resumeTypes.flatten.distinct
      distinct match {
        case tpe :: Nil => Some(tpe)
        case _ => None
      }
    }
  }

  def typedRequestSignature(sym: Symbol.DefnSym, summaries: Map[Symbol.DefnSym, Summary], root: LoweredAst.Root): Option[ExportAbi.Signature] = {
    val summary = summaries.getOrElse(sym, Summary.Empty)
    if (summary.unknown || summary.ops.size != 1) return None

    val opDefs = root.effects.valuesIterator.flatMap(_.ops.iterator).map(op => op.sym -> op).toMap
    val op = opDefs(summary.ops.head)
    op.portableSignature
  }

  def typedRequestOp(sym: Symbol.DefnSym, summaries: Map[Symbol.DefnSym, Summary], root: LoweredAst.Root): Option[LoweredAst.Op] = {
    val summary = summaries.getOrElse(sym, Summary.Empty)
    if (summary.unknown || summary.ops.size != 1) return None

    val opDefs = root.effects.valuesIterator.flatMap(_.ops.iterator).map(op => op.sym -> op).toMap
    opDefs.get(summary.ops.head)
  }

  private def visitExpr(exp: LoweredAst.Expr, handled: Set[Symbol.EffSym], summaries: Map[Symbol.DefnSym, Summary]): Summary = exp match {
    case _: LoweredAst.Expr.Cst => Summary.Empty
    case _: LoweredAst.Expr.NativeImport => Summary.Empty
    case _: LoweredAst.Expr.WasmImport => Summary.Empty
    case _: LoweredAst.Expr.Var => Summary.Empty

    case LoweredAst.Expr.ApplyAtomic(_, exps, _, _, _, _) =>
      combine(exps.map(visitExpr(_, handled, summaries)))

    case LoweredAst.Expr.ApplyClo(exp1, exp2, _, _, _, _, _) =>
      combine(List(visitExpr(exp1, handled, summaries), visitExpr(exp2, handled, summaries), Summary.UnknownOnly))

    case LoweredAst.Expr.ApplyDef(sym, exps, _, _, _, _, _) =>
      combine(exps.map(visitExpr(_, handled, summaries)) :+ summaries.getOrElse(sym, Summary.Empty).filterHandled(handled))

    case LoweredAst.Expr.ApplyOp(sym, exps, _, _, _, _) =>
      val child = combine(exps.map(visitExpr(_, handled, summaries)))
      if (handled.contains(sym.eff)) child else child ++ Summary(Set(sym), unknown = false)

    case LoweredAst.Expr.ApplySelfTail(sym, actuals, _, _, _) =>
      combine(actuals.map(visitExpr(_, handled, summaries)) :+ summaries.getOrElse(sym, Summary.Empty).filterHandled(handled))

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      combine(List(
        visitExpr(exp1, handled, summaries),
        visitExpr(exp2, handled, summaries),
        visitExpr(exp3, handled, summaries)
      ))

    case LoweredAst.Expr.Branch(exp0, branches, _, _, _) =>
      combine(visitExpr(exp0, handled, summaries) :: branches.values.map(visitExpr(_, handled, summaries)).toList)

    case _: LoweredAst.Expr.JumpTo =>
      Summary.Empty

    case LoweredAst.Expr.Let(_, exp1, exp2, _) =>
      combine(List(visitExpr(exp1, handled, summaries), visitExpr(exp2, handled, summaries)))

    case LoweredAst.Expr.Stmt(exp1, exp2, _) =>
      combine(List(visitExpr(exp1, handled, summaries), visitExpr(exp2, handled, summaries)))

    case LoweredAst.Expr.Region(_, exp0, _, _, _, _) =>
      visitExpr(exp0, handled, summaries)

    case LoweredAst.Expr.TryCatch(exp0, rules, _, _, _) =>
      combine(visitExpr(exp0, handled, summaries) :: rules.map(rule => visitExpr(rule.exp, handled, summaries)))

    case LoweredAst.Expr.RunWith(exp0, effUse, rules, _, _, _, _, _) =>
      val handledEffs = handled + effUse.sym
      combine(visitExpr(exp0, handledEffs, summaries) :: rules.map(rule => visitExpr(rule.exp, handledEffs, summaries)))

    case LoweredAst.Expr.NewObject(_, _, _, _, methods, _) =>
      combine(methods.map(m => visitExpr(m.exp, handled, summaries)))
  }

  private def combine(summaries: Iterable[Summary]): Summary =
    summaries.foldLeft(Summary.Empty)(_ ++ _)
}
