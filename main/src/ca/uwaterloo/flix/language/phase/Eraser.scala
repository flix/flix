/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.EraserMonad.ToMonad
import ca.uwaterloo.flix.language.phase.sjvm.{NamespaceInfo, SjvmOps}
import ca.uwaterloo.flix.language.phase.{EraserMonad => EM}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualString, VirtualTerminal}

object Eraser extends Phase[FinalAst.Root, ErasedAst.Root] {
  type FTypes = Set[RType[PReference[PFunction]]]

  def emptyFTypes: FTypes = Set[RType[PReference[PFunction]]]()

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[ErasedAst.Root, CompilationError] = flix.phase("Eraser") {
    val defns = EM.foldRight(root.defs)(Map[Symbol.DefnSym, ErasedAst.Def]().toMonad) {
      case ((k, v), mapp) => {
        visitDef(v) map (defn => mapp + (k -> defn))
      }
    }
    // TODO(JLS): should maybe be integrated to the other fold
    val defnsResult = defns.copyWith(namespaces = defns.value.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        // Collect all non-law definitions.
        val nonLaws = defs filter {
          case (_, defn) => SjvmOps.nonLaw(defn)
        }
        NamespaceInfo(ns, nonLaws)
    }.toSet)

    val reachable = root.reachable

    val result = ErasedAst.Root(defnsResult.value, reachable, root.sources, defnsResult.fTypes, defnsResult.namespaces)

    if (flix.options.debug) {
      val vt = new VirtualTerminal()
      vt << "All seen expressions (a-z):" << VirtualString.Indent << VirtualString.NewLine
      val expressionStrings = result.defs.foldLeft(Set[String]()) { case (set, (_, defn)) => set union collectExpressions(defn.exp) }
      expressionStrings.toList.sorted.zipWithIndex.foreach { case (str, index) => {
        vt << str
        if (index != expressionStrings.size - 1)
          vt << VirtualString.NewLine
      }
      }
      vt << VirtualString.Dedent
      println(vt.fmt(TerminalContext.AnsiTerminal))
    }

    result.toSuccess
  }

  private def collectExpressions(exp: ErasedAst.Expression[_ <: PType]): Set[String] = {
    val recursiveCalls: List[ErasedAst.Expression[_ <: PType]] = exp match {
      case ErasedAst.Expression.Unit(_) => Nil
      case ErasedAst.Expression.Null(_, _) => Nil
      case ErasedAst.Expression.True(_) => Nil
      case ErasedAst.Expression.False(_) => Nil
      case ErasedAst.Expression.Char(_, _) => Nil
      case ErasedAst.Expression.Float32(_, _) => Nil
      case ErasedAst.Expression.Float64(_, _) => Nil
      case ErasedAst.Expression.Int8(_, _) => Nil
      case ErasedAst.Expression.Int16(_, _) => Nil
      case ErasedAst.Expression.Int32(_, _) => Nil
      case ErasedAst.Expression.Int64(_, _) => Nil
      case ErasedAst.Expression.BigInt(_, _) => Nil
      case ErasedAst.Expression.Str(_, _) => Nil
      case ErasedAst.Expression.Var(_, _, _) => Nil
      case ErasedAst.Expression.Closure(sym, freeVars, tpe, loc) => Nil
      case ErasedAst.Expression.ApplyClo(exp, args, tpe, loc) => exp :: args
      case ErasedAst.Expression.ApplyDef(sym, args, tpe, loc) => args
      case ErasedAst.Expression.ApplyCloTail(exp, args, tpe, loc) => exp :: args
      case ErasedAst.Expression.ApplyDefTail(sym, args, tpe, loc) => args
      case ErasedAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => actuals
      case ErasedAst.Expression.Unary(sop, op, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => exp1 :: exp2 :: exp3 :: Nil
      case ErasedAst.Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(List[ErasedAst.Expression[_ <: PType]]()) { case (list, (_, exp)) => list :+ exp }
      case ErasedAst.Expression.JumpTo(sym, tpe, loc) => Nil
      case ErasedAst.Expression.Let(sym, exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Is(sym, tag, exp, loc) => exp :: Nil
      case ErasedAst.Expression.Tag(sym, tag, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Untag(sym, tag, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Index(base, offset, tpe, loc) => Nil
      case ErasedAst.Expression.Tuple(elms, tpe, loc) => elms
      case ErasedAst.Expression.RecordEmpty(tpe, loc) => Nil
      case ErasedAst.Expression.RecordSelect(exp, field, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.RecordExtend(field, value, rest, tpe, loc) => value :: rest :: Nil
      case ErasedAst.Expression.RecordRestrict(field, rest, tpe, loc) => rest :: Nil
      case ErasedAst.Expression.ArrayLit(elms, tpe, loc) => elms
      case ErasedAst.Expression.ArrayNew(elm, len, tpe, loc) => elm :: len :: Nil
      case ErasedAst.Expression.ArrayLoad(base, index, tpe, loc) => base :: index :: Nil
      case ErasedAst.Expression.ArrayStore(base, index, elm, tpe, loc) => base :: index :: elm :: Nil
      case ErasedAst.Expression.ArrayLength(base, tpe, loc) => base :: Nil
      case ErasedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => base :: beginIndex :: endIndex :: Nil
      case ErasedAst.Expression.Ref(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Deref(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Assign(exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Existential(fparam, exp, loc) => exp :: Nil
      case ErasedAst.Expression.Universal(fparam, exp, loc) => exp :: Nil
      case ErasedAst.Expression.Cast(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.TryCatch(exp, rules, tpe, loc) => exp :: rules.map(rule => rule.exp)
      case ErasedAst.Expression.InvokeConstructor(constructor, args, tpe, loc) => args
      case ErasedAst.Expression.InvokeMethod(method, exp, args, tpe, loc) => exp :: args
      case ErasedAst.Expression.InvokeStaticMethod(method, args, tpe, loc) => args
      case ErasedAst.Expression.GetField(field, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.PutField(field, exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.GetStaticField(field, tpe, loc) => Nil
      case ErasedAst.Expression.PutStaticField(field, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.NewChannel(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.GetChannel(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.PutChannel(exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val baseList = rules.flatMap(rule => rule.chan :: rule.exp :: Nil)
        default match {
          case Some(value) => baseList :+ value
          case None => baseList
        }
      case ErasedAst.Expression.Spawn(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Lazy(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Force(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.HoleError(sym, tpe, loc) => Nil
      case ErasedAst.Expression.MatchError(tpe, loc) => Nil
      case ErasedAst.Expression.BoxInt8(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxInt16(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxInt32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxInt64(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxChar(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxFloat32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxFloat64(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt8(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt16(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt64(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxChar(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxFloat32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxFloat64(exp, loc) => exp :: Nil
    }
    recursiveCalls.foldLeft(Set[String](exp.getClass.getSimpleName))((set, exp) => set union collectExpressions(exp))
  }

  /**
    * Translates the given definition `def0` to the ErasedAst.
    */
  private def visitDef(def0: FinalAst.Def): EraserMonad[ErasedAst.Def] = {
    for {
      formals0 <- EM.traverse(def0.formals)(visitFormalParam)
      exp <- visitExp[PReference[PFunction]](def0.exp)
      tpe <- visitTpe[PReference[PFunction]](def0.tpe)
    } yield ErasedAst.Def(def0.ann, def0.mod, def0.sym, formals0, exp, tpe, def0.loc)
  }

  /**
    * Translates the given expression `exp0` to the ErasedAst.
    */
  private def visitExp[T <: PType](baseExp: FinalAst.Expression): EraserMonad[ErasedAst.Expression[T]] = baseExp match {
    case FinalAst.Expression.Unit(loc) =>
      ErasedAst.Expression.Unit(loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Null(tpe, loc) =>
      for {
        tpe0 <- visitTpe[PReference[PRefType]](tpe)
        expRes = ErasedAst.Expression.Null[PRefType](tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.True(loc) =>
      ErasedAst.Expression.True(loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.False(loc) =>
      ErasedAst.Expression.False(loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Char(lit, loc) =>
      ErasedAst.Expression.Char(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Float32(lit, loc) =>
      ErasedAst.Expression.Float32(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Float64(lit, loc) =>
      ErasedAst.Expression.Float64(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int8(lit, loc) =>
      ErasedAst.Expression.Int8(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int16(lit, loc) =>
      ErasedAst.Expression.Int16(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int32(lit, loc) =>
      ErasedAst.Expression.Int32(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int64(lit, loc) =>
      ErasedAst.Expression.Int64(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.BigInt(lit, loc) =>
      ErasedAst.Expression.BigInt(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Str(lit, loc) =>
      ErasedAst.Expression.Str(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Var(sym, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
      } yield ErasedAst.Expression.Var(sym, tpe0, loc)

    case FinalAst.Expression.Closure(sym, freeVars, _, tpe, loc) =>
      for {
        freeVars0 <- EM.traverse(freeVars)(fv => visitTpe[PType](fv.tpe) map (
          tpe0 => ErasedAst.FreeVar(fv.sym, tpe0))
        )
        tpe0 <- visitTpe[PReference[PFunction]](tpe)
        expRes = ErasedAst.Expression.Closure(sym, freeVars0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyClo(exp, args, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        args0 <- EM.traverse(args)(visitExp[PType])
        exp0 <- visitExp[PReference[PFunction]](exp)
        expRes = ErasedAst.Expression.ApplyClo(exp0, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyDef(sym, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ApplyDef(sym, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PFunction]](exp)
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ApplyCloTail(exp0, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ApplyDefTail(sym, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      for {
        formals0 <- EM.traverse(formals)(fp =>
          visitTpe[PType](fp.tpe) map (tpe0 => ErasedAst.FormalParam(fp.sym, tpe0))
        )
        actuals0 <- EM.traverse(actuals)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ApplySelfTail(sym, formals0, actuals0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Unary(sop, op, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Unary(sop, op, exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PType](exp1)
        exp20 <- visitExp[PType](exp2)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Binary(sop, op, exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      for {
        exp10 <- visitExp[PInt32](exp1)
        exp20 <- visitExp[T](exp2)
        exp30 <- visitExp[T](exp3)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.IfThenElse(exp10, exp20, exp30, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Branch(exp, branches, tpe, loc) =>
      for {
        branches0 <- EM.foldRight(branches)(Map[Symbol.LabelSym, ErasedAst.Expression[T]]().toMonad) {
          case ((label, branchExp), map) => visitExp[T](branchExp) map (exp0 => map + (label -> exp0))
        }
        exp0 <- visitExp[T](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Branch(exp0, branches0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.JumpTo(sym, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PType](exp1)
        exp20 <- visitExp[T](exp2)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Let(sym, exp10, exp20, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Is(sym, tag, exp, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        expRes = ErasedAst.Expression.Is(sym, tag, exp0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.Tag(sym, tag, exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Untag(sym, tag, exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Index(base, offset, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PAnyObject]](base)
        tpe0 <- visitTpe[T](tpe)
        // TODO(JLS): maybe add cast
        expRes = ErasedAst.Expression.Index(base0, offset, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Tuple(elms, tpe, loc) =>
      for {
        elms0 <- EM.traverse(elms)(visitExp[PType])
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.Tuple(elms0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordEmpty(tpe, loc) =>
      for {
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.RecordEmpty(tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.RecordSelect(exp0, field, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
      for {
        value0 <- visitExp[PType](value)
        res0 <- visitExp[PReference[PAnyObject]](rest)
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.RecordExtend(field, value0, res0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
      for {
        rest0 <- visitExp[PReference[PAnyObject]](rest)
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.RecordRestrict(field, rest0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLit(elms, tpe, loc) =>
      for {
        elms0 <- EM.traverse(elms)(visitExp[PType])
        tpe0 <- visitTpe[PReference[PArray[PType]]](tpe)
        expRes = ErasedAst.Expression.ArrayLit(elms0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayNew(elm, len, tpe, loc) =>
      for {
        elm0 <- visitExp[T](elm)
        len0 <- visitExp[PInt32](len)
        tpe0 <- visitTpe[PReference[PArray[T]]](tpe)
        expRes = ErasedAst.Expression.ArrayNew(elm0, len0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLoad(base, index, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        index0 <- visitExp[PInt32](index)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ArrayLoad(base0, index0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        index0 <- visitExp[PInt32](index)
        elm0 <- visitExp[T](elm)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.ArrayStore(base0, index0, elm0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLength(base, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        tpe0 <- visitTpe[PInt32](tpe)
        expRes = ErasedAst.Expression.ArrayLength(base0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        beginIndex0 <- visitExp[PInt32](beginIndex)
        endIndex0 <- visitExp[PInt32](endIndex)
        tpe0 <- visitTpe[PReference[PArray[T]]](tpe)
        expRes = ErasedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Ref(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PRef[PType]]](tpe)
        expRes = ErasedAst.Expression.Ref(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PRef[T]]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Deref(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PReference[PRef[T]]](exp1)
        exp20 <- visitExp[T](exp2)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.Assign(exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Existential(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      for {
        exp0 <- visitExp[PInt32](exp)
        tpe0 <- visitTpe[PType](tpe)
        fparam0 = ErasedAst.FormalParam(sym, tpe0)
        expRes = ErasedAst.Expression.Existential(fparam0, exp0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Universal(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      for {
        exp0 <- visitExp[PInt32](exp)
        tpe0 <- visitTpe[PType](tpe)
        fparam0 = ErasedAst.FormalParam(sym, tpe0)
        expRes = ErasedAst.Expression.Universal(fparam0, exp0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Cast(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Cast(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.TryCatch(exp, rules, tpe, loc) =>
      for {
        rules0 <- EM.traverse(rules)(cr =>
          for {
            cexp0 <- visitExp[T](cr.exp)
          } yield ErasedAst.CatchRule[T](cr.sym, cr.clazz, cexp0)
        )
        exp0 <- visitExp[T](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.TryCatch(exp0, rules0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.InvokeConstructor(constructor, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.InvokeMethod(method, exp0, args0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.InvokeStaticMethod(method, args0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.GetField(field, exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PReference[PAnyObject]](exp1)
        exp20 <- visitExp[PType](exp2)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.PutField(field, exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.GetStaticField(field, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.PutStaticField(field, exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.NewChannel(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PInt32](exp)
        tpe0 <- visitTpe[PReference[PChan[T]]](tpe)
        expRes = ErasedAst.Expression.NewChannel(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.GetChannel(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PChan[T]]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.GetChannel(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PReference[PChan[T]]](exp1)
        exp20 <- visitExp[T](exp2)
        tpe0 <- visitTpe[PReference[PChan[T]]](tpe)
        expRes = ErasedAst.Expression.PutChannel(exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.SelectChannel(rules, default, tpe, loc) =>
      for {
        rules0 <- EM.traverse(rules)(rule => {
          for {
            exp0 <- visitExp[T](rule.exp)
            chan0 <- visitExp[PReference[PChan[PType]]](rule.chan)
          } yield ErasedAst.SelectChannelRule(rule.sym, chan0, exp0)
        })
        default0 <- default match {
          case Some(defaultExp) =>
            visitExp[T](defaultExp) map (v => Some(v))
          case None => None.toMonad
        }
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.SelectChannel(rules0, default0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Spawn(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.Spawn(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Lazy(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[T](exp)
        tpe0 <- visitTpe[PReference[PLazy[T]]](tpe)
        expRes = ErasedAst.Expression.Lazy(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Force(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PLazy[T]]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Force(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.HoleError(sym, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.MatchError(tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.MatchError(tpe0, loc)
      } yield expRes
  }

  /**
    * Translates the given formal param `p` to the ErasedAst.
    */
  private def visitFormalParam(p: FinalAst.FormalParam): EraserMonad[ErasedAst.FormalParam] = {
    visitTpe[PType](p.tpe) map (tpe0 => ErasedAst.FormalParam(p.sym, tpe0))
  }

  /**
    *
    * Translates the type 'tpe' to the ErasedType.
    */
  private def visitTpe[T <: PType](tpe: MonoType): EraserMonad[RType[T]] = tpe match {
    case MonoType.Unit => RReference(RUnit).asInstanceOf[RType[T]].toMonad
    case MonoType.Bool => RBool.asInstanceOf[RType[T]].toMonad
    case MonoType.Char => RChar.asInstanceOf[RType[T]].toMonad
    case MonoType.Float32 => RFloat32.asInstanceOf[RType[T]].toMonad
    case MonoType.Float64 => RFloat64.asInstanceOf[RType[T]].toMonad
    case MonoType.Int8 => RInt8.asInstanceOf[RType[T]].toMonad
    case MonoType.Int16 => RInt16.asInstanceOf[RType[T]].toMonad
    case MonoType.Int32 => RInt32.asInstanceOf[RType[T]].toMonad
    case MonoType.Int64 => RInt64.asInstanceOf[RType[T]].toMonad
    case MonoType.BigInt =>
      RReference(RBigInt).asInstanceOf[RType[T]].toMonad
    case MonoType.Str =>
      RReference(RStr).asInstanceOf[RType[T]].toMonad
    case MonoType.Array(tpe) =>
      for {
        tpe0 <- visitTpe[PType](tpe)
      } yield RReference(RArray[PType](tpe0)).asInstanceOf[RType[T]]
    case MonoType.Channel(tpe) =>
      for {
        tpe0 <- visitTpe[T](tpe)
      } yield RReference(RChannel(tpe0)).asInstanceOf[RType[T]]
    case MonoType.Lazy(tpe) =>
      for {
        tpe0 <- visitTpe[T](tpe)
      } yield RReference(RLazy(tpe0)).asInstanceOf[RType[T]]
    case MonoType.Ref(tpe) =>
      for {
        tpe0 <- visitTpe[T](tpe)
      } yield RReference(RRef(tpe0)).asInstanceOf[RType[T]]
    case MonoType.Tuple(elms) =>
      for {
        elms0 <- EM.traverse(elms)(visitTpe[PType])
        expRes = RReference(RTuple(elms0))
      } yield expRes.asInstanceOf[RType[T]]
    case MonoType.Enum(sym, args) =>
      for {
        args0 <- EM.traverse(args)(visitTpe[PType])
      } yield RReference(REnum(sym, args0)).asInstanceOf[RType[T]]
    case MonoType.Arrow(args, result) =>
      val newMonad = for {
        args0 <- EM.traverse(args)(visitTpe[PType])
        result0 <- visitTpe[PType](result)
        tpeRes = RReference(RArrow(args0, result0))
      } yield tpeRes
      newMonad.flatMap(f => f.asInstanceOf[RType[T]].toMonad.copyWith(fTypes = Set(f)))
    case MonoType.RecordEmpty() =>
      RReference(RRecordEmpty).asInstanceOf[RType[T]].toMonad
    case MonoType.RecordExtend(field, value, rest) =>
      for {
        value0 <- visitTpe[PType](value)
        rest0 <- visitTpe[PReference[PAnyObject]](rest)
        expRes = RReference(RRecordExtend(field, value0, rest0))
      } yield expRes.asInstanceOf[RType[T]]
    case MonoType.SchemaEmpty() =>
      RReference(RSchemaEmpty).asInstanceOf[RType[T]].toMonad
    case MonoType.SchemaExtend(name, tpe, rest) =>
      for {
        tpe0 <- visitTpe[PType](tpe)
        rest0 <- visitTpe[PReference[PAnyObject]](rest)
        expRes = RReference(RSchemaExtend(name, tpe0, rest0))
      } yield expRes.asInstanceOf[RType[T]]
    case MonoType.Relation(tpes) =>
      for {
        tpes0 <- EM.traverse(tpes)(visitTpe[PType])
      } yield RReference(RRelation(tpes0)).asInstanceOf[RType[T]]
    case MonoType.Lattice(tpes) =>
      for {
        tpes0 <- EM.traverse(tpes)(visitTpe[PType])
      } yield RReference(RLattice(tpes0)).asInstanceOf[RType[T]]
    case MonoType.Native(clazz) =>
      RReference(RNative(clazz)).asInstanceOf[RType[T]].toMonad
    case MonoType.Var(id) =>
      RReference(RVar(id)).asInstanceOf[RType[T]].toMonad
  }
}
