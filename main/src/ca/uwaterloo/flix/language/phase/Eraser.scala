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
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Eraser extends Phase[FinalAst.Root, ErasedAst.Root] {
  type FTypes = Set[RType[PReference[PFunction]]]

  def emptyFTypes(): FTypes = Set[RType[PReference[PFunction]]]()

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[ErasedAst.Root, CompilationError] = flix.phase("Eraser") {
    val (defns, functionTypes) = root.defs.foldLeft((Map[Symbol.DefnSym, ErasedAst.Def[_ <: PType]](), emptyFTypes())) { case ((m, s), (k, v)) =>
      val (defn, ftypes) = visitDef(v)
      (m + (k -> defn), s union ftypes)
    }
    //    val enums = root.enums.map {
    //      case (k, FinalAst.Enum(mod, sym, cases0, _, loc)) =>
    //        val cases = cases0 map {
    //          case (tag, FinalAst.Case(enumSym, tagName, tagTpeDeprecated, tagLoc)) => tag -> ErasedAst.Case(enumSym, tagName, visitTpe(tagTpeDeprecated), tagLoc)
    //        }
    //        k -> ErasedAst.Enum(mod, sym, cases, loc)
    //    }
    //    val properties = root.properties.map { p => visitProperty(p) }
    val reachable = root.reachable
    val functionResultTypes = functionTypes.foldLeft(Set[RType[_ <: PType]]()){case (s, RReference(RArrow(_, result))) => s + result}

    ErasedAst.Root(defns, reachable, root.sources, functionTypes).toSuccess
  }

  /**
   * Translates the given definition `def0` to the ErasedAst.
   */
  private def visitDef[T <: PType](def0: FinalAst.Def): (ErasedAst.Def[T], FTypes) = {
    val (formals0, ftypes0) = def0.formals.foldLeft((List[ErasedAst.FormalParam](), emptyFTypes())) {
      case ((l, s), param) =>
        val (expRes, ftypesRes) = visitFormalParam(param)
        (l :+ expRes, s union ftypesRes)
    }
    val (exp, ftypes1) = visitExp[T](def0.exp)
    val (tpe, ftypes2) = visitTpe[T](def0.tpe)
    val expRes = ErasedAst.Def(def0.ann, def0.mod, def0.sym, formals0, exp, tpe, def0.loc)
    val ftypesRes = ftypes0 union ftypes1 union ftypes2
    (expRes, ftypesRes)
  }

  /**
   * Translates the given expression `exp0` to the ErasedAst.
   */
  private def visitExp[T <: PType](baseExp: FinalAst.Expression): (ErasedAst.Expression[T], FTypes) = baseExp match {
    case FinalAst.Expression.Unit(loc) =>
      val expRes = ErasedAst.Expression.Unit(loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Null(tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[PReference[PRefType]](tpe)
      val expRes = ErasedAst.Expression.Null[PRefType](tpe0, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.True(loc) =>
      val expRes = ErasedAst.Expression.True(loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.False(loc) =>
      val expRes = ErasedAst.Expression.False(loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Char(lit, loc) =>
      val expRes = ErasedAst.Expression.Char(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Float32(lit, loc) =>
      val expRes = ErasedAst.Expression.Float32(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Float64(lit, loc) =>
      val expRes = ErasedAst.Expression.Float64(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Int8(lit, loc) =>
      val expRes = ErasedAst.Expression.Int8(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Int16(lit, loc) =>
      val expRes = ErasedAst.Expression.Int16(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Int32(lit, loc) =>
      val expRes = ErasedAst.Expression.Int32(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Int64(lit, loc) =>
      val expRes = ErasedAst.Expression.Int64(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.BigInt(lit, loc) =>
      val expRes = ErasedAst.Expression.BigInt(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Str(lit, loc) =>
      val expRes = ErasedAst.Expression.Str(lit, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], emptyFTypes())

    case FinalAst.Expression.Var(sym, tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Var(sym, tpe0, loc)
      (expRes, ftypesRes)

    case FinalAst.Expression.Closure(sym, freeVars, _, tpe, loc) =>
      val (freeVars0, ftypes0) = freeVars.foldLeft((List[ErasedAst.FreeVar](), emptyFTypes())) {
        case ((l, s), FinalAst.FreeVar(sym, tpe)) => {
          val (tpe0, ftypes0) = visitTpe[PType](tpe)
          (l :+ ErasedAst.FreeVar(sym, tpe0), s union ftypes0)
        }
      }
      val (tpe0, ftypes1) = visitTpe[PReference[PFunction]](tpe)
      val expRes = ErasedAst.Expression.Closure(sym, freeVars0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ApplyClo(exp, args, tpe, loc) =>
      val (tpe0, ftypes0) = visitTpe[T](tpe)
      val (args0, ftypes1) = visitExps[PType](args)
      val (exp0, ftypes2) = visitExp[PReference[PFunction]](exp)
      val expRes = ErasedAst.Expression.ApplyClo(exp0, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ApplyDef(sym, args, tpe, loc) =>
      val (args0, ftypes0) = visitExps[PType](args)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.ApplyDef(sym, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PFunction]](exp)
      val (args0, ftypes1) = visitExps[PType](args)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.ApplyCloTail(exp0, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val (args0, ftypes0) = visitExps[PType](args)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.ApplyDefTail(sym, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val (formals0, ftypes0) = formals.foldLeft((List[ErasedAst.FormalParam](), emptyFTypes())) {
        case ((l, s), FinalAst.FormalParam(sym, tpe)) => {
          val (tpe0, ftypes0) = visitTpe[PType](tpe)
          (l :+ ErasedAst.FormalParam(sym, tpe0), s union ftypes0)
        }
      }
      val (actuals0, ftypes1) = visitExps[PType](actuals)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.ApplySelfTail(sym, formals0, actuals0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Unary(sop, op, exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PType](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Unary(sop, op, exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val (exp10, ftypes0) = visitExp[PType](exp1)
      val (exp20, ftypes1) = visitExp[PType](exp2)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Binary(sop, op, exp10, exp20, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val (exp10, ftypes0) = visitExp[PInt32](exp1)
      val (exp20, ftypes1) = visitExp[T](exp2)
      val (exp30, ftypes2) = visitExp[T](exp3)
      val (tpe0, ftypes3) = visitTpe[T](tpe)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2 union ftypes3
      val expRes = ErasedAst.Expression.IfThenElse(exp10, exp20, exp30, tpe0, loc)
      (expRes, ftypesRes)

    case FinalAst.Expression.Branch(exp, branches, tpe, loc) =>
      val (branches0, ftypes0) = branches.foldLeft((Map[Symbol.LabelSym, ErasedAst.Expression[T]](), emptyFTypes())) {
        case ((m, s), (label, branchExp)) => {
          val (exp0, ftypes0) = visitExp[T](branchExp)
          (m + (label -> exp0), s union ftypes0)
        }
      }
      val (exp0, ftypes1) = visitExp[T](exp)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Branch(exp0, branches0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes, ftypesRes)

    case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.JumpTo(sym, tpe0, loc)
      (expRes, ftypesRes)

    case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val (exp10, ftypes0) = visitExp[PType](exp1)
      val (exp20, ftypes1) = visitExp[T](exp2)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Let(sym, exp10, exp20, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes, ftypesRes)

    case FinalAst.Expression.Is(sym, tag, exp, loc) =>
      val (exp0, ftypesRes) = visitExp[PReference[PAnyObject]](exp)
      val expRes = ErasedAst.Expression.Is(sym, tag, exp0, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PType](exp)
      val (tpe0, ftypes1) = visitTpe[PReference[PAnyObject]](tpe)
      val expRes = ErasedAst.Expression.Tag(sym, tag, exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PAnyObject]](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val ftypesRes = ftypes0 union ftypes1
      (ErasedAst.Expression.Untag(sym, tag, exp0, tpe0, loc), ftypesRes)

    case FinalAst.Expression.Index(base, offset, tpe, loc) =>
      val (base0, ftypes0) = visitExp[PReference[PAnyObject]](base)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      // TODO(JLS): maybe add cast
      val expRes = ErasedAst.Expression.Index(base0, offset, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.Tuple(elms, tpe, loc) =>
      val (elms0, ftypes0) = visitExps[PType](elms)
      val (tpe0, ftypes1) = visitTpe[PReference[PAnyObject]](tpe)
      val expRes = ErasedAst.Expression.Tuple(elms0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.RecordEmpty(tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[PReference[PAnyObject]](tpe)
      val expRes = ErasedAst.Expression.RecordEmpty(tpe0, loc)
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PAnyObject]](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.RecordSelect(exp0, field, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
      val (value0, ftypes0) = visitExp[PType](value)
      val (res0, ftypes1) = visitExp[PReference[PAnyObject]](rest)
      val (tpe0, ftypes2) = visitTpe[PReference[PAnyObject]](tpe)
      val expRes = ErasedAst.Expression.RecordExtend(field, value0, res0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
      val (rest0, ftypes0) = visitExp[PReference[PAnyObject]](rest)
      val (tpe0, ftypes1) = visitTpe[PReference[PAnyObject]](tpe)
      val expRes = ErasedAst.Expression.RecordRestrict(field, rest0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ArrayLit(elms, tpe, loc) =>
      val (elms0, ftypes0) = visitExps[T](elms)
      val (tpe0, ftypes1) = visitTpe[PReference[PArray[T]]](tpe)
      val expRes = ErasedAst.Expression.ArrayLit(elms0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ArrayNew(elm, len, tpe, loc) =>
      val (elm0, ftypes0) = visitExp[T](elm)
      val (len0, ftypes1) = visitExp[PInt32](len)
      val (tpe0, ftypes2) = visitTpe[PReference[PArray[T]]](tpe)
      val expRes = ErasedAst.Expression.ArrayNew(elm0, len0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ArrayLoad(base, index, tpe, loc) =>
      val (base0, ftypes0) = visitExp[PReference[PArray[T]]](base)
      val (index0, ftypes1) = visitExp[PInt32](index)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.ArrayLoad(base0, index0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes, ftypesRes)

    case FinalAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val (base0, ftypes0) = visitExp[PReference[PArray[T]]](base)
      val (index0, ftypes1) = visitExp[PInt32](index)
      val (elm0, ftypes2) = visitExp[T](elm)
      val (tpe0, ftypes3) = visitTpe[PReference[PUnit]](tpe)
      val expRes = ErasedAst.Expression.ArrayStore(base0, index0, elm0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2 union ftypes3
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ArrayLength(base, tpe, loc) =>
      val (base0, ftypes0) = visitExp[PReference[PArray[T]]](base)
      val (tpe0, ftypes1) = visitTpe[PInt32](tpe)
      val expRes = ErasedAst.Expression.ArrayLength(base0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val (base0, ftypes0) = visitExp[PReference[PArray[T]]](base)
      val (beginIndex0, ftypes1) = visitExp[PInt32](beginIndex)
      val (endIndex0, ftypes2) = visitExp[PInt32](endIndex)
      val (tpe0, ftypes3) = visitTpe[PReference[PArray[T]]](tpe)
      val expRes = ErasedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2 union ftypes3
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Ref(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PType](exp)
      val (tpe0, ftypes1) = visitTpe[PReference[PRef[PType]]](tpe)
      val expRes = ErasedAst.Expression.Ref(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PRef[T]]](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Deref(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val (exp10, ftypes0) = visitExp[PReference[PRef[T]]](exp1)
      val (exp20, ftypes1) = visitExp[T](exp2)
      val (tpe0, ftypes2) = visitTpe[PReference[PUnit]](tpe)
      val expRes = ErasedAst.Expression.Assign(exp10, exp20, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Existential(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      val (exp0, ftypes0) = visitExp[PInt32](exp)
      val (tpe0, ftypes1) = visitTpe[PType](tpe)
      val fparam0 = ErasedAst.FormalParam(sym, tpe0)
      val expRes = ErasedAst.Expression.Existential(fparam0, exp0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Universal(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      val (exp0, ftypes0) = visitExp[PInt32](exp)
      val (tpe0, ftypes1) = visitTpe[PType](tpe)
      val fparam0 = ErasedAst.FormalParam(sym, tpe0)
      val expRes = ErasedAst.Expression.Universal(fparam0, exp0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Cast(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PType](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Cast(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.TryCatch(exp, rules, tpe, loc) =>
      val (rules0, ftypes0) = rules.foldLeft((List[ErasedAst.CatchRule[T]](), emptyFTypes())) {
        case ((l, s), FinalAst.CatchRule(sym, clazz, cexp)) =>
          val (cexp0, ftypesRes) = visitExp[T](cexp)
          val expRes = ErasedAst.CatchRule[T](sym, clazz, cexp0)
          (l :+ expRes, s union ftypesRes)
      }
      val (exp0, ftypes1) = visitExp[T](exp)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.TryCatch(exp0, rules0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes, ftypesRes)

    case FinalAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val (args0, ftypes0) = visitExps[PType](args)
      val (tpe0, ftypes1) = visitTpe[PReference[PAnyObject]](tpe)
      val expRes = ErasedAst.Expression.InvokeConstructor(constructor, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PAnyObject]](exp)
      val (args0, ftypes1) = visitExps[PType](args)
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.InvokeMethod(method, exp0, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes, ftypesRes)

    case FinalAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val (args0, ftypes0) = visitExps[PType](args)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.InvokeStaticMethod(method, args0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PAnyObject]](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.GetField(field, exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val (exp10, ftypes0) = visitExp[PReference[PAnyObject]](exp1)
      val (exp20, ftypes1) = visitExp[PType](exp2)
      val (tpe0, ftypes2) = visitTpe[PReference[PUnit]](tpe)
      val expRes = ErasedAst.Expression.PutField(field, exp10, exp20, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.GetStaticField(field, tpe0, loc)
      (expRes, ftypesRes)

    case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PType](exp)
      val (tpe0, ftypes1) = visitTpe[PReference[PUnit]](tpe)
      val expRes = ErasedAst.Expression.PutStaticField(field, exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.NewChannel(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PInt32](exp)
      val (tpe0, ftypes1) = visitTpe[PReference[PChan[T]]](tpe)
      val expRes = ErasedAst.Expression.NewChannel(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.GetChannel(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PChan[T]]](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.GetChannel(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val (exp10, ftypes0) = visitExp[PReference[PChan[T]]](exp1)
      val (exp20, ftypes1) = visitExp[T](exp2)
      val (tpe0, ftypes2) = visitTpe[PReference[PChan[T]]](tpe)
      val expRes = ErasedAst.Expression.PutChannel(exp10, exp20, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.SelectChannel(rules, default, tpe, loc) =>
      val (rules0, ftypes0) = rules.foldLeft((List[ErasedAst.SelectChannelRule[T]](), emptyFTypes())) {
        case ((l, s), rule) =>
          val FinalAst.SelectChannelRule(sym, chan, exp) = rule
          val (exp0, ftypes0) = visitExp[T](exp)
          val (chan0, ftypes1) = visitExp[PReference[PChan[PType]]](chan)
          val ftypesRes = ftypes0 union ftypes1
          val expRes = ErasedAst.SelectChannelRule(sym, chan0, exp0)
          (l :+ expRes, s union ftypesRes)
      }
      val (default0, ftypes1: FTypes) = default match {
        case Some(defaultExp) =>
          val (defaultExp0, ftypes0) = visitExp[T](defaultExp)
          (Some(defaultExp0), ftypes0)
        case None => (None, Set())
      }
      val (tpe0, ftypes2) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.SelectChannel(rules0, default0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1 union ftypes2
      (expRes, ftypesRes)

    case FinalAst.Expression.Spawn(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PType](exp)
      val (tpe0, ftypes1) = visitTpe[PReference[PUnit]](tpe)
      val expRes = ErasedAst.Expression.Spawn(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Lazy(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[T](exp)
      val (tpe0, ftypes1) = visitTpe[PReference[PLazy[T]]](tpe)
      val expRes = ErasedAst.Expression.Lazy(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes.asInstanceOf[ErasedAst.Expression[T]], ftypesRes)

    case FinalAst.Expression.Force(exp, tpe, loc) =>
      val (exp0, ftypes0) = visitExp[PReference[PLazy[T]]](exp)
      val (tpe0, ftypes1) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.Force(exp0, tpe0, loc)
      val ftypesRes = ftypes0 union ftypes1
      (expRes, ftypesRes)

    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.HoleError(sym, tpe0, loc)
      (expRes, ftypesRes)

    case FinalAst.Expression.MatchError(tpe, loc) =>
      val (tpe0, ftypesRes) = visitTpe[T](tpe)
      val expRes = ErasedAst.Expression.MatchError(tpe0, loc)
      (expRes, ftypesRes)
  }

  private def visitExps[T <: PType](exps: List[FinalAst.Expression]): (List[ErasedAst.Expression[T]], FTypes) =
    exps.foldLeft((List[ErasedAst.Expression[T]](), emptyFTypes())) {
      case ((l, s), exp) => {
        val (expRes, ftypesRes) = visitExp[T](exp)
        (l :+ expRes, s union ftypesRes)
      }
    }

  /**
   * Translates the given formal param `p` to the ErasedAst.
   */
  private def visitFormalParam(p: FinalAst.FormalParam): (ErasedAst.FormalParam, FTypes) = {
    val (tpe0, ftypesRes) = visitTpe[PType](p.tpe)
    (ErasedAst.FormalParam(p.sym, tpe0), ftypesRes)
  }

  /**
   * Translates the property `p` to the ErasedAst.
   */
  private def visitProperty(p: FinalAst.Property): (ErasedAst.Property, FTypes) = {
    val (exp0, ftypesRes) = visitExp[PInt32](p.exp)
    (ErasedAst.Property(p.law, p.defn, exp0), ftypesRes)
  }

  /**
   * Translates the type 'tpe' to the ErasedType.
   */
  private def visitTpe[T <: PType](tpe: MonoType): (RType[T], FTypes) = {
    val (tpeRes, ftypesRes) = tpe match {
      case MonoType.Unit => (RReference(RUnit()), emptyFTypes())
      case MonoType.Bool => (RBool(), emptyFTypes())
      case MonoType.Char => (RChar(), emptyFTypes())
      case MonoType.Float32 => (RFloat32(), emptyFTypes())
      case MonoType.Float64 => (RFloat64(), emptyFTypes())
      case MonoType.Int8 => (RInt8(), emptyFTypes())
      case MonoType.Int16 => (RInt16(), emptyFTypes())
      case MonoType.Int32 => (RInt32(), emptyFTypes())
      case MonoType.Int64 => (RInt64(), emptyFTypes())
      case MonoType.BigInt =>
        (RReference(RBigInt()), emptyFTypes())
      case MonoType.Str =>
        (RReference(RStr()), emptyFTypes())
      case MonoType.Array(tpe) =>
        val (tpe0, ftypesRes) = visitTpe[PType](tpe)
        (RReference(RArray[PType](tpe0)), ftypesRes)
      case MonoType.Channel(tpe) =>
        val (tpe0, ftypesRes) = visitTpe[T](tpe)
        (RReference(RChannel(tpe0)), ftypesRes)
      case MonoType.Lazy(tpe) =>
        val (tpe0, ftypesRes) = visitTpe[T](tpe)
        (RReference(RLazy(tpe0)), ftypesRes)
      case MonoType.Ref(tpe) =>
        val (tpe0, ftypesRes) = visitTpe[T](tpe)
        (RReference(RRef(tpe0)), ftypesRes)
      case MonoType.Tuple(elms) =>
        val (elms0, ftypesRes) = visitTpes(elms)
        (RReference(RTuple(elms0)), ftypesRes)
      case MonoType.Enum(sym, args) =>
        val (args0, ftypesRes) = visitTpes[PType](args)
        (RReference(REnum(sym, args0)), ftypesRes)
      case MonoType.Arrow(args, result) =>
        val (args0, ftypes0) = visitTpes(args)
        val (result0, ftypes1) = visitTpe(result)
        val tpeRes = RReference(RArrow(args0, result0))
        val ftypesRes = ftypes0 union ftypes1
        (tpeRes, ftypesRes + tpeRes)
      case MonoType.RecordEmpty() =>
        (RReference(RRecordEmpty()), emptyFTypes())
      case MonoType.RecordExtend(field, value, rest) =>
        val (value0, ftypes0) = visitTpe[PType](value)
        val (rest0, ftypes1) = visitTpe[PReference[PAnyObject]](rest)
        val ftypesRes = ftypes0 union ftypes1
        (RReference(RRecordExtend(field, value0, rest0)), ftypesRes)
      case MonoType.SchemaEmpty() =>
        (RReference(RSchemaEmpty()), emptyFTypes())
      case MonoType.SchemaExtend(name, tpe, rest) =>
        val (tpe0, ftypes0) = visitTpe[PType](tpe)
        val (rest0, ftypes1) = visitTpe[PReference[PAnyObject]](rest)
        val ftypesRes = ftypes0 union ftypes1
        (RReference(RSchemaExtend(name, tpe0, rest0)), ftypesRes)
      case MonoType.Relation(tpes) =>
        val (tpes0, ftypesRes) = visitTpes[PType](tpes)
        (RReference(RRelation(tpes0)), ftypesRes)
      case MonoType.Lattice(tpes) =>
        val (tpes0, ftypesRes) = visitTpes[PType](tpes)
        (RReference(RLattice(tpes0)), ftypesRes)
      case MonoType.Native(clazz) =>
        (RReference(RNative(clazz)), emptyFTypes())
      case MonoType.Var(id) =>
        (RReference(RVar(id)), emptyFTypes())
    }
    (tpeRes.asInstanceOf[RType[T]], ftypesRes)
  }

  private def visitTpes[T <: PType](tpes: List[MonoType]): (List[RType[T]], FTypes) =
    tpes.foldLeft((List[RType[T]](), emptyFTypes())) {
      case ((l, s), tpe) => {
        val (tpe0, ftypesRes) = visitTpe[T](tpe)
        (l :+ tpe0, s union ftypesRes)
      }
    }
}
