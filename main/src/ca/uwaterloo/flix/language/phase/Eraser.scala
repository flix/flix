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
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.{RType, ErasedAst, FinalAst, MonoType, PType, Symbol}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Eraser extends Phase[FinalAst.Root, FinalAst.Root] {

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[FinalAst.Root, CompilationError] = flix.phase("Eraser") {
    val defns = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map {
      case (k, FinalAst.Enum(mod, sym, cases0, _, loc)) =>
        val cases = cases0 map {
          case (tag, FinalAst.Case(enumSym, tagName, tagTpeDeprecated, tagLoc)) => tag -> ErasedAst.Case(enumSym, tagName, visitTpe(tagTpeDeprecated), tagLoc)
        }
        k -> ErasedAst.Enum(mod, sym, cases, loc)
    }
    val properties = root.properties.map { p => visitProperty(p) }
    val reachable = root.reachable

    val actualTransformation = ErasedAst.Root(defns, enums, properties, reachable, root.sources).toSuccess
    root.toSuccess
  }

  /**
    * Translates the given definition `def0` to the ErasedAst.
    */
  private def visitDef(def0: FinalAst.Def): ErasedAst.Def = {
    val fs = def0.formals.map(visitFormalParam)
    val exp = visitExp[PType](def0.exp)
    val tpe = visitTpe[PType](def0.tpe)
    ErasedAst.Def(def0.ann, def0.mod, def0.sym, fs, exp, tpe, def0.loc)
  }

  /**
    * Translates the given expression `exp0` to the ErasedAst.
    */
  private def visitExp[T <: PType](exp0: FinalAst.Expression): ErasedAst.Expression[T] = exp0 match {
    case FinalAst.Expression.Unit(loc) =>
      ErasedAst.Expression.Unit(loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Null(tpe, loc) =>
      ErasedAst.Expression.Null[PAnyObject](visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.True(loc) =>
      ErasedAst.Expression.True(loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.False(loc) =>
      (ErasedAst.Expression.False(loc)).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Char(lit, loc) =>
      ErasedAst.Expression.Char(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Float32(lit, loc) =>
      ErasedAst.Expression.Float32(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Float64(lit, loc) =>
      ErasedAst.Expression.Float64(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Int8(lit, loc) =>
      ErasedAst.Expression.Int8(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Int16(lit, loc) =>
      ErasedAst.Expression.Int16(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Int32(lit, loc) =>
      ErasedAst.Expression.Int32(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Int64(lit, loc) =>
      ErasedAst.Expression.Int64(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.BigInt(lit, loc) =>
      ErasedAst.Expression.BigInt(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Str(lit, loc) =>
      ErasedAst.Expression.Str(lit, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Var(sym, tpe, loc) =>
      ErasedAst.Expression.Var(sym, visitTpe(tpe), loc)

    case FinalAst.Expression.Closure(sym, freeVars, _, tpe, loc) =>
      val newFreeVars = freeVars.map { case FinalAst.FreeVar(sym, tpe) => ErasedAst.FreeVar(sym, visitTpe(tpe)) }
      ErasedAst.Expression.Closure(sym, newFreeVars, visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyClo(exp, args, tpe, loc) =>
      ErasedAst.Expression.ApplyClo(visitExp(exp), args.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyDef(sym, args, tpe, loc) =>
      ErasedAst.Expression.ApplyDef(sym, args.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
      ErasedAst.Expression.ApplyCloTail(visitExp(exp), args.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
      ErasedAst.Expression.ApplyDefTail(sym, args.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val newFormals = formals.map { case FinalAst.FormalParam(sym, tpe) => ErasedAst.FormalParam(sym, visitTpe(tpe)) }
      ErasedAst.Expression.ApplySelfTail(sym, newFormals, actuals.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Unary(sop, op, exp, tpe, loc) =>
      ErasedAst.Expression.Unary(sop, op, visitExp(exp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.Binary(sop, op, visitExp(exp1), visitExp(exp2), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      ErasedAst.Expression.IfThenElse(visitExp(exp1), visitExp[T](exp2), visitExp[T](exp3), visitTpe(tpe), loc)

    case FinalAst.Expression.Branch(exp, branches, tpe, loc) =>
      val newBranches = branches.map { case (label, branchExp) => (label, visitExp[T](branchExp)) }
      ErasedAst.Expression.Branch(visitExp(exp), newBranches, visitTpe(tpe), loc)

    case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
      ErasedAst.Expression.JumpTo(sym, visitTpe(tpe), loc)

    case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.Let(sym, visitExp(exp1), visitExp(exp2), visitTpe(tpe), loc)

    case FinalAst.Expression.Is(sym, tag, exp, loc) =>
      ErasedAst.Expression.Is(sym, tag, visitExp(exp), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
      ErasedAst.Expression.Tag(sym, tag, visitExp(exp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
      ErasedAst.Expression.Untag(sym, tag, visitExp(exp), visitTpe(tpe), loc)

    case FinalAst.Expression.Index(base, offset, tpe, loc) =>
      val e: ErasedAst.Expression[PType] = ErasedAst.Expression.Index(visitExp(base), offset, visitTpe(tpe), loc)
      ErasedAst.Expression.Cast(e, visitTpe(tpe), loc)

    case FinalAst.Expression.Tuple(elms, tpe, loc) =>
      ErasedAst.Expression.Tuple(elms.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordEmpty(tpe, loc) =>
      ErasedAst.Expression.RecordEmpty(visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      ErasedAst.Expression.RecordSelect(visitExp(exp), field, visitTpe(tpe), loc)

    case FinalAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
      ErasedAst.Expression.RecordExtend(field, visitExp(value), visitExp(rest), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
      ErasedAst.Expression.RecordRestrict(field, visitExp(rest), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLit(elms, tpe, loc) =>
      ErasedAst.Expression.ArrayLit(elms.map(visitExp[PType]), visitTpe[PReference[PArray[PType]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayNew(elm, len, tpe, loc) =>
      ErasedAst.Expression.ArrayNew(visitExp[PType](elm), visitExp(len), visitTpe[PReference[PArray[PType]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLoad(base, index, tpe, loc) =>
      ErasedAst.Expression.ArrayLoad(visitExp[PReference[PArray[T]]](base), visitExp(index), visitTpe(tpe), loc)

    case FinalAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
      ErasedAst.Expression.ArrayStore(visitExp[PReference[PArray[PType]]](base), visitExp(index), visitExp(elm), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLength(base, tpe, loc) =>
      ErasedAst.Expression.ArrayLength(visitExp[PReference[PArray[PType]]](base), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      ErasedAst.Expression.ArraySlice(visitExp[PReference[PArray[PType]]](base), visitExp(beginIndex), visitExp(endIndex), visitTpe[PReference[PArray[PType]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Ref(exp, tpe, loc) =>
      val tpe0 = visitTpe[PReference[PRef[PType]]](tpe)
      ErasedAst.Expression.Ref(visitExp(exp), tpe0, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      val tpe0 = visitTpe[T](tpe)
      val refValue = visitExp[PReference[PRef[T]]](exp)
      ErasedAst.Expression.Deref(refValue, tpe0, loc)

    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val tpe0 = visitTpe[PReference[PUnit]](tpe)
      ErasedAst.Expression.Assign(visitExp[PReference[PRef[PType]]](exp1), visitExp(exp2), tpe0, loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Existential(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      ErasedAst.Expression.Existential(ErasedAst.FormalParam(sym, visitTpe(tpe)), visitExp(exp), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Universal(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      ErasedAst.Expression.Universal(ErasedAst.FormalParam(sym, visitTpe(tpe)), visitExp(exp), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Cast(exp, tpe, loc) =>
      ErasedAst.Expression.Cast(visitExp(exp), visitTpe(tpe), loc)

    case FinalAst.Expression.TryCatch(exp, rules, tpe, loc) =>
      val newRules = rules.map { case FinalAst.CatchRule(sym, clazz, exp) =>
        ErasedAst.CatchRule[T](sym, clazz, visitExp(exp))
      }
      ErasedAst.Expression.TryCatch(visitExp(exp), newRules, visitTpe(tpe), loc)

    case FinalAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      ErasedAst.Expression.InvokeConstructor(constructor, args.map(visitExp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      ErasedAst.Expression.InvokeMethod(method, visitExp(exp), args.map(visitExp), visitTpe(tpe), loc)

    case FinalAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      ErasedAst.Expression.InvokeStaticMethod(method, args.map(visitExp), visitTpe(tpe), loc)

    case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
      ErasedAst.Expression.GetField(field, visitExp(exp), visitTpe(tpe), loc)

    case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.PutField(field, visitExp(exp1), visitExp(exp2), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
      ErasedAst.Expression.GetStaticField(field, visitTpe(tpe), loc)

    case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      ErasedAst.Expression.PutStaticField(field, visitExp(exp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.NewChannel(exp, tpe, loc) =>
      ErasedAst.Expression.NewChannel(visitExp(exp), visitTpe[PReference[PChan[T]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.GetChannel(exp, tpe, loc) =>
      ErasedAst.Expression.GetChannel(visitExp(exp), visitTpe(tpe), loc)

    case FinalAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.PutChannel(visitExp[PReference[PChan[PType]]](exp1), visitExp(exp2), visitTpe[PReference[PChan[PType]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.SelectChannel(rules, default, tpe, loc) =>
      val newRules = rules.map { case FinalAst.SelectChannelRule(sym, chan, exp) =>
        ErasedAst.SelectChannelRule[T](sym, visitExp(chan), visitExp(exp))
      }
      ErasedAst.Expression.SelectChannel(newRules, default.map(visitExp[T]), visitTpe(tpe), loc)

    case FinalAst.Expression.Spawn(exp, tpe, loc) =>
      ErasedAst.Expression.Spawn(visitExp(exp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Lazy(exp, tpe, loc) =>
      ErasedAst.Expression.Lazy(visitExp(exp), visitTpe[PReference[PLazy[PType]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Force(exp, tpe, loc) =>
      ErasedAst.Expression.Force(visitExp(exp), visitTpe(tpe), loc)

    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      ErasedAst.Expression.HoleError(sym, visitTpe(tpe), loc)

    case FinalAst.Expression.MatchError(tpe, loc) =>
      ErasedAst.Expression.MatchError(visitTpe(tpe), loc)
  }

  /**
    * Translates the given formal param `p` to the ErasedAst.
    */
  private def visitFormalParam(p: FinalAst.FormalParam): ErasedAst.FormalParam =
    ErasedAst.FormalParam(p.sym, visitTpe(p.tpe))

  /**
    * Translates the property `p` to the ErasedAst.
    */
  private def visitProperty(p: FinalAst.Property): ErasedAst.Property =
    ErasedAst.Property(p.law, p.defn, visitExp(p.exp))

  /**
    * Translates the type 'tpe' to the ErasedType.
    */
  private def visitTpe[T <: PType](tpe: MonoType): RType[T] = (tpe match {
    case MonoType.Unit => RReference(RUnit())
    case MonoType.Bool => RBool()
    case MonoType.Char => RChar()
    case MonoType.Float32 => RFloat32()
    case MonoType.Float64 => RFloat64()
    case MonoType.Int8 => RInt8()
    case MonoType.Int16 => RInt16()
    case MonoType.Int32 => RInt32()
    case MonoType.Int64 => RInt64()
    case MonoType.BigInt =>
      RReference(RBigInt())
    case MonoType.Str =>
      RReference(RStr())
    case MonoType.Array(tpe) =>
      RReference(RArray[PType](visitTpe[PType](tpe)))
    case MonoType.Channel(tpe) =>
      RReference(RChannel(visitTpe(tpe)))
    case MonoType.Lazy(tpe) =>
      RReference(RLazy(visitTpe(tpe)))
    case MonoType.Ref(tpe) =>
      RReference(RRef(visitTpe(tpe)))
    case MonoType.Tuple(elms) =>
      RReference(RTuple(elms.map(visitTpe)))
    case MonoType.Enum(sym, args) =>
      RReference(REnum(sym, args.map(visitTpe)))
    case MonoType.Arrow(args, result) =>
      RReference(RArrow(args.map(visitTpe), visitTpe(result)))
    case MonoType.RecordEmpty() =>
      RReference(RRecordEmpty())
    case MonoType.RecordExtend(field, value, rest) =>
      RReference(RRecordExtend(field, visitTpe(value), visitTpe(rest)))
    case MonoType.SchemaEmpty() =>
      RReference(RSchemaEmpty())
    case MonoType.SchemaExtend(name, tpe, rest) =>
      RReference(RSchemaExtend(name, visitTpe(tpe), visitTpe(rest)))
    case MonoType.Relation(tpes) =>
      RReference(RRelation(tpes.map(visitTpe)))
    case MonoType.Lattice(tpes) =>
      RReference(RLattice(tpes.map(visitTpe)))
    case MonoType.Native(clazz) =>
      RReference(RNative(clazz))
    case MonoType.Var(id) =>
      RReference(RVar(id))
  }).asInstanceOf[RType[T]]
}
