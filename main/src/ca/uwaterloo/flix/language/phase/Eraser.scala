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
import ca.uwaterloo.flix.language.ast.{ERefType, EType, ErasedAst, FinalAst, MonoType, PRefType, PType, Symbol}
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
    val latticeOps: Map[EType[PType], ErasedAst.LatticeOps] = root.latticeOps.map { case (k, v) => visitTpe[PType](k) -> visitLatticeOps(v) }
    val properties = root.properties.map { p => visitProperty(p) }
    val specialOps = root.specialOps.map { case (k1, m) => k1 -> m.map[EType[PType], Symbol.DefnSym] { case (k2, v) => visitTpe[PType](k2) -> v } }
    val reachable = root.reachable

    val actualTransformation = ErasedAst.Root(defns, enums, latticeOps, properties, specialOps, reachable, root.sources).toSuccess
    root.toSuccess
  }

  /**
    * Translates the given `constraint0` to the ErasedAst.
    */
  private def visitConstraint(constraint0: FinalAst.Constraint): ErasedAst.Constraint = {
    val cparams = constraint0.cparams.map {
      case FinalAst.ConstraintParam.HeadParam(sym, tpe, loc) => ErasedAst.ConstraintParam.HeadParam(sym, visitTpe(tpe), loc)
      case FinalAst.ConstraintParam.RuleParam(sym, tpe, loc) => ErasedAst.ConstraintParam.RuleParam(sym, visitTpe(tpe), loc)
    }
    val head = visitHeadPred(constraint0.head)
    val body = constraint0.body.map(visitBodyPred)

    ErasedAst.Constraint(cparams, head, body, constraint0.loc)
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
    case FinalAst.Expression.Unit(loc) => ErasedAst.Expression.Unit(loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Null(tpe, loc) => ErasedAst.Expression.Null[PRefType.PAnyObject](visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.True(loc) => ErasedAst.Expression.True(loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.False(loc) => (ErasedAst.Expression.False(loc)).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Char(lit, loc) => ErasedAst.Expression.Char(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Float32(lit, loc) => ErasedAst.Expression.Float32(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Float64(lit, loc) => ErasedAst.Expression.Float64(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Int8(lit, loc) => ErasedAst.Expression.Int8(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Int16(lit, loc) => ErasedAst.Expression.Int16(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Int32(lit, loc) => ErasedAst.Expression.Int32(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Int64(lit, loc) => ErasedAst.Expression.Int64(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.BigInt(lit, loc) => ErasedAst.Expression.BigInt(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Str(lit, loc) => ErasedAst.Expression.Str(lit, loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Var(sym, tpe, loc) => ErasedAst.Expression.Var(sym, visitTpe(tpe), loc)
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
      ErasedAst.Expression.Ref(visitExp(exp), visitTpe[PReference[PRef[PType]]](tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      ErasedAst.Expression.Deref(visitExp(exp), visitTpe(tpe), loc)
    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.Assign(visitExp[PReference[PRef[PType]]](exp1), visitExp(exp2), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
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
    case FinalAst.Expression.FixpointConstraintSet(cs, tpe, loc) =>
      val newCs = cs.map(visitConstraint)
      ErasedAst.Expression.FixpointConstraintSet(newCs, visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.FixpointCompose(visitExp(exp1), visitExp(exp2), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.FixpointSolve(exp, stf, tpe, loc) =>
      ErasedAst.Expression.FixpointSolve(visitExp(exp), stf, visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
      ErasedAst.Expression.FixpointProject(pred, visitExp(exp), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.FixpointEntails(visitExp(exp1), visitExp(exp2), visitTpe(tpe), loc).asInstanceOf[ErasedAst.Expression[T]]
    case FinalAst.Expression.FixpointFold(pred, init, f, constraints, tpe, loc) =>
      def visitVar[TT <: PType](v: FinalAst.Expression.Var): ErasedAst.Expression.Var[TT] =
        visitExp(v).asInstanceOf[ErasedAst.Expression.Var[TT]]

      ErasedAst.Expression.FixpointFold(pred, visitVar(init), visitVar(f), visitVar(constraints), visitTpe(tpe), loc)
    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      ErasedAst.Expression.HoleError(sym, visitTpe(tpe), loc)
    case FinalAst.Expression.MatchError(tpe, loc) =>
      ErasedAst.Expression.MatchError(visitTpe(tpe), loc)
  }

  /**
    * Translates the given `head` predicate to the ErasedAst.
    */
  private def visitHeadPred(head: FinalAst.Predicate.Head): ErasedAst.Predicate.Head = head match {
    case FinalAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
      ErasedAst.Predicate.Head.Atom(pred, den, terms.map(visitTermHead), visitTpe(tpe), loc)

    case FinalAst.Predicate.Head.Union(exp, terms, tpe, loc) =>
      ErasedAst.Predicate.Head.Union(visitExp(exp), terms.map(visitTermHead), visitTpe(tpe), loc)
  }

  /**
    * Translates the given `body` predicate to the ErasedAst.
    */
  private def visitBodyPred(body: FinalAst.Predicate.Body): ErasedAst.Predicate.Body = body match {
    case FinalAst.Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      ErasedAst.Predicate.Body.Atom(pred, den, polarity, terms.map(visitTermBody), visitTpe(tpe), loc)

    case FinalAst.Predicate.Body.Guard(exp, terms, loc) =>
      ErasedAst.Predicate.Body.Guard(visitExp(exp), terms.map(visitTermBody), loc)
  }

  /**
    * Translates the given 'head' term to the ErasedAst.
    */
  private def visitTermHead(head: FinalAst.Term.Head): ErasedAst.Term.Head = head match {
    case FinalAst.Term.Head.QuantVar(sym, tpe, loc) =>
      ErasedAst.Term.Head.QuantVar(sym, visitTpe(tpe), loc)
    case FinalAst.Term.Head.CapturedVar(sym, tpe, loc) =>
      ErasedAst.Term.Head.CapturedVar(sym, visitTpe(tpe), loc)
    case FinalAst.Term.Head.Lit(sym, tpe, loc) =>
      ErasedAst.Term.Head.Lit(sym, visitTpe(tpe), loc)
    case FinalAst.Term.Head.App(exp, args, tpe, loc) =>
      ErasedAst.Term.Head.App(visitExp(exp), args, visitTpe(tpe), loc)
  }

  /**
    * Translates the given `body` term to the ErasedAst.
    */
  private def visitTermBody(body: FinalAst.Term.Body): ErasedAst.Term.Body = body match {
    case FinalAst.Term.Body.Wild(tpe, loc) =>
      ErasedAst.Term.Body.Wild(visitTpe(tpe), loc)
    case FinalAst.Term.Body.QuantVar(sym, tpe, loc) =>
      ErasedAst.Term.Body.QuantVar(sym, visitTpe(tpe), loc)
    case FinalAst.Term.Body.CapturedVar(sym, tpe, loc) =>
      ErasedAst.Term.Body.CapturedVar(sym, visitTpe(tpe), loc)
    case FinalAst.Term.Body.Lit(sym, tpe, loc) =>
      ErasedAst.Term.Body.Lit(sym, visitTpe(tpe), loc)
  }

  /**
    * Translates the given `lattice0` to the ErasedAst.
    */
  private def visitLatticeOps(lattice0: FinalAst.LatticeOps): ErasedAst.LatticeOps = lattice0 match {
    case FinalAst.LatticeOps(tpe, bot, equ, leq, lub, glb) =>
      ErasedAst.LatticeOps(visitTpe(tpe), bot, equ, leq, lub, glb)
  }

  /**
    * Translates the given attribute `a` to the ErasedAst.
    */
  private def visitAttribute(a: FinalAst.Attribute): ErasedAst.Attribute =
    ErasedAst.Attribute(a.name, visitTpe(a.tpe))

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
  private def visitTpe[T <: PType](tpe: MonoType): EType[T] = (tpe match {
    case MonoType.Unit => EType.Reference(ERefType.Unit())
    case MonoType.Bool => EType.Bool()
    case MonoType.Char => EType.Char()
    case MonoType.Float32 => EType.Float32()
    case MonoType.Float64 => EType.Float64()
    case MonoType.Int8 => EType.Int8()
    case MonoType.Int16 => EType.Int16()
    case MonoType.Int32 => EType.Int32()
    case MonoType.Int64 => EType.Int64()
    case MonoType.BigInt =>
      EType.Reference(ERefType.BigInt())
    case MonoType.Str =>
      EType.Reference(ERefType.Str())
    case MonoType.Array(tpe) =>
      EType.Reference(ERefType.Array[PType](visitTpe[PType](tpe)))
    case MonoType.Channel(tpe) =>
      EType.Reference(ERefType.Channel(visitTpe(tpe)))
    case MonoType.Lazy(tpe) =>
      EType.Reference(ERefType.Lazy(visitTpe(tpe)))
    case MonoType.Ref(tpe) =>
      EType.Reference(ERefType.Ref(visitTpe(tpe)))
    case MonoType.Tuple(elms) =>
      EType.Reference(ERefType.Tuple(elms.map(visitTpe)))
    case MonoType.Enum(sym, args) =>
      EType.Reference(ERefType.Enum(sym, args.map(visitTpe)))
    case MonoType.Arrow(args, result) =>
      EType.Reference(ERefType.Arrow(args.map(visitTpe), visitTpe(result)))
    case MonoType.RecordEmpty() =>
      EType.Reference(ERefType.RecordEmpty())
    case MonoType.RecordExtend(field, value, rest) =>
      EType.Reference(ERefType.RecordExtend(field, visitTpe(value), visitTpe(rest)))
    case MonoType.SchemaEmpty() =>
      EType.Reference(ERefType.SchemaEmpty())
    case MonoType.SchemaExtend(name, tpe, rest) =>
      EType.Reference(ERefType.SchemaExtend(name, visitTpe(tpe), visitTpe(rest)))
    case MonoType.Relation(tpes) =>
      EType.Reference(ERefType.Relation(tpes.map(visitTpe)))
    case MonoType.Lattice(tpes) =>
      EType.Reference(ERefType.Lattice(tpes.map(visitTpe)))
    case MonoType.Native(clazz) =>
      EType.Reference(ERefType.Native(clazz))
    case MonoType.Var(id) =>
      EType.Reference(ERefType.Var(id))
  }).asInstanceOf[EType[T]]
}
