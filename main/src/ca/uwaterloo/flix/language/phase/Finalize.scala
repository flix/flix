/*
 * Copyright 2015-2016 Ming-Ho Yee
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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

object Finalize extends Phase[LiftedAst.Root, FinalAst.Root] {

  private type TopLevel = mutable.Map[Symbol.DefnSym, FinalAst.Def]

  def run(root: LiftedAst.Root)(implicit flix: Flix): Validation[FinalAst.Root, CompilationError] = flix.phase("Finalize") {

    val m: TopLevel = mutable.Map.empty

    val defs = root.defs.map {
      case (k, v) => k -> visitDef(v, m)
    }

    val enums = root.enums.map {
      case (sym, enum) => sym -> visitEnum(enum, m)
    }

    val latticesOps = root.latticeOps.map {
      case (k, v) => visitType(k) -> visitLatticeOps(v, m)
    }

    val properties = root.properties.map(p => visitProperty(p, m))

    val specialOps = root.specialOps.map {
      case (op, ops) => op -> ops.map {
        case (tpe, sym) => visitType(tpe) -> sym
      }
    }

    val reachable = root.reachable

    FinalAst.Root(defs ++ m, enums, latticesOps, properties, specialOps, reachable, root.sources).toSuccess
  }

  private def visitDef(def0: LiftedAst.Def, m: TopLevel)(implicit flix: Flix): FinalAst.Def = {
    val fs = def0.fparams.map(visitFormalParam)
    val e = visitExp(def0.exp, m)
    val tpe = visitType(def0.tpe)
    FinalAst.Def(def0.ann, def0.mod, def0.sym, fs, e, tpe, def0.loc)
  }

  private def visitEnum(enum0: LiftedAst.Enum, m: TopLevel)(implicit flix: Flix): FinalAst.Enum = enum0 match {
    case LiftedAst.Enum(mod, sym, cases0, tpe0, loc) =>
      val cases = cases0.map {
        case (tag, LiftedAst.Case(enumSym, tagName, tagType, tagLoc)) =>
          val tpe = visitType(tagType)
          tag -> FinalAst.Case(enumSym, tagName, tpe, tagLoc)
      }
      val tpe = visitType(tpe0)
      FinalAst.Enum(mod, sym, cases, tpe, loc)
  }

  private def visitConstraint(constraint0: LiftedAst.Constraint, m: TopLevel)(implicit flix: Flix): FinalAst.Constraint = {
    val head = visitHeadPredicate(constraint0.cparams, constraint0.head, m)
    val body = constraint0.body.map(b => visitBodyPredicate(constraint0.cparams, b, m))
    val cparams = constraint0.cparams.map {
      case LiftedAst.ConstraintParam.HeadParam(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.ConstraintParam.HeadParam(sym, tpe, loc)

      case LiftedAst.ConstraintParam.RuleParam(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.ConstraintParam.RuleParam(sym, tpe, loc)
    }
    FinalAst.Constraint(cparams, head, body, constraint0.loc)
  }

  private def visitLatticeOps(lc: LiftedAst.LatticeOps, m: TopLevel)(implicit flix: Flix): FinalAst.LatticeOps = lc match {
    case LiftedAst.LatticeOps(tpe0, bot, equ, leq, lub, glb) =>
      val tpe = visitType(tpe0)
      FinalAst.LatticeOps(tpe, bot, equ, leq, lub, glb)
  }

  private def visitExp(exp0: LiftedAst.Expression, m: TopLevel)(implicit flix: Flix): FinalAst.Expression = {

    def visit(e0: LiftedAst.Expression): FinalAst.Expression = e0 match {
      case LiftedAst.Expression.Unit(loc) =>
        FinalAst.Expression.Unit(loc)

      case LiftedAst.Expression.Null(tpe, loc) =>
        FinalAst.Expression.Null(visitType(tpe), loc)

      case LiftedAst.Expression.True(loc) =>
        FinalAst.Expression.True(loc)

      case LiftedAst.Expression.False(loc) =>
        FinalAst.Expression.False(loc)

      case LiftedAst.Expression.Char(lit, loc) =>
        FinalAst.Expression.Char(lit, loc)

      case LiftedAst.Expression.Float32(lit, loc) =>
        FinalAst.Expression.Float32(lit, loc)

      case LiftedAst.Expression.Float64(lit, loc) =>
        FinalAst.Expression.Float64(lit, loc)

      case LiftedAst.Expression.Int8(lit, loc) =>
        FinalAst.Expression.Int8(lit, loc)

      case LiftedAst.Expression.Int16(lit, loc) =>
        FinalAst.Expression.Int16(lit, loc)

      case LiftedAst.Expression.Int32(lit, loc) =>
        FinalAst.Expression.Int32(lit, loc)

      case LiftedAst.Expression.Int64(lit, loc) =>
        FinalAst.Expression.Int64(lit, loc)

      case LiftedAst.Expression.BigInt(lit, loc) =>
        FinalAst.Expression.BigInt(lit, loc)

      case LiftedAst.Expression.Str(lit, loc) =>
        FinalAst.Expression.Str(lit, loc)

      case LiftedAst.Expression.Var(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.Var(sym, t, loc)

      case LiftedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars.map(visitFreeVar)
        val t = visitType(tpe)
        FinalAst.Expression.Closure(sym, fvs, getFunctionTypeTemporaryToBeRemoved(fvs, t), t, loc)

      case LiftedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyClo(visit(exp), as, t, loc)

      case LiftedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyDef(name, as, t, loc)

      case LiftedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val e = visit(exp)
        val rs = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyCloTail(e, rs, t, loc)

      case LiftedAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyDefTail(sym, as, t, loc)

      case LiftedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
        val fs = formals.map(visitFormalParam)
        val as = actuals.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.ApplySelfTail(name, fs, as, t, loc)

      case LiftedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Unary(sop, op, e, t, loc)

      case LiftedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Binary(sop, op, e1, e2, t, loc)

      case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val v3 = visit(exp3)
        val t = visitType(tpe)
        FinalAst.Expression.IfThenElse(e1, e2, v3, t, loc)

      case LiftedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        val t = visitType(tpe)
        FinalAst.Expression.Branch(e, bs, t, loc)

      case LiftedAst.Expression.JumpTo(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.JumpTo(sym, t, loc)

      case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Let(sym, e1, e2, t, loc)

      case LiftedAst.Expression.Is(sym, tag, exp, loc) =>
        val e1 = visit(exp)
        FinalAst.Expression.Is(sym, tag, e1, loc)

      case LiftedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Tag(enum, tag, e, t, loc)

      case LiftedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Untag(sym, tag, e, t, loc)

      case LiftedAst.Expression.Index(base, offset, tpe, loc) =>
        val b = visit(base)
        val t = visitType(tpe)
        FinalAst.Expression.Index(b, offset, t, loc)

      case LiftedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visit
        val t = visitType(tpe)
        FinalAst.Expression.Tuple(es, t, loc)

      case LiftedAst.Expression.RecordEmpty(tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.RecordEmpty(t, loc)

      case LiftedAst.Expression.RecordSelect(exp, field, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.RecordSelect(e, field, t, loc)

      case LiftedAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
        val v = visit(value)
        val r = visit(rest)
        val t = visitType(tpe)
        FinalAst.Expression.RecordExtend(field, v, r, t, loc)

      case LiftedAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
        val r = visit(rest)
        val t = visitType(tpe)
        FinalAst.Expression.RecordRestrict(field, r, t, loc)

      case LiftedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visit
        val t = visitType(tpe)
        FinalAst.Expression.ArrayLit(es, t, loc)

      case LiftedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visit(elm)
        val l = visit(len)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayNew(e, l, t, loc)

      case LiftedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayLoad(b, i, t, loc)

      case LiftedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayStore(b, i, e, t, loc)

      case LiftedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayLength(b, t, loc)

      case LiftedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visit(base)
        val i1 = visit(startIndex)
        val i2 = visit(endIndex)
        val t = visitType(tpe)
        FinalAst.Expression.ArraySlice(b, i1, i2, t, loc)

      case LiftedAst.Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Ref(e, t, loc)

      case LiftedAst.Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Deref(e, t, loc)

      case LiftedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Assign(e1, e2, t, loc)

      case LiftedAst.Expression.Existential(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Existential(p, e, loc)

      case LiftedAst.Expression.Universal(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Universal(p, e, loc)

      case LiftedAst.Expression.Cast(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Cast(e, t, loc)

      case LiftedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visit(exp)
        val rs = rules map {
          case LiftedAst.CatchRule(sym, clazz, body) =>
            val b = visit(body)
            FinalAst.CatchRule(sym, clazz, b)
        }
        val t = visitType(tpe)
        FinalAst.Expression.TryCatch(e, rs, t, loc)

      case LiftedAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.InvokeConstructor(constructor, as, t, loc)

      case LiftedAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = visit(exp)
        val as = args.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.InvokeMethod(method, e, as, t, loc)

      case LiftedAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.InvokeStaticMethod(method, as, t, loc)

      case LiftedAst.Expression.GetField(field, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.GetField(field, e, t, loc)

      case LiftedAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.PutField(field, e1, e2, t, loc)

      case LiftedAst.Expression.GetStaticField(field, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.GetStaticField(field, t, loc)

      case LiftedAst.Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.PutStaticField(field, e, t, loc)

      case LiftedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.NewChannel(e, t, loc)

      case LiftedAst.Expression.GetChannel(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.GetChannel(e, t, loc)

      case LiftedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.PutChannel(e1, e2, t, loc)

      case LiftedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case LiftedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visit(chan)
            val e = visit(exp)
            FinalAst.SelectChannelRule(sym, c, e)
        }
        val d = default.map(exp => visit(exp))
        val t = visitType(tpe)
        FinalAst.Expression.SelectChannel(rs, d, t, loc)

      case LiftedAst.Expression.Spawn(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Spawn(e, t, loc)

      case LiftedAst.Expression.Lazy(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Lazy(e, t, loc)

      case LiftedAst.Expression.Force(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Force(e, t, loc)

      case LiftedAst.Expression.FixpointConstraintSet(cs0, tpe, loc) =>
        val cs = cs0.map(visitConstraint(_, m))
        val t = visitType(tpe)
        FinalAst.Expression.FixpointConstraintSet(cs, t, loc)

      case LiftedAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointCompose(e1, e2, t, loc)

      case LiftedAst.Expression.FixpointSolve(exp, stf, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointSolve(e, stf, t, loc)

      case LiftedAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointProject(pred, e, t, loc)

      case LiftedAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointEntails(e1, e2, t, loc)

      case LiftedAst.Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1).asInstanceOf[FinalAst.Expression.Var]
        val e2 = visit(exp2).asInstanceOf[FinalAst.Expression.Var]
        val e3 = visit(exp3).asInstanceOf[FinalAst.Expression.Var]
        val t = visitType(tpe)
        FinalAst.Expression.FixpointFold(pred, e1, e2, e3, t, loc)

      case LiftedAst.Expression.HoleError(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.HoleError(sym, t, loc)

      case LiftedAst.Expression.MatchError(tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.MatchError(t, loc)
    }

    visit(exp0)
  }

  private def visitHeadPredicate(cparams0: List[LiftedAst.ConstraintParam], head0: LiftedAst.Predicate.Head, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Head = head0 match {
    case LiftedAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
      val ts = terms.map(t => visitHeadTerm(t, m))
      val t = visitType(tpe)
      FinalAst.Predicate.Head.Atom(pred, den, ts, t, loc)

    case LiftedAst.Predicate.Head.Union(exp, tpe, loc) =>
      val e = visitExp(exp, m)
      val ts = cparams0.map(cparam => FinalAst.Term.Head.QuantVar(cparam.sym, visitType(cparam.tpe), cparam.loc))
      val t = visitType(tpe)
      FinalAst.Predicate.Head.Union(e, ts, t, loc)
  }

  private def visitBodyPredicate(cparams0: List[LiftedAst.ConstraintParam], body0: LiftedAst.Predicate.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Body = body0 match {
    case LiftedAst.Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val ts = terms.map(t => visitBodyTerm(t, m))
      val t = visitType(tpe)
      FinalAst.Predicate.Body.Atom(pred, den, polarity, ts, t, loc)

    case LiftedAst.Predicate.Body.Guard(exp, loc) =>
      val e = visitExp(exp, m)
      val ts = cparams0.map(cparam => FinalAst.Term.Body.QuantVar(cparam.sym, visitType(cparam.tpe), cparam.loc))
      FinalAst.Predicate.Body.Guard(e, ts, loc)
  }

  private def visitHeadTerm(t0: LiftedAst.Term.Head, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Head = t0 match {
    case LiftedAst.Term.Head.QuantVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.QuantVar(sym, t, loc)

    case LiftedAst.Term.Head.CapturedVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.CapturedVar(sym, t, loc)

    case LiftedAst.Term.Head.Lit(lit, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.Lit(lit2symTemporaryToBeRemoved(lit, loc, m), t, loc)

    case LiftedAst.Term.Head.App(exp, args, tpe, loc) =>
      val e = visitExp(exp, m)
      val t = visitType(tpe)
      FinalAst.Term.Head.App(e, args, t, loc)
  }

  private def visitBodyTerm(t0: LiftedAst.Term.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Body = t0 match {
    case LiftedAst.Term.Body.Wild(tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.Wild(t, loc)

    case LiftedAst.Term.Body.QuantVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.QuantVar(sym, t, loc)

    case LiftedAst.Term.Body.CapturedVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.CapturedVar(sym, t, loc)

    case LiftedAst.Term.Body.Lit(lit, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.Lit(lit2symTemporaryToBeRemoved(lit, loc, m), t, loc)
  }

  private def visitAttribute(a0: LiftedAst.Attribute): FinalAst.Attribute = {
    val tpe = visitType(a0.tpe)
    FinalAst.Attribute(a0.name, tpe)
  }

  private def visitFormalParam(p0: LiftedAst.FormalParam): FinalAst.FormalParam = {
    val tpe = visitType(p0.tpe)
    FinalAst.FormalParam(p0.sym, tpe)
  }

  private def visitFreeVar(v0: LiftedAst.FreeVar): FinalAst.FreeVar = {
    val tpe = visitType(v0.tpe)
    FinalAst.FreeVar(v0.sym, tpe)
  }

  private def visitProperty(p0: LiftedAst.Property, m: TopLevel)(implicit flix: Flix): FinalAst.Property =
    FinalAst.Property(p0.law, p0.defn, visitExp(p0.exp, m))

  // TODO: Should be private
  def visitType(t0: Type): MonoType = {
    val base = t0.typeConstructor
    val args = t0.typeArguments.map(visitType)

    base match {
      case None => t0 match {
        case Type.Var(id, _, _, _) => MonoType.Var(id)
        case _ => throw InternalCompilerException(s"Unexpected type: '$t0'.")
      }

      case Some(tc) =>
        tc match {
          case TypeConstructor.Unit => MonoType.Unit

          case TypeConstructor.Null => MonoType.Unit

          case TypeConstructor.Bool => MonoType.Bool

          case TypeConstructor.Char => MonoType.Char

          case TypeConstructor.Float32 => MonoType.Float32

          case TypeConstructor.Float64 => MonoType.Float64

          case TypeConstructor.Int8 => MonoType.Int8

          case TypeConstructor.Int16 => MonoType.Int16

          case TypeConstructor.Int32 => MonoType.Int32

          case TypeConstructor.Int64 => MonoType.Int64

          case TypeConstructor.BigInt => MonoType.BigInt

          case TypeConstructor.Str => MonoType.Str

          case TypeConstructor.Relation => MonoType.Relation(args)

          case TypeConstructor.Lattice => MonoType.Lattice(args)

          case TypeConstructor.RecordEmpty => MonoType.RecordEmpty()

          case TypeConstructor.SchemaEmpty => MonoType.SchemaEmpty()

          case TypeConstructor.Array => MonoType.Array(args.head)

          case TypeConstructor.Channel => MonoType.Channel(args.head)

          case TypeConstructor.Lazy => MonoType.Lazy(args.head)

          case TypeConstructor.Enum(sym, _) => MonoType.Enum(sym, args)

          case TypeConstructor.Tag(sym, _) => throw InternalCompilerException(s"Unexpected type: '$t0'.")

          case TypeConstructor.Native(clazz) => MonoType.Native(clazz)

          case TypeConstructor.Ref => MonoType.Ref(args.head)

          case TypeConstructor.Tuple(l) => MonoType.Tuple(args)

          case TypeConstructor.Arrow(l) => MonoType.Arrow(args.drop(1).init, args.last)

          case TypeConstructor.RecordExtend(field) => MonoType.RecordExtend(field.name, args.head, args(1))

          case TypeConstructor.SchemaExtend(pred) => MonoType.SchemaExtend(pred.name, args.head, args(1))

          case TypeConstructor.True => MonoType.Unit

          case TypeConstructor.False => MonoType.Unit

          case TypeConstructor.Not => MonoType.Unit

          case TypeConstructor.And => MonoType.Unit

          case TypeConstructor.Or => MonoType.Unit
        }
    }
  }

  // TODO: Deprecated
  private def getFunctionTypeTemporaryToBeRemoved(fvs: List[FinalAst.FreeVar], tpe: MonoType): MonoType = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      val freeArgs = fvs.map(_.tpe)
      MonoType.Arrow(freeArgs ::: targs, tresult)
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  // TODO: Deprecated
  // TODO: This should be done in a prior phase, perhaps during lambda lifting, or not done at all...
  private def lit2symTemporaryToBeRemoved(exp0: LiftedAst.Expression, loc: SourceLocation, m: TopLevel)(implicit flix: Flix): Symbol.DefnSym = {
    // Generate a top-level function for the constant.
    val sym = Symbol.freshDefnSym("lit", loc)
    val lit = visitExp(exp0, m)
    val ann = Ast.Annotations.Empty
    val mod = Ast.Modifiers(List(Ast.Modifier.Synthetic))
    val varX = Symbol.freshVarSym("_unit")
    varX.setStackOffset(0)
    val fparam = FinalAst.FormalParam(varX, MonoType.Unit)
    val fs = List(fparam)
    val tpe = MonoType.Arrow(MonoType.Unit :: Nil, visitType(exp0.tpe))
    val defn = FinalAst.Def(ann, mod, sym, fs, lit, tpe, exp0.loc)
    m += (sym -> defn)
    sym
  }

}
