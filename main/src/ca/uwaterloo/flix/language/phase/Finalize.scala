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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

object Finalize extends Phase[SimplifiedAst.Root, FinalAst.Root] {

  private type TopLevel = mutable.Map[Symbol.DefnSym, FinalAst.Def]

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[FinalAst.Root, CompilationError] = flix.phase("Finalize") {

    val m: TopLevel = mutable.Map.empty

    val defs = root.defs.map {
      case (k, v) => k -> visitDef(v, m)
    }

    val effs = root.effs.map {
      case (k, v) => k -> visitEff(v)
    }

    val handlers = root.handlers.map {
      case (k, v) => k -> visitHandler(v, m)
    }

    val enums = root.enums.map {
      case (sym, enum) => sym -> visitEnum(enum, m)
    }

    val relations = root.relations.map {
      case (k, v) => k -> visitRelation(v)
    }

    val lattices = root.lattices.map {
      case (k, v) => k -> visitLattice(v)
    }

    val latticeComponents = root.latticeComponents.map {
      case (k, v) => visitType(k) -> visitLatticeComponents(v, m)
    }

    val properties = root.properties.map(p => visitProperty(p, m))

    val specialOps = root.specialOps.map {
      case (op, ops) => op -> ops.map {
        case (tpe, sym) => visitType(tpe) -> sym
      }
    }

    val reachable = root.reachable

    FinalAst.Root(defs ++ m, effs, handlers, enums, relations, lattices, latticeComponents, properties, specialOps, reachable, root.sources).toSuccess
  }

  private def visitDef(def0: SimplifiedAst.Def, m: TopLevel)(implicit flix: Flix): FinalAst.Def = {
    val fs = def0.fparams.map(visitFormalParam)
    val e = visitExp(def0.exp, m)
    val tpe = visitType(def0.tpe)
    FinalAst.Def(def0.ann, def0.mod, def0.sym, fs, e, tpe, def0.loc)
  }

  private def visitEnum(enum0: SimplifiedAst.Enum, m: TopLevel)(implicit flix: Flix): FinalAst.Enum = enum0 match {
    case SimplifiedAst.Enum(mod, sym, cases0, tpe0, loc) =>
      val cases = cases0.map {
        case (tag, SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)) =>
          val tpe = visitType(tagType)
          tag -> FinalAst.Case(enumSym, tagName, tpe, tagLoc)
      }
      val tpe = visitType(tpe0)
      FinalAst.Enum(mod, sym, cases, tpe, loc)
  }

  private def visitEff(eff0: SimplifiedAst.Eff): FinalAst.Eff = {
    val fs = eff0.fparams.map(visitFormalParam)
    val tpe = visitType(eff0.tpe)
    FinalAst.Eff(eff0.ann, eff0.mod, eff0.sym, fs, tpe, eff0.loc)
  }

  private def visitHandler(handler0: SimplifiedAst.Handler, m: TopLevel)(implicit flix: Flix): FinalAst.Handler = {
    val fs = handler0.fparams.map(visitFormalParam)
    val e = visitExp(handler0.exp, m)
    val tpe = visitType(handler0.tpe)
    FinalAst.Handler(handler0.ann, handler0.mod, handler0.sym, fs, e, tpe, handler0.loc)
  }

  private def visitRelation(relation0: SimplifiedAst.Relation): FinalAst.Relation = relation0 match {
    case SimplifiedAst.Relation(mod, sym, attr, loc) =>
      FinalAst.Relation(mod, sym, attr.map(visitAttribute), loc)
  }

  private def visitLattice(lattice0: SimplifiedAst.Lattice): FinalAst.Lattice = lattice0 match {
    case SimplifiedAst.Lattice(mod, sym, attr, loc) =>
      FinalAst.Lattice(mod, sym, attr.map(visitAttribute), loc)
  }

  private def visitConstraint(constraint0: SimplifiedAst.Constraint, m: TopLevel)(implicit flix: Flix): FinalAst.Constraint = {
    val head = visitHeadPredicate(constraint0.head, m)
    val body = constraint0.body.map(b => visitBodyPredicate(b, m))
    val cparams = constraint0.cparams.map {
      case SimplifiedAst.ConstraintParam.HeadParam(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.ConstraintParam.HeadParam(sym, tpe, loc)

      case SimplifiedAst.ConstraintParam.RuleParam(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.ConstraintParam.RuleParam(sym, tpe, loc)
    }
    FinalAst.Constraint(cparams, head, body)
  }

  private def visitLatticeComponents(lc: SimplifiedAst.LatticeComponents, m: TopLevel)(implicit flix: Flix): FinalAst.LatticeComponents = lc match {
    case SimplifiedAst.LatticeComponents(tpe0, bot, top, equ, leq, lub, glb, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc)
  }

  private def visitExp(exp0: SimplifiedAst.Expression, m: TopLevel)(implicit flix: Flix): FinalAst.Expression = {

    def visit(e0: SimplifiedAst.Expression): FinalAst.Expression = e0 match {
      case SimplifiedAst.Expression.Unit =>
        FinalAst.Expression.Unit

      case SimplifiedAst.Expression.True =>
        FinalAst.Expression.True

      case SimplifiedAst.Expression.False =>
        FinalAst.Expression.False

      case SimplifiedAst.Expression.Char(lit) =>
        FinalAst.Expression.Char(lit)

      case SimplifiedAst.Expression.Float32(lit) =>
        FinalAst.Expression.Float32(lit)

      case SimplifiedAst.Expression.Float64(lit) =>
        FinalAst.Expression.Float64(lit)

      case SimplifiedAst.Expression.Int8(lit) =>
        FinalAst.Expression.Int8(lit)

      case SimplifiedAst.Expression.Int16(lit) =>
        FinalAst.Expression.Int16(lit)

      case SimplifiedAst.Expression.Int32(lit) =>
        FinalAst.Expression.Int32(lit)

      case SimplifiedAst.Expression.Int64(lit) =>
        FinalAst.Expression.Int64(lit)

      case SimplifiedAst.Expression.BigInt(lit) =>
        FinalAst.Expression.BigInt(lit)

      case SimplifiedAst.Expression.Str(lit) =>
        FinalAst.Expression.Str(lit)

      case SimplifiedAst.Expression.Var(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.Var(sym, t, loc)

      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars.map(visitFreeVar)
        val t = visitType(tpe)
        FinalAst.Expression.Closure(sym, fvs, getFunctionTypeTemporaryToBeRemoved(fvs, t), t, loc)

      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyClo(visit(exp), as, t, loc)

      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyDef(name, as, t, loc)

      case SimplifiedAst.Expression.ApplyEff(sym, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyEff(sym, as, t, loc)

      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val e = visit(exp)
        val rs = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyCloTail(e, rs, t, loc)

      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyDefTail(sym, as, t, loc)

      case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyEffTail(sym, as, t, loc)

      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
        val fs = formals.map(visitFormalParam)
        val as = actuals.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.ApplySelfTail(name, fs, as, t, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Unary(sop, op, e, t, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Binary(sop, op, e1, e2, t, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val v3 = visit(exp3)
        val t = visitType(tpe)
        FinalAst.Expression.IfThenElse(e1, e2, v3, t, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        val t = visitType(tpe)
        FinalAst.Expression.Branch(e, bs, t, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.JumpTo(sym, t, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Let(sym, e1, e2, t, loc)

      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.LetRec(sym, e1, e2, t, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        val e1 = visit(exp)
        FinalAst.Expression.Is(sym, tag, e1, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Tag(enum, tag, e, t, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Untag(sym, tag, e, t, loc)

      case SimplifiedAst.Expression.Index(base, offset, tpe, loc) =>
        val b = visit(base)
        val t = visitType(tpe)
        FinalAst.Expression.Index(b, offset, t, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visit
        val t = visitType(tpe)
        FinalAst.Expression.Tuple(es, t, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.RecordEmpty(t, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, label, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.RecordSelect(e, label, t, loc)

      case SimplifiedAst.Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val v = visit(value)
        val r = visit(rest)
        val t = visitType(tpe)
        FinalAst.Expression.RecordExtend(label, v, r, t, loc)

      case SimplifiedAst.Expression.RecordRestrict(label, rest, tpe, loc) =>
        val r = visit(rest)
        val t = visitType(tpe)
        FinalAst.Expression.RecordRestrict(label, r, t, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visit
        val t = visitType(tpe)
        FinalAst.Expression.ArrayLit(es, t, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visit(elm)
        val l = visit(len)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayNew(e, l, t, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayLoad(b, i, t, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayStore(b, i, e, t, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base)
        val t = visitType(tpe)
        FinalAst.Expression.ArrayLength(b, t, loc)

      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visit(base)
        val i1 = visit(startIndex)
        val i2 = visit(endIndex)
        val t = visitType(tpe)
        FinalAst.Expression.ArraySlice(b, i1, i2, t, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Ref(e, t, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Deref(e, t, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Assign(e1, e2, t, loc)

      case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case SimplifiedAst.HandlerBinding(sym, body) => FinalAst.HandlerBinding(sym, visit(body))
        }
        val t = visitType(tpe)
        FinalAst.Expression.HandleWith(e, bs, t, loc)

      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Existential(p, e, loc)

      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Universal(p, e, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visit(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visit(body)
            FinalAst.CatchRule(sym, clazz, b)
        }
        val t = visitType(tpe)
        FinalAst.Expression.TryCatch(e, rs, t, loc)

      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.NativeConstructor(constructor, es, t, loc)

      case SimplifiedAst.Expression.NativeField(field, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.NativeField(field, t, loc)

      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.NativeMethod(method, es, t, loc)

      case SimplifiedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.NewChannel(e, t, loc)

      case SimplifiedAst.Expression.GetChannel(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.GetChannel(e, t, loc)

      case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.PutChannel(e1, e2, t, loc)

      case SimplifiedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SimplifiedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visit(chan)
            val e = visit(exp)
            FinalAst.SelectChannelRule(sym, c, e)
        }
        val d = default.map(exp => visit(exp))
        val t = visitType(tpe)
        FinalAst.Expression.SelectChannel(rs, d, t, loc)

      case SimplifiedAst.Expression.ProcessSpawn(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.ProcessSpawn(e, t, loc)

      case SimplifiedAst.Expression.ProcessSleep(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.ProcessSleep(e, t, loc)

      case SimplifiedAst.Expression.ProcessPanic(msg, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.ProcessPanic(msg, t, loc)

      case SimplifiedAst.Expression.FixpointConstraint(c0, tpe, loc) =>
        val c = visitConstraint(c0, m)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointConstraint(c, t, loc)

      case SimplifiedAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointCompose(e1, e2, t, loc)

      case SimplifiedAst.Expression.FixpointSolve(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointSolve(Ast.freshUId(), e, Ast.Stratification.Empty, t, loc)

      case SimplifiedAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
        val p = visitPredicateWithParam(pred, m)
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointProject(p, e, t, loc)

      case SimplifiedAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.FixpointEntails(e1, e2, t, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.HoleError(sym, t, loc)

      case SimplifiedAst.Expression.MatchError(tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.MatchError(t, loc)

      case SimplifiedAst.Expression.SwitchError(tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.SwitchError(t, loc)

      case SimplifiedAst.Expression.Def(sym, t, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Eff(sym, t, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Lambda(fparams, exp, t, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.LambdaClosure(fparams, freeVars, exp, t, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Apply(exp, args, t, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
    }

    visit(exp0)
  }

  private def visitHeadPredicate(p0: SimplifiedAst.Predicate.Head, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Head = p0 match {
    case SimplifiedAst.Predicate.Head.Atom(pred, terms, tpe, loc) =>
      val p = visitPredicateWithParam(pred, m)
      val ts = terms.map(t => visitHeadTerm(t, m))
      val t = visitType(tpe)
      FinalAst.Predicate.Head.Atom(p, ts, t, loc)
  }

  private def visitBodyPredicate(p0: SimplifiedAst.Predicate.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Body = p0 match {
    case SimplifiedAst.Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
      val p = visitPredicateWithParam(pred, m)
      val ts = terms.map(t => visitBodyTerm(t, m))
      val t = visitType(tpe)
      FinalAst.Predicate.Body.Atom(p, polarity, ts, t, loc)

    case SimplifiedAst.Predicate.Body.Filter(sym, terms, loc) =>
      val ts = terms.map(t => visitBodyTerm(t, m))
      FinalAst.Predicate.Body.Filter(sym, ts, loc)

    case SimplifiedAst.Predicate.Body.Functional(varSym, term, loc) => term match {
      case SimplifiedAst.Term.Head.App(defSym, args, tpe, _) =>
        FinalAst.Predicate.Body.Functional(varSym, defSym, args, loc)

      case _ => throw InternalCompilerException(s"Unexpected term: $term.")
    }
  }

  private def visitPredicateWithParam(p0: SimplifiedAst.PredicateWithParam, m: TopLevel)(implicit flix: Flix): FinalAst.PredicateWithParam = p0 match {
    case SimplifiedAst.PredicateWithParam(sym, exp) =>
      val e = visitExp(exp, m)
      FinalAst.PredicateWithParam(sym, e)
  }

  private def visitHeadTerm(t0: SimplifiedAst.Term.Head, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Head = t0 match {
    case SimplifiedAst.Term.Head.QuantVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.QuantVar(sym, t, loc)

    case SimplifiedAst.Term.Head.CapturedVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.CapturedVar(sym, t, loc)

    case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.Lit(lit2symTemporaryToBeRemoved(lit, m), t, loc)

    case SimplifiedAst.Term.Head.App(sym, args, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Head.App(sym, args, t, loc)
  }

  private def visitBodyTerm(t0: SimplifiedAst.Term.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Body = t0 match {
    case SimplifiedAst.Term.Body.Wild(tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.Wild(t, loc)

    case SimplifiedAst.Term.Body.QuantVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.QuantVar(sym, t, loc)

    case SimplifiedAst.Term.Body.CapturedVar(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.CapturedVar(sym, t, loc)

    case SimplifiedAst.Term.Body.Lit(lit, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Term.Body.Lit(lit2symTemporaryToBeRemoved(lit, m), t, loc)
  }

  private def visitAttribute(a0: SimplifiedAst.Attribute): FinalAst.Attribute = {
    val tpe = visitType(a0.tpe)
    FinalAst.Attribute(a0.name, tpe)
  }

  private def visitFormalParam(p0: SimplifiedAst.FormalParam): FinalAst.FormalParam = {
    val tpe = visitType(p0.tpe)
    FinalAst.FormalParam(p0.sym, tpe)
  }

  private def visitFreeVar(v0: SimplifiedAst.FreeVar): FinalAst.FreeVar = {
    val tpe = visitType(v0.tpe)
    FinalAst.FreeVar(v0.sym, tpe)
  }

  private def visitProperty(p0: SimplifiedAst.Property, m: TopLevel)(implicit flix: Flix): FinalAst.Property =
    FinalAst.Property(p0.law, p0.defn, visitExp(p0.exp, m))

  // TODO: Should be private
  def visitType(t0: Type): MonoType = {
    val base = t0.typeConstructor
    val args = t0.typeArguments.map(visitType)

    base match {
      // Primitive Types.
      case Type.Cst(TypeConstructor.Unit) => MonoType.Unit
      case Type.Cst(TypeConstructor.Bool) => MonoType.Bool
      case Type.Cst(TypeConstructor.Char) => MonoType.Char
      case Type.Cst(TypeConstructor.Float32) => MonoType.Float32
      case Type.Cst(TypeConstructor.Float64) => MonoType.Float64
      case Type.Cst(TypeConstructor.Int8) => MonoType.Int8
      case Type.Cst(TypeConstructor.Int16) => MonoType.Int16
      case Type.Cst(TypeConstructor.Int32) => MonoType.Int32
      case Type.Cst(TypeConstructor.Int64) => MonoType.Int64
      case Type.Cst(TypeConstructor.BigInt) => MonoType.BigInt
      case Type.Cst(TypeConstructor.Str) => MonoType.Str

      // Compound Types.
      case Type.Cst(TypeConstructor.Array) => MonoType.Array(args.head)

      case Type.Cst(TypeConstructor.Channel) => MonoType.Channel(args.head)

      case Type.Cst(TypeConstructor.Enum(sym, _)) => MonoType.Enum(sym, args)

      case Type.Cst(TypeConstructor.Native(clazz)) => MonoType.Native(clazz)

      case Type.Cst(TypeConstructor.Ref) => MonoType.Ref(args.head)

      case Type.Cst(TypeConstructor.Vector) => MonoType.Array(args.head)

      case Type.Cst(TypeConstructor.Tuple(l)) => MonoType.Tuple(args)

      case Type.Arrow(l) => MonoType.Arrow(args.init, args.last)

      case Type.RecordEmpty => MonoType.RecordEmpty()

      case Type.RecordExtend(label, value, rest) => MonoType.RecordExtend(label, visitType(value), visitType(rest))

      case Type.SchemaEmpty => MonoType.SchemaEmpty()

      case Type.SchemaExtend(sym, tpe, rest) => MonoType.SchemaExtend(sym, visitType(tpe), visitType(rest))

      case Type.Relation(sym, attr, _) => MonoType.Relation(sym, attr map visitType)

      case Type.Lattice(sym, attr, _) => MonoType.Lattice(sym, attr map visitType)

      case Type.Zero => MonoType.Unit

      case Type.Succ(l, t) => MonoType.Unit

      case Type.Var(id, kind) => MonoType.Var(id) // TODO: Should never happen.

      case Type.Apply(_, _) => throw InternalCompilerException(s"Unexpected type: '$t0'.")
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
  private def lit2symTemporaryToBeRemoved(exp0: SimplifiedAst.Expression, m: TopLevel)(implicit flix: Flix): Symbol.DefnSym = {
    implicit val _ = flix.genSym
    // Generate a top-level function for the constant.
    val sym = Symbol.freshDefnSym("lit")
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
