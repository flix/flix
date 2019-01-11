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

    FinalAst.Root(defs ++ m, effs, handlers, enums, relations, lattices, latticeComponents, properties, specialOps, reachable).toSuccess
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

      case SimplifiedAst.Expression.Var(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.Var(sym, tpe, loc)

      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe0, loc) =>
        val fvs = freeVars.map(visitFreeVar)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Closure(sym, fvs, getFunctionTypeTemporaryToBeRemoved(fvs, tpe), tpe, loc)

      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe0, loc) =>
        val as = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplyClo(visit(exp), as, tpe, loc)

      case SimplifiedAst.Expression.ApplyDef(name, args, tpe0, loc) =>
        val as = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplyDef(name, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyEff(sym, args, tpe0, loc) =>
        val as = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplyEff(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe0, loc) =>
        val e = visit(exp)
        val rs = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplyCloTail(e, rs, tpe, loc)

      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe0, loc) =>
        val as = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplyDefTail(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe0, loc) =>
        val as = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplyEffTail(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe0, loc) =>
        val fs = formals.map(visitFormalParam)
        val as = actuals.map(visit)
        val tpe = visitType(tpe0)
        FinalAst.Expression.ApplySelfTail(name, fs, as, tpe, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Unary(sop, op, e, tpe, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Binary(sop, op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val v3 = visit(exp3)
        val tpe = visitType(tpe0)
        FinalAst.Expression.IfThenElse(e1, e2, v3, tpe, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe0, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        val tpe = visitType(tpe0)
        FinalAst.Expression.Branch(e, bs, tpe, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.JumpTo(sym, tpe, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Let(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val tpe = visitType(tpe0)
        FinalAst.Expression.LetRec(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        val e1 = visit(exp)
        FinalAst.Expression.Is(sym, tag, e1, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Untag(sym, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Index(base, offset, tpe0, loc) =>
        val b = visit(base)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Index(b, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe0, loc) =>
        val es = elms map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, label, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.RecordSelect(e, label, tpe, loc)

      case SimplifiedAst.Expression.RecordExtend(label, value, rest, tpe0, loc) =>
        val v = visit(value)
        val r = visit(rest)
        val tpe = visitType(tpe0)
        FinalAst.Expression.RecordExtend(label, v, r, tpe, loc)

      case SimplifiedAst.Expression.RecordRestrict(label, rest, tpe0, loc) =>
        val r = visit(rest)
        val tpe = visitType(tpe0)
        FinalAst.Expression.RecordRestrict(label, r, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe0, loc) =>
        val es = elms map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.ArrayLit(es, tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe0, loc) =>
        val e = visit(elm)
        val l = visit(len)
        val tpe = visitType(tpe0)
        FinalAst.Expression.ArrayNew(e, l, tpe, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe0, loc) =>
        val b = visit(base)
        val i = visit(index)
        val tpe = visitType(tpe0)
        FinalAst.Expression.ArrayLoad(b, i, tpe, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe0, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        val tpe = visitType(tpe0)
        FinalAst.Expression.ArrayStore(b, i, e, tpe, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe0, loc) =>
        val b = visit(base)
        val tpe = visitType(tpe0)
        FinalAst.Expression.ArrayLength(b, tpe, loc)

      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe0, loc) =>
        val b = visit(base)
        val i1 = visit(startIndex)
        val i2 = visit(endIndex)
        val tpe = visitType(tpe0)
        FinalAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Ref(e, tpe, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Deref(e, tpe, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Assign(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe0, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case SimplifiedAst.HandlerBinding(sym, body) => FinalAst.HandlerBinding(sym, visit(body))
        }
        val tpe = visitType(tpe0)
        FinalAst.Expression.HandleWith(e, bs, tpe, loc)

      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Existential(p, e, loc)

      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Universal(p, e, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe0, loc) =>
        val e = visit(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visit(body)
            FinalAst.CatchRule(sym, clazz, b)
        }
        val tpe = visitType(tpe0)
        FinalAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe0, loc) =>
        val es = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case SimplifiedAst.Expression.NativeField(field, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.NativeField(field, tpe, loc)

      case SimplifiedAst.Expression.NativeMethod(method, args, tpe0, loc) =>
        val es = args map visit
        val tpe = visitType(tpe0)
        FinalAst.Expression.NativeMethod(method, es, tpe, loc)

      case SimplifiedAst.Expression.NewChannel(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.NewChannel(e, tpe, loc)

      case SimplifiedAst.Expression.GetChannel(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.GetChannel(e, tpe, loc)

      case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        FinalAst.Expression.PutChannel(e1, e2, visitType(tpe0), loc)

      case SimplifiedAst.Expression.SelectChannel(rules, default, tpe0, loc) =>
        val rs = rules map {
          case SimplifiedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visit(chan)
            val e = visit(exp)
            FinalAst.SelectChannelRule(sym, c, e)
        }
        val d = default.map(exp => visit(exp))
        val tpe = visitType(tpe0)
        FinalAst.Expression.SelectChannel(rs, d, tpe, loc)

      case SimplifiedAst.Expression.Spawn(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.Spawn(e, tpe, loc)

      case SimplifiedAst.Expression.Sleep(exp, tpe0, loc) =>
        val e = visit(exp)
        val t = visitType(tpe0)
        FinalAst.Expression.Sleep(e, t, loc)

      case SimplifiedAst.Expression.FixpointConstraint(c0, tpe0, loc) =>
        val c = visitConstraint(c0, m)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointConstraint(c, tpe, loc)

      case SimplifiedAst.Expression.FixpointCompose(exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointCompose(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.FixpointSolve(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointSolve(Ast.freshUId(), e, Ast.Stratification.Empty, tpe, loc)

      case SimplifiedAst.Expression.FixpointCheck(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointCheck(Ast.freshUId(), e, Ast.Stratification.Empty, tpe, loc)

      case SimplifiedAst.Expression.FixpointDelta(exp, tpe0, loc) =>
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointDelta(Ast.freshUId(), e, Ast.Stratification.Empty, tpe, loc)

      case SimplifiedAst.Expression.FixpointProject(pred, exp, tpe0, loc) =>
        val p = visitPredicateWithParam(pred, m)
        val e = visit(exp)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointProject(p, e, tpe, loc)

      case SimplifiedAst.Expression.FixpointEntails(exp1, exp2, tpe0, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val tpe = visitType(tpe0)
        FinalAst.Expression.FixpointEntails(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.UserError(tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.HoleError(sym, tpe, loc)

      case SimplifiedAst.Expression.MatchError(tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.MatchError(tpe, loc)

      case SimplifiedAst.Expression.SwitchError(tpe0, loc) =>
        val tpe = visitType(tpe0)
        FinalAst.Expression.SwitchError(tpe, loc)

      case SimplifiedAst.Expression.Def(sym, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Eff(sym, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Lambda(fparams, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
    }

    visit(exp0)
  }

  private def visitHeadPredicate(p0: SimplifiedAst.Predicate.Head, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Head = p0 match {
    case SimplifiedAst.Predicate.Head.True(loc) => FinalAst.Predicate.Head.True(loc)
    case SimplifiedAst.Predicate.Head.False(loc) => FinalAst.Predicate.Head.False(loc)

    case SimplifiedAst.Predicate.Head.Atom(pred, terms, tpe0, loc) =>
      val p = visitPredicateWithParam(pred, m)
      val ts = terms.map(t => visitHeadTerm(t, m))
      val tpe = visitType(tpe0)
      FinalAst.Predicate.Head.Atom(p, ts, tpe, loc)
  }

  private def visitBodyPredicate(p0: SimplifiedAst.Predicate.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Body = p0 match {
    case SimplifiedAst.Predicate.Body.Atom(pred, polarity, terms, tpe0, loc) =>
      val p = visitPredicateWithParam(pred, m)
      val ts = terms.map(t => visitBodyTerm(t, m))
      val tpe = visitType(tpe0)
      FinalAst.Predicate.Body.Atom(p, polarity, ts, tpe, loc)

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
    case SimplifiedAst.Term.Head.QuantVar(sym, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Head.QuantVar(sym, tpe, loc)

    case SimplifiedAst.Term.Head.CapturedVar(sym, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Head.CapturedVar(sym, tpe, loc)

    case SimplifiedAst.Term.Head.Lit(lit, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Head.Lit(lit2symTemporaryToBeRemoved(lit, m), tpe, loc)

    case SimplifiedAst.Term.Head.App(sym, args, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Head.App(sym, args, tpe, loc)
  }

  private def visitBodyTerm(t0: SimplifiedAst.Term.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Body = t0 match {
    case SimplifiedAst.Term.Body.Wild(tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Body.Wild(tpe, loc)

    case SimplifiedAst.Term.Body.QuantVar(sym, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Body.QuantVar(sym, tpe, loc)

    case SimplifiedAst.Term.Body.CapturedVar(sym, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Body.CapturedVar(sym, tpe, loc)

    case SimplifiedAst.Term.Body.Lit(lit, tpe0, loc) =>
      val tpe = visitType(tpe0)
      FinalAst.Term.Body.Lit(lit2symTemporaryToBeRemoved(lit, m), tpe, loc)
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
  def visitType(t0: Type): MonoType = t0 match {

    case Type.Unit => MonoType.Unit
    case Type.Bool => MonoType.Bool
    case Type.Char => MonoType.Char
    case Type.Float32 => MonoType.Float32
    case Type.Float64 => MonoType.Float64
    case Type.Int8 => MonoType.Int8
    case Type.Int16 => MonoType.Int16
    case Type.Int32 => MonoType.Int32
    case Type.Int64 => MonoType.Int64
    case Type.BigInt => MonoType.BigInt
    case Type.Str => MonoType.Str

    case Type.Channel => ??? // cannot happen...
    case Type.Array => ??? // cannot happen...
    case Type.Native(clazz) => MonoType.Native(clazz)
    case Type.Ref => ??? // cannot happen...
    case Type.Arrow(length) => ??? // cannot happen...
    case Type.Relation(sym, attr, kind) => MonoType.Relation(sym, attr map visitType)
    case Type.Lattice(sym, attr, kind) => MonoType.Lattice(sym, attr map visitType)
    case Type.Schema(m0) =>
      val m = m0.foldLeft(Map.empty[Symbol.PredSym, MonoType]) {
        case (macc, (sym, t)) => macc + (sym -> visitType(t))
      }
      MonoType.Schema(m)
    case Type.Tuple(length) => MonoType.Tuple(Nil) // TODO: Seems very suspicious
    case Type.RecordEmpty => MonoType.RecordEmpty()
    case Type.RecordExtend(label, value, rest) => MonoType.RecordExtend(label, visitType(value), visitType(rest))
    case Type.Apply(Type.Channel, tpe2) => MonoType.Channel(visitType(tpe2))
    case Type.Apply(Type.Ref, tpe2) => MonoType.Ref(visitType(tpe2))

    case Type.Var(id, kind) => MonoType.Var(id)

    case _ =>

      t0.typeConstructor match {
        case Type.Enum(sym, _) =>
          MonoType.Enum(sym, t0.typeArguments.map(visitType))
        case Type.Tuple(_) => MonoType.Tuple(t0.typeArguments.map(visitType))
        case Type.Array => MonoType.Array(visitType(t0.typeArguments.head))
        case Type.Vector => MonoType.Array(visitType(t0.typeArguments.head))
        case Type.Arrow(l) =>
          val targs = t0.typeArguments
          MonoType.Arrow(targs.init.map(visitType), visitType(targs.last))
        case _ => t0 match {
          case Type.Apply(tpe1, tpe2) => MonoType.Apply(visitType(tpe1), visitType(tpe2))
          case _ => ??? // TODO
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
