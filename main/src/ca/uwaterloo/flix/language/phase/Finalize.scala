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
      case (k, v) => k -> visitLatticeComponents(v, m)
    }

    val properties = root.properties.map(p => visitProperty(p, m))

    val specialOps = root.specialOps

    val reachable = root.reachable

    FinalAst.Root(defs ++ m, effs, handlers, enums, relations, lattices, latticeComponents, properties, specialOps, reachable).toSuccess
  }

  private def visitDef(def0: SimplifiedAst.Def, m: TopLevel)(implicit flix: Flix): FinalAst.Def = {
    val fs = def0.fparams.map(visitFormalParam)
    FinalAst.Def(def0.ann, def0.mod, def0.sym, fs, visitExp(def0.exp, m), def0.tpe, def0.loc)
  }

  private def visitEnum(enum0: SimplifiedAst.Enum, m: TopLevel)(implicit flix: Flix): FinalAst.Enum = enum0 match {
    case SimplifiedAst.Enum(mod, sym, cases0, tpe, loc) =>
      val cases = cases0.map {
        case (tag, SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)) => tag -> FinalAst.Case(enumSym, tagName, tagType, tagLoc)
      }
      FinalAst.Enum(mod, sym, cases, tpe, loc)
  }

  private def visitEff(eff0: SimplifiedAst.Eff): FinalAst.Eff = {
    val fs = eff0.fparams.map(visitFormalParam)
    FinalAst.Eff(eff0.ann, eff0.mod, eff0.sym, fs, eff0.tpe, eff0.loc)
  }

  private def visitHandler(handler0: SimplifiedAst.Handler, m: TopLevel)(implicit flix: Flix): FinalAst.Handler = {
    val fs = handler0.fparams.map(visitFormalParam)
    val e = visitExp(handler0.exp, m)
    FinalAst.Handler(handler0.ann, handler0.mod, handler0.sym, fs, e, handler0.tpe, handler0.loc)
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
      case SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc) => FinalAst.ConstraintParam.HeadParam(sym, tpe, loc)
      case SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc) => FinalAst.ConstraintParam.RuleParam(sym, tpe, loc)
    }
    FinalAst.Constraint(cparams, head, body)
  }

  private def visitLatticeComponents(lc: SimplifiedAst.LatticeComponents, m: TopLevel)(implicit flix: Flix): FinalAst.LatticeComponents = lc match {
    case SimplifiedAst.LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc) =>
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
        FinalAst.Expression.Var(sym, tpe, loc)

      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars.map(visitFreeVar)
        FinalAst.Expression.Closure(sym, fvs, getFunctionTypeTemporaryToBeRemoved(fvs, tpe), tpe, loc)

      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val as = args map visit
        FinalAst.Expression.ApplyClo(visit(exp), as, tpe, loc)

      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        val as = args map visit
        FinalAst.Expression.ApplyDef(name, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyEff(sym, args, tpe, loc) =>
        val as = args map visit
        FinalAst.Expression.ApplyEff(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val rs = args map visit
        FinalAst.Expression.ApplyCloTail(visit(exp), rs, tpe, loc)

      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map visit
        FinalAst.Expression.ApplyDefTail(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe, loc) =>
        val as = args map visit
        FinalAst.Expression.ApplyEffTail(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
        val fs = formals.map(visitFormalParam)
        val as = actuals.map(visit)
        FinalAst.Expression.ApplySelfTail(name, fs, as, tpe, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.Unary(sop, op, e, tpe, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        FinalAst.Expression.Binary(sop, op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val v3 = visit(exp3)
        FinalAst.Expression.IfThenElse(e1, e2, v3, tpe, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        FinalAst.Expression.Branch(e, bs, tpe, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        FinalAst.Expression.JumpTo(sym, tpe, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        FinalAst.Expression.Let(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        FinalAst.Expression.LetRec(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        val e1 = visit(exp)
        FinalAst.Expression.Is(sym, tag, e1, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.Untag(sym, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Index(base, offset, tpe, loc) =>
        val b = visit(base)
        FinalAst.Expression.Index(b, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visit
        FinalAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        FinalAst.Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, label, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.RecordSelect(e, label, tpe, loc)

      case SimplifiedAst.Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val v = visit(value)
        val r = visit(rest)
        FinalAst.Expression.RecordExtend(label, v, r, tpe, loc)

      case SimplifiedAst.Expression.RecordRestrict(label, rest, tpe, loc) =>
        val r = visit(rest)
        FinalAst.Expression.RecordRestrict(label, r, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visit
        FinalAst.Expression.ArrayLit(es, tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visit(elm)
        val l = visit(len)
        FinalAst.Expression.ArrayNew(e, l, tpe, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        FinalAst.Expression.ArrayLoad(b, i, tpe, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        FinalAst.Expression.ArrayStore(b, i, e, tpe, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base)
        FinalAst.Expression.ArrayLength(b, tpe, loc)

      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visit(base)
        val i1 = visit(startIndex)
        val i2 = visit(endIndex)
        FinalAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.Ref(e, tpe, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.Deref(e, tpe, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        FinalAst.Expression.Assign(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case SimplifiedAst.HandlerBinding(sym, body) => FinalAst.HandlerBinding(sym, visit(body))
        }
        FinalAst.Expression.HandleWith(e, bs, tpe, loc)

      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Existential(p, e, loc)

      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visit(exp)
        FinalAst.Expression.Universal(p, e, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visit(body)
            FinalAst.CatchRule(sym, clazz, b)
        }
        FinalAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args map visit
        FinalAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case SimplifiedAst.Expression.NativeField(field, tpe, loc) =>
        FinalAst.Expression.NativeField(field, tpe, loc)

      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args map visit
        FinalAst.Expression.NativeMethod(method, es, tpe, loc)

      case SimplifiedAst.Expression.NewRelation(sym, tpe, loc) =>
        FinalAst.Expression.NewRelation(sym, tpe, loc)

      case SimplifiedAst.Expression.NewLattice(sym, tpe, loc) =>
        FinalAst.Expression.NewLattice(sym, tpe, loc)

      case SimplifiedAst.Expression.Constraint(c0, tpe, loc) =>
        val c = visitConstraint(c0, m)
        FinalAst.Expression.Constraint(c, tpe, loc)

      case SimplifiedAst.Expression.ConstraintUnion(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        FinalAst.Expression.ConstraintUnion(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.FixpointSolve(exp, stf, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.FixpointSolve(e, stf, tpe, loc)

      case SimplifiedAst.Expression.FixpointCheck(exp, stf, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.FixpointCheck(e, stf, tpe, loc)

      case SimplifiedAst.Expression.FixpointDelta(exp, stf, tpe, loc) =>
        val e = visit(exp)
        FinalAst.Expression.FixpointDelta(e, stf, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe, loc) =>
        FinalAst.Expression.UserError(tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, eff, loc) =>
        FinalAst.Expression.HoleError(sym, tpe, loc)

      case SimplifiedAst.Expression.MatchError(tpe, loc) =>
        FinalAst.Expression.MatchError(tpe, loc)

      case SimplifiedAst.Expression.SwitchError(tpe, loc) =>
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
    case SimplifiedAst.Predicate.Head.RelAtom(baseOpt, sym, terms, tpe, loc) =>
      val ts = terms.map(t => visitHeadTerm(t, m))
      FinalAst.Predicate.Head.RelAtom(baseOpt, sym, ts, tpe, loc)
    case SimplifiedAst.Predicate.Head.LatAtom(baseOpt, sym, terms, tpe, loc) =>
      val ts = terms.map(t => visitHeadTerm(t, m))
      FinalAst.Predicate.Head.LatAtom(baseOpt, sym, ts, tpe, loc)
  }

  private def visitBodyPredicate(p0: SimplifiedAst.Predicate.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Predicate.Body = p0 match {
    case SimplifiedAst.Predicate.Body.RelAtom(baseOpt, sym, polarity, terms, tpe, loc) =>
      val ts = terms.map(t => visitBodyTerm(t, m))
      val index2var = getIndex2VarTemporaryToBeRemoved(ts)
      FinalAst.Predicate.Body.RelAtom(baseOpt, sym, polarity, ts, index2var, tpe, loc)

    case SimplifiedAst.Predicate.Body.LatAtom(baseOpt, sym, polarity, terms, tpe, loc) =>
      val ts = terms.map(t => visitBodyTerm(t, m))
      val index2var = getIndex2VarTemporaryToBeRemoved(ts)
      FinalAst.Predicate.Body.LatAtom(baseOpt, sym, polarity, ts, index2var, tpe, loc)

    case SimplifiedAst.Predicate.Body.Filter(sym, terms, loc) =>
      val ts = terms.map(t => visitBodyTerm(t, m))
      FinalAst.Predicate.Body.Filter(sym, ts, loc)

    case SimplifiedAst.Predicate.Body.Functional(varSym, term, loc) => term match {
      case SimplifiedAst.Term.Head.App(defSym, args, tpe, _) =>
        FinalAst.Predicate.Body.Functional(varSym, defSym, args, loc)

      case _ => throw InternalCompilerException(s"Unexpected term: $term.")
    }
  }

  private def visitHeadTerm(t0: SimplifiedAst.Term.Head, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Head = t0 match {
    case SimplifiedAst.Term.Head.QuantVar(sym, tpe, loc) => FinalAst.Term.Head.QuantVar(sym, tpe, loc)
    case SimplifiedAst.Term.Head.CapturedVar(sym, tpe, loc) => FinalAst.Term.Head.CapturedVar(sym, tpe, loc)
    case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => FinalAst.Term.Head.Lit(lit2symTemporaryToBeRemoved(lit, m), tpe, loc)
    case SimplifiedAst.Term.Head.App(sym, args, tpe, loc) => FinalAst.Term.Head.App(sym, args, tpe, loc)
  }

  private def visitBodyTerm(t0: SimplifiedAst.Term.Body, m: TopLevel)(implicit flix: Flix): FinalAst.Term.Body = t0 match {
    case SimplifiedAst.Term.Body.Wild(tpe, loc) => FinalAst.Term.Body.Wild(tpe, loc)
    case SimplifiedAst.Term.Body.QuantVar(sym, tpe, loc) => FinalAst.Term.Body.QuantVar(sym, tpe, loc)
    case SimplifiedAst.Term.Body.CapturedVar(sym, tpe, loc) => FinalAst.Term.Body.CapturedVar(sym, tpe, loc)
    case SimplifiedAst.Term.Body.Lit(lit, tpe, loc) => FinalAst.Term.Body.Lit(lit2symTemporaryToBeRemoved(lit, m), tpe, loc)
  }

  private def visitAttribute(a0: SimplifiedAst.Attribute): FinalAst.Attribute =
    FinalAst.Attribute(a0.name, a0.tpe)

  private def visitFormalParam(p0: SimplifiedAst.FormalParam): FinalAst.FormalParam =
    FinalAst.FormalParam(p0.sym, p0.tpe)

  private def visitFreeVar(v0: SimplifiedAst.FreeVar): FinalAst.FreeVar =
    FinalAst.FreeVar(v0.sym, v0.tpe)

  private def visitProperty(p0: SimplifiedAst.Property, m: TopLevel)(implicit flix: Flix): FinalAst.Property =
    FinalAst.Property(p0.law, p0.defn, visitExp(p0.exp, m))

  // TODO: Deprecated
  private def getFunctionTypeTemporaryToBeRemoved(fvs: List[FinalAst.FreeVar], tpe: Type): Type = {
    val base = tpe.typeConstructor
    val targs = tpe.typeArguments
    val freeArgs = fvs.map(_.tpe)
    Type.mkArrow(freeArgs ::: targs.init, targs.last)
  }

  // TODO: Deprecated
  private def getIndex2VarTemporaryToBeRemoved(ts: List[FinalAst.Term.Body]): List[Symbol.VarSym] = {
    val r = new Array[Symbol.VarSym](ts.length)
    var i = 0
    while (i < r.length) {
      ts(i) match {
        case FinalAst.Term.Body.QuantVar(sym, _, _) =>
          r(i) = sym
        case _ => // nop
      }
      i = i + 1
    }
    r.toList
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
    val fparam = FinalAst.FormalParam(varX, Type.Unit)
    val fs = List(fparam)
    val tpe = Type.mkArrow(Type.Unit, exp0.tpe)
    val defn = FinalAst.Def(ann, mod, sym, fs, lit, tpe, exp0.loc)
    m += (sym -> defn)
    sym
  }

}
