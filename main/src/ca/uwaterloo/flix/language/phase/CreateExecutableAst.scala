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
import ca.uwaterloo.flix.runtime.solver.api.ProxyObject
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

// TODO: This class is pretty ugly and could use a rewrite.
// TODO: Rename to Finalize.

object CreateExecutableAst extends Phase[SimplifiedAst.Root, ExecutableAst.Root] {

  private type TopLevel = mutable.Map[Symbol.DefnSym, ExecutableAst.Def]

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = flix.phase("CreateExecutableAst") {

    // A mutable map to hold top-level definitions created by lifting lattice expressions.
    val m: TopLevel = mutable.Map.empty

    val constants = root.defs.map { case (k, v) => k -> visitDef(v, m) }
    val effs = root.effs.map { case (k, v) => k -> visitEff(v) }
    val handlers = root.handlers.map { case (k, v) => k -> visitHandler(v, m) }

    val enums = root.enums.map {
      case (sym, SimplifiedAst.Enum(mod, _, cases0, tpe, loc)) =>
        val cases = cases0.map {
          case (tag, SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)) => tag -> ExecutableAst.Case(enumSym, tagName, tagType, tagLoc)
        }
        sym -> ExecutableAst.Enum(mod, sym, cases, tpe, loc)
    }

    val latticeComponents = root.latticeComponents.map { case (k, v) => k -> visitLatticeComponents(v, m) }
    val relations = root.relations.map { case (k, v) => k -> visitRelation(v) }
    val lattices = root.lattices.map { case (k, v) => k -> visitLattice(v) }
    val strata = root.strata.map(s => ExecutableAst.Stratum(s.constraints.map(c => visitConstraint(c, m))))
    val properties = root.properties.map(p => visitProperty(p, m))
    val specialOps = root.specialOps
    val reachable = root.reachable

    ExecutableAst.Root(constants ++ m, effs, handlers, enums, relations, lattices, latticeComponents, strata, properties, specialOps, reachable).toSuccess
  }

  private def visitDef(def0: SimplifiedAst.Def, m: TopLevel)(implicit flix: Flix): ExecutableAst.Def = {
    val fs = def0.fparams.map(visitFormalParam)
    ExecutableAst.Def(def0.ann, def0.mod, def0.sym, fs, visitExp(def0.exp, m), def0.tpe, def0.loc)
  }

  private def visitEff(eff0: SimplifiedAst.Eff): ExecutableAst.Eff = {
    val fs = eff0.fparams.map(visitFormalParam)
    ExecutableAst.Eff(eff0.ann, eff0.mod, eff0.sym, fs, eff0.tpe, eff0.loc)
  }

  private def visitHandler(handler0: SimplifiedAst.Handler, m: TopLevel)(implicit flix: Flix): ExecutableAst.Handler = {
    val fs = handler0.fparams.map(visitFormalParam)
    val e = visitExp(handler0.exp, m)
    ExecutableAst.Handler(handler0.ann, handler0.mod, handler0.sym, fs, e, handler0.tpe, handler0.loc)
  }

  private def visitLatticeComponents(sast: SimplifiedAst.LatticeComponents, m: TopLevel)(implicit flix: Flix): ExecutableAst.LatticeComponents = sast match {
    case SimplifiedAst.LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc) =>
      ExecutableAst.LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc)
  }

  private def visitRelation(r: SimplifiedAst.Relation): ExecutableAst.Relation = r match {
    case SimplifiedAst.Relation(mod, sym, attr, loc) =>
      ExecutableAst.Relation(mod, sym, attr.map(visitAttribute), loc)
  }

  private def visitLattice(l: SimplifiedAst.Lattice): ExecutableAst.Lattice = l match {
    case SimplifiedAst.Lattice(mod, sym, attr, loc) =>
      ExecutableAst.Lattice(mod, sym, attr.map(visitAttribute), loc)
  }

  private def visitConstraint(c0: SimplifiedAst.Constraint, m: TopLevel)(implicit flix: Flix): ExecutableAst.Constraint = {
    val head = visitHeadPredicate(c0.head, m)
    val body = c0.body.map(b => visitBodyPredicate(b, m))
    val cparams = c0.cparams.map {
      case SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc) => ExecutableAst.ConstraintParam.HeadParam(sym, tpe, loc)
      case SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc) => ExecutableAst.ConstraintParam.RuleParam(sym, tpe, loc)
    }
    ExecutableAst.Constraint(cparams, head, body)
  }

  private def visitExp(exp0: SimplifiedAst.Expression, m: TopLevel)(implicit flix: Flix): ExecutableAst.Expression = {

    def visit(e0: SimplifiedAst.Expression): ExecutableAst.Expression = e0 match {
      case SimplifiedAst.Expression.Unit =>
        ExecutableAst.Expression.Unit

      case SimplifiedAst.Expression.True =>
        ExecutableAst.Expression.True

      case SimplifiedAst.Expression.False =>
        ExecutableAst.Expression.False

      case SimplifiedAst.Expression.Char(lit) =>
        ExecutableAst.Expression.Char(lit)

      case SimplifiedAst.Expression.Float32(lit) =>
        ExecutableAst.Expression.Float32(lit)

      case SimplifiedAst.Expression.Float64(lit) =>
        ExecutableAst.Expression.Float64(lit)

      case SimplifiedAst.Expression.Int8(lit) =>
        ExecutableAst.Expression.Int8(lit)

      case SimplifiedAst.Expression.Int16(lit) =>
        ExecutableAst.Expression.Int16(lit)

      case SimplifiedAst.Expression.Int32(lit) =>
        ExecutableAst.Expression.Int32(lit)

      case SimplifiedAst.Expression.Int64(lit) =>
        ExecutableAst.Expression.Int64(lit)

      case SimplifiedAst.Expression.BigInt(lit) =>
        ExecutableAst.Expression.BigInt(lit)

      case SimplifiedAst.Expression.Str(lit) =>
        ExecutableAst.Expression.Str(lit)

      case SimplifiedAst.Expression.Var(sym, tpe, loc) =>
        ExecutableAst.Expression.Var(sym, tpe, loc)

      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars.map(visitFreeVar)
        // TODO: Temporary fix to compute the function interface type (as opposed to the closure interface type).
        // In the future this "computation" should not be performed here.
        val base = tpe.typeConstructor
        val targs = tpe.typeArguments
        val freeArgs = fvs.map(_.tpe)
        val fnType = Type.mkArrow(freeArgs ::: targs.init, targs.last)
        ExecutableAst.Expression.Closure(sym, fvs, fnType, tpe, loc)

      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val as = args map visit
        ExecutableAst.Expression.ApplyClo(visit(exp), as, tpe, loc)

      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        val as = args map visit
        ExecutableAst.Expression.ApplyDef(name, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyEff(sym, args, tpe, loc) =>
        val as = args map visit
        ExecutableAst.Expression.ApplyEff(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val rs = args map visit
        ExecutableAst.Expression.ApplyCloTail(visit(exp), rs, tpe, loc)

      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map visit
        ExecutableAst.Expression.ApplyDefTail(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe, loc) =>
        val as = args map visit
        ExecutableAst.Expression.ApplyEffTail(sym, as, tpe, loc)

      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
        val fs = formals.map(visitFormalParam)
        val as = actuals.map(visit)
        ExecutableAst.Expression.ApplySelfTail(name, fs, as, tpe, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.Unary(sop, op, e, tpe, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        ExecutableAst.Expression.Binary(sop, op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val v3 = visit(exp3)
        ExecutableAst.Expression.IfThenElse(e1, e2, v3, tpe, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        ExecutableAst.Expression.Branch(e, bs, tpe, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        ExecutableAst.Expression.JumpTo(sym, tpe, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        ExecutableAst.Expression.Let(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        ExecutableAst.Expression.LetRec(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        val e1 = visit(exp)
        ExecutableAst.Expression.Is(sym, tag, e1, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.Untag(sym, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Index(base, offset, tpe, loc) =>
        val b = visit(base)
        ExecutableAst.Expression.Index(b, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visit
        ExecutableAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visit
        ExecutableAst.Expression.ArrayLit(es, tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visit(elm)
        val ln = visit(len)
        ExecutableAst.Expression.ArrayNew(e, ln, tpe, loc)
      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        ExecutableAst.Expression.ArrayLoad(b, i, tpe, loc)
      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        ExecutableAst.Expression.ArrayStore(b, i, e, tpe, loc)
      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base)
        ExecutableAst.Expression.ArrayLength(b, tpe, loc)
      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visit(base)
        val i1 = visit(startIndex)
        val i2 = visit(endIndex)
        ExecutableAst.Expression.ArraySlice(b, i1, i2, tpe, loc)
      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.Ref(e, tpe, loc)
      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.Deref(e, tpe, loc)
      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        ExecutableAst.Expression.Assign(e1, e2, tpe, loc)
      case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case SimplifiedAst.HandlerBinding(sym, body) => ExecutableAst.HandlerBinding(sym, visit(body))
        }
        ExecutableAst.Expression.HandleWith(e, bs, tpe, loc)
      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Existential(p, visit(exp), loc)
      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Universal(p, visit(exp), loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visit(body)
            ExecutableAst.CatchRule(sym, clazz, b)
        }
        ExecutableAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args.map(e => visit(e))
        ExecutableAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case SimplifiedAst.Expression.NativeField(field, tpe, loc) => ExecutableAst.Expression.NativeField(field, tpe, loc)

      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args.map(e => visit(e))
        ExecutableAst.Expression.NativeMethod(method, es, tpe, loc)

      case SimplifiedAst.Expression.NewRelation(sym, tpe, loc) =>
        ExecutableAst.Expression.NewRelation(sym, tpe, loc)

      case SimplifiedAst.Expression.NewLattice(sym, tpe, loc) =>
        ExecutableAst.Expression.NewLattice(sym, tpe, loc)

      case SimplifiedAst.Expression.Constraint(c0, tpe, loc) =>
        val c = visitConstraint(c0, m)
        ExecutableAst.Expression.Constraint(c, tpe, loc)

      case SimplifiedAst.Expression.ConstraintUnion(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        ExecutableAst.Expression.ConstraintUnion(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.FixpointSolve(exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.FixpointSolve(e, tpe, loc)

      case SimplifiedAst.Expression.FixpointCheck(exp, tpe, loc) =>
        val e = visit(exp)
        ExecutableAst.Expression.FixpointCheck(e, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe, loc) => ExecutableAst.Expression.UserError(tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, eff, loc) => ExecutableAst.Expression.HoleError(sym, tpe, loc)

      case SimplifiedAst.Expression.MatchError(tpe, loc) => ExecutableAst.Expression.MatchError(tpe, loc)

      case SimplifiedAst.Expression.SwitchError(tpe, loc) => ExecutableAst.Expression.SwitchError(tpe, loc)

      case SimplifiedAst.Expression.Def(sym, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Eff(sym, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Lambda(fparams, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$e0'.")
    }

    visit(exp0)
  }

  private def visitPattern(pat0: SimplifiedAst.Pattern): ExecutableAst.Pattern = pat0 match {
    case SimplifiedAst.Pattern.Wild(tpe, loc) => ExecutableAst.Pattern.Wild(tpe, loc)
    case SimplifiedAst.Pattern.Var(sym, tpe, loc) => ExecutableAst.Pattern.Var(sym, tpe, loc)
    case SimplifiedAst.Pattern.Unit(loc) => ExecutableAst.Pattern.Unit(loc)
    case SimplifiedAst.Pattern.True(loc) => ExecutableAst.Pattern.True(loc)
    case SimplifiedAst.Pattern.False(loc) => ExecutableAst.Pattern.False(loc)
    case SimplifiedAst.Pattern.Char(lit, loc) => ExecutableAst.Pattern.Char(lit, loc)
    case SimplifiedAst.Pattern.Float32(lit, loc) => ExecutableAst.Pattern.Float32(lit, loc)
    case SimplifiedAst.Pattern.Float64(lit, loc) => ExecutableAst.Pattern.Float64(lit, loc)
    case SimplifiedAst.Pattern.Int8(lit, loc) => ExecutableAst.Pattern.Int8(lit, loc)
    case SimplifiedAst.Pattern.Int16(lit, loc) => ExecutableAst.Pattern.Int16(lit, loc)
    case SimplifiedAst.Pattern.Int32(lit, loc) => ExecutableAst.Pattern.Int32(lit, loc)
    case SimplifiedAst.Pattern.Int64(lit, loc) => ExecutableAst.Pattern.Int64(lit, loc)
    case SimplifiedAst.Pattern.BigInt(lit, loc) => ExecutableAst.Pattern.BigInt(lit, loc)
    case SimplifiedAst.Pattern.Str(lit, loc) => ExecutableAst.Pattern.Str(lit, loc)
    case SimplifiedAst.Pattern.Tag(sym, tag, pat, tpe, loc) => ExecutableAst.Pattern.Tag(sym, tag, visitPattern(pat), tpe, loc)
    case SimplifiedAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms map visitPattern
      ExecutableAst.Pattern.Tuple(es, tpe, loc)
  }

  private def visitHeadPredicate(p0: SimplifiedAst.Predicate.Head, m: TopLevel)(implicit flix: Flix): ExecutableAst.Predicate.Head = p0 match {
    case SimplifiedAst.Predicate.Head.True(loc) => ExecutableAst.Predicate.Head.True(loc)
    case SimplifiedAst.Predicate.Head.False(loc) => ExecutableAst.Predicate.Head.False(loc)
    case SimplifiedAst.Predicate.Head.RelAtom(sym, terms, loc) =>
      val ts = terms.map(t => visitHeadTerm(t, m))
      ExecutableAst.Predicate.Head.RelAtom(sym, ts, loc)
    case SimplifiedAst.Predicate.Head.LatAtom(sym, terms, loc) =>
      val ts = terms.map(t => visitHeadTerm(t, m))
      ExecutableAst.Predicate.Head.LatAtom(sym, ts, loc)
  }

  private def visitBodyPredicate(p0: SimplifiedAst.Predicate.Body, m: TopLevel)(implicit flix: Flix): ExecutableAst.Predicate.Body = p0 match {
    case SimplifiedAst.Predicate.Body.RelAtom(sym, polarity, terms, loc) =>
      val termsArray = terms.map(t => visitBodyTerm(t, m))
      val index2var: Array[Symbol.VarSym] = {
        val r = new Array[Symbol.VarSym](termsArray.length)
        var i = 0
        while (i < r.length) {
          termsArray(i) match {
            case ExecutableAst.Term.Body.Var(sym, _, _) =>
              r(i) = sym
            case _ => // nop
          }
          i = i + 1
        }
        r
      }
      ExecutableAst.Predicate.Body.RelAtom(sym, polarity, termsArray, index2var.toList, loc)

    case SimplifiedAst.Predicate.Body.LatAtom(sym, polarity, terms, loc) =>
      val termsArray = terms.map(t => visitBodyTerm(t, m))
      val index2var: Array[Symbol.VarSym] = {
        val r = new Array[Symbol.VarSym](termsArray.length)
        var i = 0
        while (i < r.length) {
          termsArray(i) match {
            case ExecutableAst.Term.Body.Var(sym, _, _) =>
              r(i) = sym
            case _ => // nop
          }
          i = i + 1
        }
        r
      }
      ExecutableAst.Predicate.Body.LatAtom(sym, polarity, termsArray, index2var.toList, loc)
    case SimplifiedAst.Predicate.Body.Filter(name, terms, loc) =>
      val termsArray = terms.map(t => visitBodyTerm(t, m))
      ExecutableAst.Predicate.Body.Filter(name, termsArray, loc)
    case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) =>
      ExecutableAst.Predicate.Body.Loop(sym, visitHeadTerm(term, m), loc)
  }

  private def visitHeadTerm(t0: SimplifiedAst.Term.Head, m: TopLevel)(implicit flix: Flix): ExecutableAst.Term.Head = t0 match {
    case SimplifiedAst.Term.Head.Var(sym, tpe, loc) => ExecutableAst.Term.Head.Var(sym, tpe, loc)
    case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => toValueOpt(lit) match {
      case Some(value) => ExecutableAst.Term.Head.Lit(value, tpe, loc)
      case None => ExecutableAst.Term.Head.Cst(lit2sym(lit, m), tpe, loc)
    }
    case SimplifiedAst.Term.Head.App(name, args, tpe, loc) =>
      ExecutableAst.Term.Head.App(name, args, tpe, loc)
  }

  private def visitBodyTerm(t0: SimplifiedAst.Term.Body, m: TopLevel)(implicit flix: Flix): ExecutableAst.Term.Body = t0 match {
    case SimplifiedAst.Term.Body.Wild(tpe, loc) => ExecutableAst.Term.Body.Wild(tpe, loc)
    case SimplifiedAst.Term.Body.Var(sym, tpe, loc) => ExecutableAst.Term.Body.Var(sym, tpe, loc)
    case SimplifiedAst.Term.Body.Lit(lit, tpe, loc) => toValueOpt(lit) match {
      case Some(value) => ExecutableAst.Term.Body.Lit(value, tpe, loc)
      case None => ExecutableAst.Term.Body.Cst(lit2sym(lit, m), tpe, loc)
    }
    case SimplifiedAst.Term.Body.Pat(pat, tpe, loc) => ExecutableAst.Term.Body.Pat(visitPattern(pat), tpe, loc)
  }

  private def visitAttribute(a0: SimplifiedAst.Attribute): ExecutableAst.Attribute =
    ExecutableAst.Attribute(a0.name, a0.tpe)

  private def visitFormalParam(p0: SimplifiedAst.FormalParam): ExecutableAst.FormalParam =
    ExecutableAst.FormalParam(p0.sym, p0.tpe)

  private def visitFreeVar(v0: SimplifiedAst.FreeVar): ExecutableAst.FreeVar =
    ExecutableAst.FreeVar(v0.sym, v0.tpe)

  private def visitProperty(p0: SimplifiedAst.Property, m: TopLevel)(implicit flix: Flix): ExecutableAst.Property =
    ExecutableAst.Property(p0.law, p0.defn, visitExp(p0.exp, m))

  private def toValueOpt(exp0: SimplifiedAst.Expression): Option[ProxyObject] = exp0 match {
    case SimplifiedAst.Expression.True => Some(new ProxyObject(java.lang.Boolean.TRUE, null, null, null))
    case SimplifiedAst.Expression.False => Some(new ProxyObject(java.lang.Boolean.FALSE, null, null, null))
    case SimplifiedAst.Expression.Char(lit) => Some(new ProxyObject(new java.lang.Character(lit), null, null, null))
    case SimplifiedAst.Expression.Float32(lit) => Some(new ProxyObject(new java.lang.Float(lit), null, null, null))
    case SimplifiedAst.Expression.Float64(lit) => Some(new ProxyObject(new java.lang.Double(lit), null, null, null))
    case SimplifiedAst.Expression.Int8(lit) => Some(new ProxyObject(new java.lang.Byte(lit), null, null, null))
    case SimplifiedAst.Expression.Int16(lit) => Some(new ProxyObject(new java.lang.Short(lit), null, null, null))
    case SimplifiedAst.Expression.Int32(lit) => Some(new ProxyObject(new java.lang.Integer(lit), null, null, null))
    case SimplifiedAst.Expression.Int64(lit) => Some(new ProxyObject(new java.lang.Long(lit), null, null, null))
    case SimplifiedAst.Expression.BigInt(lit) => Some(new ProxyObject(lit, null, null, null))
    case SimplifiedAst.Expression.Str(lit) => Some(new ProxyObject(lit, null, null, null))
    case _ => None
  }

  private def lit2sym(exp0: SimplifiedAst.Expression, m: TopLevel)(implicit flix: Flix): Symbol.DefnSym = {
    implicit val _ = flix.genSym
    // Generate a top-level function for the constant.
    val sym = Symbol.freshDefnSym("lit")
    val lit = visitExp(exp0, m)
    val ann = Ast.Annotations.Empty
    val mod = Ast.Modifiers(List(Ast.Modifier.Synthetic))
    val varX = Symbol.freshVarSym("_unit")
    varX.setStackOffset(0)
    val fparam = ExecutableAst.FormalParam(varX, Type.Unit)
    val fs = List(fparam)
    val tpe = Type.mkArrow(Type.Unit, exp0.tpe)
    val defn = ExecutableAst.Def(ann, mod, sym, fs, lit, tpe, exp0.loc)
    m += (sym -> defn)
    sym
  }

}
