/*
 * Copyright 2021 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, Scheme, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

// TODO: Add doc

object Lowering extends Phase[Root, Root] {

  // TODO: Add doc
  // TODO: Return validations?
  // TODO: Need implicits?

  private val SL: SourcePosition = SourcePosition.Unknown

  private object Defs {
    lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.solve")
    lazy val Union: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.union")
    lazy val IsSubsetOf: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.isSubsetOf")
    lazy val Project: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.project")

    lazy val Box: Symbol.SigSym = Symbol.mkSigSym(Symbol.mkClassSym(Name.NName(SL, Nil, SL), Name.Ident(SL, "Boxable", SL)), Name.Ident(SL, "box", SL))

    lazy val Lift1: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift1")
    lazy val Lift2: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift2")
    lazy val Lift3: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift3")
    lazy val Lift4: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift4")
    lazy val Lift5: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift5")

    /**
      * Returns the definition associated with the given symbol `sym`.
      */
    def lookup(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): Def = root.defs.get(sym) match {
      case None => throw InternalCompilerException(s"Symbol '$sym' not found. Missing library?")
      case Some(d) => d
    }
  }

  private object Enums {
    lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Datalog")
    lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Constraint")

    lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.HeadPredicate")
    lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyPredicate")

    lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.HeadTerm")
    lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyTerm")

    lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.PredSym")
    lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.VarSym")

    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
    lazy val SourceLocation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.SourceLocation")

    lazy val Comparison: Symbol.EnumSym = Symbol.mkEnumSym("Comparison")
    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym("Boxed")
  }

  private object Types {
    //
    // Data Types
    //
    lazy val Datalog: Type = Type.mkEnum(Enums.Datalog, Boxed :: Nil)
    lazy val Constraint: Type = Type.mkEnum(Enums.Constraint, Boxed :: Nil)

    lazy val HeadPredicate: Type = Type.mkEnum(Enums.HeadPredicate, Boxed :: Nil)
    lazy val BodyPredicate: Type = Type.mkEnum(Enums.BodyPredicate, Boxed :: Nil)

    lazy val HeadTerm: Type = Type.mkEnum(Enums.HeadTerm, Boxed :: Nil)
    lazy val BodyTerm: Type = Type.mkEnum(Enums.BodyTerm, Boxed :: Nil)

    lazy val PredSym: Type = Type.mkEnum(Enums.PredSym, Nil)
    lazy val VarSym: Type = Type.mkEnum(Enums.VarSym, Nil)

    lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil)
    lazy val SourceLocation: Type = Type.mkEnum(Enums.SourceLocation, Nil)

    lazy val Comparison: Type = Type.mkEnum(Enums.Comparison, Nil)
    lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil)


    //
    // Function Types.
    //
    lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog)
    lazy val ComposeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog)
    lazy val EntailsType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Type.Bool)
    lazy val ProjectType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog)

  }

  /**
    * Translates internal Datalog constraints into Flix Datalog constraints.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Lowering") {
    val defs = ParOps.parMap(root.defs.values, (d: Def) => visitDef(d)(root, flix))

    // TODO: Matt: Visit classes and instances.

    val newDefs = defs.map(kv => kv.sym -> kv).toMap
    root.copy(defs = newDefs).toSuccess
  }

  /**
    * Lowers the given definition `defn0`.
    */
  private def visitDef(defn0: Def)(implicit root: Root, flix: Flix): Def = defn0 match {
    case Def(sym, spec0, impl0) =>
      val spec = visitSpec(spec0)
      val impl = visitImpl(impl0)
      Def(sym, spec, impl)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def visitSpec(spec0: Spec)(implicit root: Root, flix: Flix): Spec = spec0 match {
    case Spec(doc, ann, mod, tparams, fparams, declaredScheme, eff, loc) =>
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      Spec(doc, ann, mod, tparams, fs, ds, eff, loc)
  }

  /**
    * Lowers the given `impl0`.
    */
  private def visitImpl(impl0: Impl)(implicit root: Root, flix: Flix): Impl = impl0 match {
    case Impl(exp, inferredScheme) =>
      val e = visitExp(exp)
      val s = visitScheme(inferredScheme)
      Impl(e, s)
  }

  /**
    * Lowers the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit root: Root, flix: Flix): Expression = exp0 match {
    case Expression.Unit(_) => exp0

    case Expression.Null(_, _) => exp0

    case Expression.True(_) => exp0

    case Expression.False(_) => exp0

    case Expression.Char(_, _) => exp0

    case Expression.Float32(_, _) => exp0

    case Expression.Float64(_, _) => exp0

    case Expression.Int8(_, _) => exp0

    case Expression.Int16(_, _) => exp0

    case Expression.Int32(_, _) => exp0

    case Expression.Int64(_, _) => exp0

    case Expression.BigInt(_, _) => exp0

    case Expression.Str(_, _) => exp0

    case Expression.Default(tpe, loc) =>
      val t = visitType(tpe)
      Expression.Default(t, loc)

    case Expression.Wild(tpe, loc) =>
      val t = visitType(tpe)
      Expression.Wild(t, loc)

    case Expression.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Var(sym, t, loc)

    case Expression.Def(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Def(sym, t, loc)

    case Expression.Sig(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Sig(sym, t, loc)

    case Expression.Hole(sym, tpe, eff, loc) =>
      val t = visitType(tpe)
      Expression.Hole(sym, t, eff, loc)

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Lambda(p, e, t, loc)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      val t = visitType(tpe)
      Expression.Apply(e, es, t, eff, loc)

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Unary(sop, e, t, eff, loc)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Binary(sop, e1, e2, t, eff, loc)

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Let(sym, e1, e2, t, eff, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      Expression.IfThenElse(e1, e2, e3, t, eff, loc)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Stm(e1, e2, t, eff, loc)

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe)
      Expression.Match(e, rs, t, eff, loc)

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      val es = visitExps(exps)
      val rs = rules.map(visitChoiceRule)
      val t = visitType(tpe)
      Expression.Choose(es, rs, t, eff, loc)

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Tag(sym, tag, e, t, eff, loc)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      val es = visitExps(elms)
      val t = visitType(tpe)
      Expression.Tuple(es, t, eff, loc)

    case Expression.RecordEmpty(tpe, loc) =>
      val t = visitType(tpe)
      Expression.RecordEmpty(t, loc)

    case Expression.RecordSelect(exp, field, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.RecordSelect(e, field, t, eff, loc)

    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val t = visitType(tpe)
      Expression.RecordExtend(field, v, r, t, eff, loc)

    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      val r = visitExp(rest)
      val t = visitType(tpe)
      Expression.RecordRestrict(field, r, t, eff, loc)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      val es = visitExps(elms)
      val t = visitType(tpe)
      Expression.ArrayLit(es, t, eff, loc)

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      val e = visitExp(elm)
      val l = visitExp(len)
      val t = visitType(tpe)
      Expression.ArrayNew(e, l, t, eff, loc)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      Expression.ArrayLoad(b, i, t, eff, loc)

    case Expression.ArrayLength(base, eff, loc) =>
      val b = visitExp(base)
      Expression.ArrayLength(b, eff, loc)

    case Expression.ArrayStore(base, index, elm, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val e = visitExp(elm)
      Expression.ArrayStore(b, i, e, loc)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = visitExp(base)
      val bi = visitExp(beginIndex)
      val ei = visitExp(endIndex)
      val t = visitType(tpe)
      Expression.ArraySlice(b, bi, ei, t, loc)

    case Expression.Ref(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Ref(e, t, eff, loc)

    case Expression.Deref(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Deref(e, t, eff, loc)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.Assign(e1, e2, t, eff, loc)

    case Expression.Existential(fparam, exp, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      Expression.Existential(p, e, loc)

    case Expression.Universal(fparam, exp, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      Expression.Universal(p, e, loc)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Ascribe(e, t, eff, loc)

    case Expression.Cast(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Cast(e, t, eff, loc)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe)
      Expression.TryCatch(e, rs, t, eff, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      val as = visitExps(args)
      val t = visitType(tpe)
      Expression.InvokeConstructor(constructor, as, t, eff, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      val e = visitExp(exp)
      val as = visitExps(args)
      val t = visitType(tpe)
      Expression.InvokeMethod(method, e, as, t, eff, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      val as = visitExps(args)
      val t = visitType(tpe)
      Expression.InvokeStaticMethod(method, as, t, eff, loc)

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.GetField(field, e, t, eff, loc)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.PutField(field, e1, e2, t, eff, loc)

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe)
      Expression.GetStaticField(field, t, eff, loc)

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.PutStaticField(field, e, t, eff, loc)

    case Expression.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.NewChannel(e, t, eff, loc)

    case Expression.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.GetChannel(e, t, eff, loc)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      Expression.PutChannel(e1, e2, t, eff, loc)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val d = default.map(visitExp)
      val t = visitType(tpe)
      Expression.SelectChannel(rs, d, tpe, t, loc)

    case Expression.Spawn(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Spawn(e, t, eff, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Lazy(e, t, loc)

    case Expression.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Force(e, t, eff, loc)

    case Expression.FixpointConstraintSet(cs, _, _, loc) =>
      mkDatalog(cs, loc)

    case Expression.FixpointCompose(exp1, exp2, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Union)
      val defExp = Expression.Def(defn.sym, Types.ComposeType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Types.Datalog
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointSolve(exp, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Solve)
      val defExp = Expression.Def(defn.sym, Types.SolveType, loc)
      val argExps = visitExp(exp) :: Nil
      val resultType = Types.Datalog
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
      val defn = Defs.lookup(Defs.Project)
      val defExp = Expression.Def(defn.sym, Types.ProjectType, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
      val defn = Defs.lookup(Defs.IsSubsetOf)
      val defExp = Expression.Def(defn.sym, Types.EntailsType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Type.Bool
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) =>
      // TODO: Replace FixpointFold with alternative.
      throw InternalCompilerException("Deprecated Expression: FixpointFold")
  }

  /**
    * Lowers the given list of expressions `exps0`.
    */
  private def visitExps(exps0: List[Expression])(implicit root: Root, flix: Flix): List[Expression] = exps0.map(visitExp)

  /**
    * Lowers the given pattern `pat0`.
    */
  private def visitPat(pat0: Pattern)(implicit root: Root, flix: Flix): Pattern = pat0 match {
    case Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Wild(t, loc)

    case Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Var(sym, t, loc)

    case Pattern.Unit(_) => pat0

    case Pattern.True(_) => pat0

    case Pattern.False(_) => pat0

    case Pattern.Char(_, _) => pat0

    case Pattern.Float32(_, _) => pat0

    case Pattern.Float64(_, _) => pat0

    case Pattern.Int8(_, _) => pat0

    case Pattern.Int16(_, _) => pat0

    case Pattern.Int32(_, _) => pat0

    case Pattern.Int64(_, _) => pat0

    case Pattern.BigInt(_, _) => pat0

    case Pattern.Str(_, _) => pat0

    case Pattern.Tag(sym, tag, pat, tpe, loc) =>
      val p = visitPat(pat)
      val t = visitType(tpe)
      Pattern.Tag(sym, tag, p, t, loc)

    case Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      Pattern.Tuple(es, t, loc)

    case Pattern.Array(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      Pattern.Array(es, t, loc)

    case Pattern.ArrayTailSpread(elms, sym, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      Pattern.ArrayTailSpread(es, sym, t, loc)

    case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      Pattern.ArrayHeadSpread(sym, es, t, loc)
  }

  /**
    * Lowers the given scheme `sc0`.
    */
  private def visitScheme(sc0: Scheme)(implicit root: Root, flix: Flix): Scheme = sc0 match {
    case Scheme(quantifiers, constraints, base) =>
      // TODO: What about constraints?
      val b = visitType(base)
      Scheme(quantifiers, constraints, b)
  }

  /**
    * Lowers the given type `tpe0`.
    */
  private def visitType(tpe0: Type)(implicit root: Root, flix: Flix): Type = {
    def visit(tpe: Type): Type = tpe match {
      case Type.Var(id, kind, rigidity, text) => kind match {
        case Kind.Schema => Type.Var(id, Kind.Star, rigidity, text)
        case _ => tpe0
      }

      case Type.Cst(tc, loc) => tpe0

      case Type.Apply(tpe1, tpe2) =>
        val t1 = visitType(tpe1)
        val t2 = visitType(tpe2)
        Type.Apply(t1, t2)

      case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe0'.")
    }

    if (tpe0.kind == Kind.Schema)
      Types.Datalog
    else
      visit(tpe0)
  }

  /**
    * Lowers the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam)(implicit root: Root, flix: Flix): FormalParam = fparam0 match {
    case FormalParam(sym, mod, tpe, loc) =>
      val t = visitType(tpe)
      FormalParam(sym, mod, t, loc)
  }

  /**
    * Lowers the given choice rule `rule0`.
    */
  private def visitChoiceRule(rule0: ChoiceRule)(implicit root: Root, flix: Flix): ChoiceRule = rule0 match {
    case ChoiceRule(pat, exp) =>
      val p = pat.map {
        case p@ChoicePattern.Wild(loc) => p
        case p@ChoicePattern.Absent(loc) => p
        case ChoicePattern.Present(sym, tpe, loc) =>
          val t = visitType(tpe)
          ChoicePattern.Present(sym, t, loc)
      }
      val e = visitExp(exp)
      ChoiceRule(p, e)
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def visitCatchRule(rule0: CatchRule)(implicit root: Root, flix: Flix): CatchRule = rule0 match {
    case CatchRule(sym, clazz, exp) =>
      val e = visitExp(exp)
      CatchRule(sym, clazz, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: MatchRule)(implicit root: Root, flix: Flix): MatchRule = rule0 match {
    case MatchRule(pat, guard, exp) =>
      val p = visitPat(pat)
      val g = visitExp(guard)
      val e = visitExp(exp)
      MatchRule(p, g, e)
  }

  /**
    * Lowers the given select channel rule `rule0`.
    */
  private def visitSelectChannelRule(rule0: SelectChannelRule)(implicit root: Root, flix: Flix): SelectChannelRule = rule0 match {
    case SelectChannelRule(sym, chan, exp) =>
      val c = visitExp(chan)
      val e = visitExp(exp)
      SelectChannelRule(sym, c, e)
  }

  /**
    * Constructs a `Fixpoint/Ast.Datalog` value from the given list of Datalog constraints `cs`.
    */
  private def mkDatalog(cs: List[Constraint], loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val factExps = cs.filter(c => c.body.isEmpty).map(visitConstraint)
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(visitConstraint)

    val factArrayExp = mkArray(factExps, Types.Constraint, loc)
    val ruleArrayExp = mkArray(ruleExps, Types.Constraint, loc)

    val innerExp = mkTuple(List(factArrayExp, ruleArrayExp), loc)
    mkTag(Enums.Datalog, "Datalog", innerExp, Types.Datalog, loc)
  }

  /**
    * Lowers the given constraint `c0`.
    */
  private def visitConstraint(c0: Constraint)(implicit root: Root, flix: Flix): Expression = c0 match {
    case Constraint(_, head, body, loc) =>
      val headExp = visitHeadPred(head)
      val bodyExp = mkArray(body.map(visitBodyPred), Types.BodyPredicate, loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(headExp :: bodyExp :: locExp :: Nil, loc)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(p0: Predicate.Head)(implicit root: Root, flix: Flix): Expression = p0 match {
    case Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val termsExp = mkArray(terms.map(visitHeadTerm), Types.HeadTerm, loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(predSymExp :: termsExp :: locExp :: Nil, loc)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)

    case Head.Union(exp, tpe, loc) =>
      throw InternalCompilerException("Deprecated Expression") // TODO: Replace Union with some alternative.
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(p0: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p0 match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val predSymExp = mkPredSym(pred)
      val polarityExp = mkPolarity(polarity, loc)
      val termsExp = mkArray(terms.map(visitBodyTerm), Types.BodyTerm, loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(predSymExp :: termsExp :: locExp :: Nil, loc)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case Body.Guard(exp, loc) =>
      // TODO: need to only consider variables that are not bound by the local scope.
      // See e.g. testGuardQuantAndCapturedVar02
      mkGuardOrAppTerm(isGuard = true, exp, loc)
  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def visitHeadTerm(exp0: Expression)(implicit root: Root, flix: Flix): Expression = {
    //
    // We need to consider three cases:
    //
    // Case 1: The expression is quantified variable. We translate it to a variable term.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression does contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case Expression.Var(sym, _, loc) =>
        // Case 1: Variable term.
        mkHeadTermVar(sym)

      case _ =>
        // Call TypedAstOps.freeVarsOf
        val quantifiedVars = Nil // TODO

        if (quantifiedVars.isEmpty) {
          // Case 2: Evaluate to (boxed) value.
          mkHeadTermLit(box(exp0))
        } else {
          // Case 3: Translate to application term.

          // TODO: use TypedAstOps.substitute.

          ???
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def visitBodyTerm(pat0: Pattern)(implicit root: Root, flix: Flix): Expression = pat0 match {
    case Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case Pattern.Var(sym, _, loc) =>
      mkBodyTermVar(sym)

    case Pattern.Unit(loc) =>
      mkBodyTermLit(box(Expression.Unit(loc)))

    case Pattern.True(loc) =>
      mkBodyTermLit(box(Expression.True(loc)))

    case Pattern.False(loc) =>
      mkBodyTermLit(box(Expression.False(loc)))

    case Pattern.Char(lit, loc) =>
      mkBodyTermLit(box(Expression.Char(lit, loc)))

    case Pattern.Float32(lit, loc) =>
      mkBodyTermLit(box(Expression.Float32(lit, loc)))

    case Pattern.Float64(lit, loc) =>
      mkBodyTermLit(box(Expression.Float64(lit, loc)))

    case Pattern.Int8(lit, loc) =>
      mkBodyTermLit(box(Expression.Int8(lit, loc)))

    case Pattern.Int16(lit, loc) =>
      mkBodyTermLit(box(Expression.Int16(lit, loc)))

    case Pattern.Int32(lit, loc) =>
      mkBodyTermLit(box(Expression.Int32(lit, loc)))

    case Pattern.Int64(lit, loc) =>
      mkBodyTermLit(box(Expression.Int64(lit, loc)))

    case Pattern.BigInt(lit, loc) =>
      mkBodyTermLit(box(Expression.BigInt(lit, loc)))

    case Pattern.Str(lit, loc) =>
      mkBodyTermLit(box(Expression.Str(lit, loc)))

    // TODO: What other expressions to support as body terms?

    case Pattern.Tag(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.Tuple(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.Array(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.ArrayTailSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.ArrayHeadSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym)(implicit root: Root, flix: Flix): Expression = {
    val loc = sym.loc
    val symExp = mkVarSym(sym)
    val locExp = mkSourceLocation(sym.loc)
    val innerExp = mkTuple(symExp :: locExp :: Nil, loc)
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: Expression)(implicit root: Root, flix: Flix): Expression = {
    val loc = exp.loc
    mkTag(Enums.HeadTerm, "Lit", exp, Types.HeadTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation): Expression = {
    val innerExp = Expression.Unit(loc)
    mkTag(Enums.BodyTerm, "Wild", innerExp, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym): Expression = {
    val loc = sym.loc
    val symExp = mkVarSym(sym)
    val locExp = mkSourceLocation(sym.loc)
    val innerExp = mkTuple(symExp :: locExp :: Nil, loc)
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp0: Expression)(implicit root: Root, flix: Flix): Expression = {
    mkTag(Enums.BodyTerm, "Lit", exp0, Types.BodyTerm, exp0.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Ast.Polarity, loc: SourceLocation): Expression = p match {
    case Polarity.Positive =>
      val innerExp = Expression.Unit(loc)
      mkTag(Enums.Polarity, "Positive", innerExp, Types.Polarity, loc)

    case Polarity.Negative =>
      val innerExp = Expression.Unit(loc)
      mkTag(Enums.Polarity, "Negative", innerExp, Types.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred): Expression = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = Expression.Str(sym, loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(nameExp :: locExp :: Nil, loc)
      mkTag(Enums.PredSym, "PredSym", innerExp, Types.PredSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): Expression = {
    val loc = sym.loc
    val nameExp = Expression.Str(sym.text, loc)
    val locExp = mkSourceLocation(loc)
    val innerExp = mkTuple(nameExp :: locExp :: Nil, loc)
    mkTag(Enums.VarSym, "VarSym", innerExp, Types.VarSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.SourceLocation` from the given source location `loc`.
    */
  private def mkSourceLocation(loc: SourceLocation): Expression = {
    val name = Expression.Str(loc.source.format, loc)
    val beginLine = Expression.Int32(loc.beginLine, loc)
    val beginCol = Expression.Int32(loc.beginCol, loc)
    val endLine = Expression.Int32(loc.endLine, loc)
    val endCol = Expression.Int32(loc.endCol, loc)
    val innerExp = mkTuple(List(name, beginLine, beginCol, endLine, endCol), loc)
    mkTag(Enums.SourceLocation, "SourceLocation", innerExp, Types.SourceLocation, loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: Expression)(implicit root: Root, flix: Flix): Expression = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed)
    Expression.Sig(Defs.Box, tpe, loc)
  }

  /**
    * Returns a `Fixpoint/Ast.BodyPredicate.GuardX` or `Fixpoint/Ast.Term.AppX`.
    */
  // TODO: Deal with the case where `exp` has no free variables.
  private def mkGuardOrAppTerm(isGuard: Boolean, exp: Expression, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    // Construct the location expression.
    val locExp = mkSourceLocation(loc)

    // Compute the free variables in `exp` (and convert to list to ensure stable iteration order).
    val fvs = freeVars(exp).toList

    // Check that we have <= 5 free variables.
    if (fvs.length > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.")
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      // Construct a lambda that takes the unit argument.
      val fparam = FormalParam(Symbol.freshVarSym("_unit", loc), Ast.Modifiers.Empty, Type.Unit, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe)
      val innerExp = Expression.Lambda(fparam, exp, tpe, loc)
      // TODO: Refactor
      return if (isGuard)
        mkTag(Enums.BodyPredicate, s"Guard0", innerExp, Types.BodyPredicate, loc)
      else
        mkTag(Enums.HeadTerm, s"App0", innerExp, Types.HeadTerm, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, tpe)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldLeft(freshExp) {
      case (acc, (oldSym, tpe)) =>
        val freshSym = freshVars(oldSym)
        val fparam = FormalParam(freshSym, Ast.Modifiers.Empty, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe)
        Expression.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs, Type.Bool)

    // Compute the number of free variables.
    val arity = fvs.length

    // Construct the `Fixpoint.Ast/BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps ::: locExp :: Nil, loc)

    if (isGuard)
      mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
    else
      mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given list of parameters `params`.
    */
  private def liftX(exp0: Expression, params: List[(Symbol.VarSym, Type)], resultType: Type): Expression = {
    // Compute the liftXb symbol.
    val arity = params.length
    val sym = resultType.typeConstructor match {
      case Some(TypeConstructor.Bool) => Symbol.mkDefnSym(s"Boxable.lift${arity}b")
      case _ => Symbol.mkDefnSym(s"Boxable.lift${arity}")
    }

    //
    // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> `resultType`.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> `resultType`.
    val argType = Type.mkPureCurriedArrow(params.map(_._2), Type.Bool)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> `resultType`.
    val returnType = Type.mkPureCurriedArrow(params.map(_ => Types.Boxed), Type.Bool)

    // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> `resultType`).
    val liftType = Type.mkPureArrow(argType, returnType)

    // Construct a call to the liftX function.
    val defn = Expression.Def(sym, liftType, exp0.loc)
    val args = List(exp0)
    val tpe = returnType
    val eff = Type.Pure
    Expression.Apply(defn, args, tpe, eff, exp0.loc)
  }

  /**
    * Returns a pure array expression constructed from the given list of expressions `exps`.
    */
  private def mkArray(exps: List[Expression], elmType: Type, loc: SourceLocation): Expression = {
    val tpe = Type.mkArray(elmType)
    val eff = Type.Pure
    Expression.ArrayLit(exps, tpe, eff, loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exp: Expression, tpe: Type, loc: SourceLocation): Expression = {
    Expression.Tag(sym, Name.Tag(tag, loc), exp, tpe, Type.Pure, loc)
  }

  /**
    * Returns a pure tuple expression constructed from the given list of expressions `exps`.
    */
  private def mkTuple(exps: List[Expression], loc: SourceLocation): Expression = {
    val tpe = Type.mkTuple(exps.map(_.tpe))
    val eff = Type.Pure
    Expression.Tuple(exps, tpe, eff, loc)
  }


  // TODO: Move into TypedAstOps

  /**
    * Returns the free variables in the given expression `exp0`.
    */
  private def freeVars(exp0: Expression): Map[Symbol.VarSym, Type] = exp0 match {
    case Expression.Unit(_) => Map.empty

    case Expression.Null(_, _) => Map.empty

    case Expression.True(_) => Map.empty

    case Expression.False(_) => Map.empty

    case Expression.Char(_, _) => Map.empty

    case Expression.Float32(_, _) => Map.empty

    case Expression.Float64(_, _) => Map.empty

    case Expression.Int8(_, _) => Map.empty

    case Expression.Int16(_, _) => Map.empty

    case Expression.Int32(_, _) => Map.empty

    case Expression.Int64(_, _) => Map.empty

    case Expression.BigInt(_, _) => Map.empty

    case Expression.Str(_, _) => Map.empty

    case Expression.Default(_, _) => Map.empty

    case Expression.Wild(_, _) => Map.empty

    case Expression.Var(sym, tpe, _) => Map(sym -> tpe)

    case Expression.Def(_, _, _) => Map.empty

    case Expression.Sig(_, _, _) => Map.empty

    case Expression.Hole(_, _, _, _) => Map.empty

    case Expression.Lambda(fparam, exp, _, _) =>
      freeVars(exp) - fparam.sym

    case Expression.Apply(exp, exps, _, _, _) =>
      exps.foldLeft(freeVars(exp)) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expression.Unary(_, exp, _, _, _) =>
      freeVars(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ??? // TODO
    case Expression.Stm(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.Match(exp, rules, tpe, eff, loc) => ??? // TODO
    case Expression.Choose(exps, rules, tpe, eff, loc) => ??? // TODO

    case Expression.Tag(_, _, exp, _, _, _) =>
      freeVars(exp)

    case Expression.Tuple(elms, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expression.RecordEmpty(tpe, loc) => ??? // TODO
    case Expression.RecordSelect(exp, field, tpe, eff, loc) => ??? // TODO
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => ??? // TODO
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLit(elms, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLength(base, eff, loc) => ??? // TODO
    case Expression.ArrayStore(base, index, elm, loc) => ??? // TODO
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ??? // TODO
    case Expression.Ref(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Deref(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.Existential(fparam, exp, loc) => ??? // TODO
    case Expression.Universal(fparam, exp, loc) => ??? // TODO
    case Expression.Ascribe(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Cast(exp, tpe, eff, loc) => ??? // TODO
    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // TODO
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => ??? // TODO
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => ??? // TODO
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => ??? // TODO
    case Expression.GetField(field, exp, tpe, eff, loc) => ??? // TODO
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.GetStaticField(field, tpe, eff, loc) => ??? // TODO
    case Expression.PutStaticField(field, exp, tpe, eff, loc) => ??? // TODO
    case Expression.NewChannel(exp, tpe, eff, loc) => ??? // TODO
    case Expression.GetChannel(exp, tpe, eff, loc) => ??? // TODO
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // TODO
    case Expression.Spawn(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Lazy(exp, tpe, loc) => ??? // TODO
    case Expression.Force(exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => ??? // TODO
    case Expression.FixpointCompose(exp1, exp2, stf, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) => ??? // TODO
  }

  // TODO: Move into TypedAstOps
  // TODO: Should this also refresh variables in tree??
  private def substExp(exp0: Expression, subst: Map[Symbol.VarSym, Symbol.VarSym]): Expression = exp0 match {
    case Expression.Unit(_) => exp0

    case Expression.Null(_, _) => exp0

    case Expression.True(_) => exp0

    case Expression.False(_) => exp0

    case Expression.Char(_, _) => exp0

    case Expression.Float32(_, _) => exp0

    case Expression.Float64(_, _) => exp0

    case Expression.Int8(_, _) => exp0

    case Expression.Int16(_, _) => exp0

    case Expression.Int32(_, _) => exp0

    case Expression.Int64(_, _) => exp0

    case Expression.BigInt(_, _) => exp0

    case Expression.Str(_, _) => exp0

    case Expression.Default(_, _) => exp0

    case Expression.Wild(_, _) => exp0

    case Expression.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      Expression.Var(s, tpe, loc)

    case Expression.Def(_, _, _) => exp0

    case Expression.Sig(_, _, _) => exp0

    case Expression.Hole(_, _, _, _) => exp0

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val p = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      Expression.Lambda(p, e, tpe, loc)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val es = exps.map(substExp(_, subst))
      Expression.Apply(e, es, tpe, eff, loc)

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Unary(sop, e, tpe, eff, loc)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.Binary(sop, e1, e2, tpe, eff, loc)

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ??? // TODO
    case Expression.Stm(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.Match(exp, rules, tpe, eff, loc) => ??? // TODO
    case Expression.Choose(exps, rules, tpe, eff, loc) => ??? // TODO

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Tag(sym, tag, e, tpe, eff, loc)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      val es = elms.map(substExp(_, subst))
      Expression.Tuple(es, tpe, eff, loc)

    case Expression.RecordEmpty(tpe, loc) => ??? // TODO
    case Expression.RecordSelect(exp, field, tpe, eff, loc) => ??? // TODO
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => ??? // TODO
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLit(elms, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLength(base, eff, loc) => ??? // TODO
    case Expression.ArrayStore(base, index, elm, loc) => ??? // TODO
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ??? // TODO
    case Expression.Ref(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Deref(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.Existential(fparam, exp, loc) => ??? // TODO
    case Expression.Universal(fparam, exp, loc) => ??? // TODO
    case Expression.Ascribe(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Cast(exp, tpe, eff, loc) => ??? // TODO
    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // TODO
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => ??? // TODO
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => ??? // TODO
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => ??? // TODO
    case Expression.GetField(field, exp, tpe, eff, loc) => ??? // TODO
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.GetStaticField(field, tpe, eff, loc) => ??? // TODO
    case Expression.PutStaticField(field, exp, tpe, eff, loc) => ??? // TODO
    case Expression.NewChannel(exp, tpe, eff, loc) => ??? // TODO
    case Expression.GetChannel(exp, tpe, eff, loc) => ??? // TODO
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // TODO
    case Expression.Spawn(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Lazy(exp, tpe, loc) => ??? // TODO
    case Expression.Force(exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => ??? // TODO
    case Expression.FixpointCompose(exp1, exp2, stf, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) => ??? // TODO
  }

  // TODO: DOC
  private def substFormalParam(fparam0: FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fparam0 match {
    case FormalParam(sym, mod, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      FormalParam(s, mod, tpe, loc)
  }

}
