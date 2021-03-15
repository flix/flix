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
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, ChoicePattern, ChoiceRule, Constraint, Def, Expression, FormalParam, MatchRule, Pattern, Predicate, Root, SelectChannelRule}
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import scala.annotation.tailrec

// TODO: Add doc

object Lowering extends Phase[Root, Root] {

  // TODO: Add doc

  val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyPredicate")
  val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyTerm")
  val ConstraintSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Constraint")
  val DatalogSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Datalog")
  val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.HeadPredicate")
  val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.HeadTerm")
  val PolaritySym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
  val PredSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.PredSym")
  val SourceLocationSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.SourceLocation")
  val VarSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.VarSym")

  // TODO: Remove parameter from UnsafeBox?
  val ComparisonSym: Symbol.EnumSym = Symbol.mkEnumSym("Comparison")
  val UnsafeBox: Symbol.EnumSym = Symbol.mkEnumSym("UnsafeBox")

  object Defs {
    // TODO: Sort/rename
    val Solve: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.solve")
    val Compose: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.compose")
    val Entails: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.entails")
    val Project: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Solver.project")

    def lookup(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): Def = root.defs.get(sym) match {
      case None => throw InternalCompilerException(s"Symbol '$sym' not found. Missing library?")
      case Some(d) => d
    }
  }

  object Types {
    val SolveType: Type = Type.mkPureArrow(mkDatalogType(), mkDatalogType())
    val ComposeType: Type = Type.mkPureUncurriedArrow(List(mkDatalogType(), mkDatalogType()), mkDatalogType())
    val EntailsType: Type = Type.mkPureUncurriedArrow(List(mkDatalogType(), mkDatalogType()), Type.Bool)
    val ProjectType: Type = Type.mkPureUncurriedArrow(List(mkPredSymType(), mkDatalogType()), mkDatalogType())
  }

  /**
    * Translates internal Datalog constraints into Flix Datalog constraints.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Lowering") {
    val defs = ParOps.parMap(root.defs.values, (d: Def) => visitDef(d)(root, flix))

    // TODO: Visit expressions in other parts of the AST (e.g. in classes and instances.)

    root.copy(defs = defs.map(kv => kv.sym -> kv).toMap).toSuccess
  }

  // TODO: Return validations?

  private def visitDef(defn: Def)(implicit root: Root, flix: Flix): Def = {
    val e = visitExp(defn.exp)
    defn.copy(exp = e)
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

    case Expression.Default(_, _) => exp0 // TODO: Types?

    case Expression.Wild(_, _) => exp0 // TODO: Types?

    case Expression.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Var(sym, t, loc)

    case Expression.Def(_, _, _) => exp0 // TODO: Types?

    case Expression.Sig(_, _, _) => exp0 // TODO: Types?

    case Expression.Hole(_, _, _, _) => exp0 // TODO: Types?

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
      val defn = Defs.lookup(Defs.Compose)
      val defExp = Expression.Def(defn.sym, Types.ComposeType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = mkDatalogType()
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointSolve(exp, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Solve)
      val defExp = Expression.Def(defn.sym, Types.SolveType, loc)
      val argExps = visitExp(exp) :: Nil
      val resultType = mkDatalogType()
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
      val defn = Defs.lookup(Defs.Project)
      val defExp = Expression.Def(defn.sym, Types.ProjectType, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      val resultType = mkDatalogType()
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
      val defn = Defs.lookup(Defs.Entails)
      val defExp = Expression.Def(defn.sym, Types.EntailsType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Type.Bool
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) =>
      // TODO: Replace FixpointFold with alternative.
      throw InternalCompilerException("Deprecated Expression: FixpointFold")
  }

  /**
    * Lowers the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expression])(implicit root: Root, flix: Flix): List[Expression] = exps.map(visitExp)

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
    * Lowers the given type `tpe0`.
    */
  // TODO: What is the right way to do this replacement?
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
      mkDatalogType()
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
    * Constructs a Datalog program value.
    */
  private def mkDatalog(cs: List[Constraint], loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val factExps = cs.filter(c => c.body.isEmpty).map(visitConstraint)
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(visitConstraint)

    val factArrayExp = mkArray(factExps, mkConstraintType(), loc)
    val ruleArrayExp = mkArray(ruleExps, mkConstraintType(), loc)

    val innerExp = mkTuple(List(factArrayExp, ruleArrayExp), loc)
    mkTag(DatalogSym, "Datalog", innerExp, mkDatalogType(), loc)
  }

  /**
    * Translates the given [[Constraint]] into the Flix AST.
    */
  private def visitConstraint(c: Constraint)(implicit root: Root, flix: Flix): Expression = c match {
    case Constraint(_, head, body, loc) =>
      val headExp = visitHeadPred(head)
      val bodyExp = mkArray(body.map(visitBodyPred), mkBodyPredicateType(), loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(headExp :: bodyExp :: locExp :: Nil, loc)
      mkTag(ConstraintSym, "Constraint", innerExp, mkConstraintType(), loc)
  }

  /**
    * Translates the given [[Predicate.Head]] into the Flix AST.
    */
  private def visitHeadPred(p: Predicate.Head)(implicit root: Root, flix: Flix): Expression = p match {
    case Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val termsExp = mkArray(terms.map(visitHeadTerm), mkHeadTermType(), loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(predSymExp :: termsExp :: locExp :: Nil, loc)
      mkTag(HeadPredicate, "HeadAtom", innerExp, mkHeadPredicateType(), loc)

    case Head.Union(exp, tpe, loc) =>
      throw InternalCompilerException("Deprecated Expression") // TODO: Replace Union with some alternative.
  }

  /**
    * Translates the given [[Predicate.Body]] to the Flix AST.
    */
  private def visitBodyPred(p: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val predSymExp = mkPredSym(pred)
      val polarityExp = visitPolarity(polarity, loc)
      val termsExp = mkArray(terms.map(visitBodyTerm), mkBodyTermType(), loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(predSymExp :: termsExp :: locExp :: Nil, loc)
      mkTag(BodyPredicate, "BodyAtom", innerExp, mkBodyPredicateType(), loc)

    case Body.Guard(exp, loc) =>
      ??? // TODO: Add support for guards.
  }

  /**
    * Lowers the given head term `exp0` to a `Fixpoint/Ast.HeadTerm`.
    */
  @tailrec
  private def visitHeadTerm(exp0: Expression)(implicit root: Root, flix: Flix): Expression = exp0 match {
    case Expression.Var(sym, _, loc) => mkHeadTermVar(sym, loc)

    case Expression.Unit(loc) =>
      mkHeadTermLit(mkUnsafeBox(exp0, loc), loc)

    case Expression.True(loc) =>
      mkHeadTermLit(mkUnsafeBox(boxBool(exp0), loc), loc)

    case Expression.False(loc) =>
      mkHeadTermLit(mkUnsafeBox(boxBool(exp0), loc), loc)

    case Expression.Char(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxChar(exp0), loc), loc)

    case Expression.Float32(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxFloat32(exp0), loc), loc)

    case Expression.Float64(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxFloat64(exp0), loc), loc)

    case Expression.Int8(lit, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxInt8(exp0), loc), loc)

    case Expression.Int16(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxInt16(exp0), loc), loc)

    case Expression.Int32(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxInt32(exp0), loc), loc)

    case Expression.Int64(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(boxInt64(exp0), loc), loc)

    case Expression.BigInt(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(exp0, loc), loc)

    case Expression.Str(_, loc) =>
      mkHeadTermLit(mkUnsafeBox(exp0, loc), loc)

    case Expression.Ascribe(exp, _, _, _) => visitHeadTerm(exp)

    // TODO: Translate other expressions into function applications.

    case _ => throw InternalCompilerException(s"Unexpected expression: '$exp0'.")
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val symExp = mkVarSym(sym, loc)
    val locExp = mkSourceLocation(sym.loc)
    val innerExp = mkTuple(symExp :: locExp :: Nil, loc)
    mkTag(HeadTerm, "Var", innerExp, mkHeadTermType(), loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Lit` value which wraps the given expression `exp0`.
    */
  private def mkHeadTermLit(exp0: Expression, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    mkTag(HeadTerm, "Lit", exp0, mkHeadTermType(), loc)
  }


  /**
    * Lowers the given body term `pat0` (a subset of patterns).
    */
  private def visitBodyTerm(pat0: Pattern)(implicit root: Root, flix: Flix): Expression = pat0 match {
    case Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case Pattern.Var(sym, _, loc) =>
      mkBodyTermVar(sym, loc)

    case Pattern.Unit(loc) =>
      mkBodyTermLit(mkUnsafeBox(Expression.Unit(loc), loc))

    case Pattern.True(loc) =>
      mkBodyTermLit(mkUnsafeBox(boxBool(Expression.True(loc)), loc))

    case Pattern.False(loc) =>
      mkBodyTermLit(mkUnsafeBox(boxBool(Expression.False(loc)), loc))

    case Pattern.Char(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxChar(Expression.Char(lit, loc)), loc))

    case Pattern.Float32(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxFloat32(Expression.Float32(lit, loc)), loc))

    case Pattern.Float64(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxFloat64(Expression.Float64(lit, loc)), loc))

    case Pattern.Int8(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxInt8(Expression.Int8(lit, loc)), loc))

    case Pattern.Int16(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxInt16(Expression.Int16(lit, loc)), loc))

    case Pattern.Int32(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxInt32(Expression.Int32(lit, loc)), loc))

    case Pattern.Int64(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(boxInt64(Expression.Int64(lit, loc)), loc))

    case Pattern.BigInt(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(Expression.BigInt(lit, loc), loc))

    case Pattern.Str(lit, loc) =>
      mkBodyTermLit(mkUnsafeBox(Expression.Str(lit, loc), loc))

    // TODO: What other expressions to support as body terms?

    case Pattern.Tag(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.Tuple(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.Array(_, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.ArrayTailSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")

    case Pattern.ArrayHeadSpread(_, _, _, _) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.")
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Wild`.
    */
  private def mkBodyTermWild(loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val innerExp = Expression.Unit(loc)
    mkTag(BodyTerm, "Wild", innerExp, mkBodyTermType(), loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val symExp = mkVarSym(sym, loc)
    val locExp = mkSourceLocation(sym.loc)
    val innerExp = mkTuple(symExp :: locExp :: Nil, loc)
    mkTag(BodyTerm, "Var", innerExp, mkBodyTermType(), loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp0: Expression)(implicit root: Root, flix: Flix): Expression = {
    mkTag(BodyTerm, "Lit", exp0, mkBodyTermType(), exp0.loc)
  }

  /**
    * Translates a [[Polarity]] AST node to an expression.
    */
  private def visitPolarity(p: Ast.Polarity, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = p match {
    case Polarity.Positive =>
      val (_, tpe) = Scheme.instantiate(root.enums(PolaritySym).sc, InstantiateMode.Flexible)
      mkUnitTag(PolaritySym, "Positive", tpe, loc)

    case Polarity.Negative =>
      val (_, tpe) = Scheme.instantiate(root.enums(PolaritySym).sc, InstantiateMode.Flexible)
      mkUnitTag(PolaritySym, "Negative", tpe, loc)
  }

  /**
    * Translates the given [[Name.Pred]] to the Flix AST.
    */
  private def mkPredSym(pred: Name.Pred)(implicit root: Root, flix: Flix): Expression = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = Expression.Str(sym, loc)
      val locExp = mkSourceLocation(loc)
      val innerExp = mkTuple(nameExp :: locExp :: Nil, loc)
      mkTag(PredSym, "PredSym", innerExp, mkPredSymType(), loc)
  }

  /**
    * Translates the given [[Symbol.VarSym]] to the Flix AST.
    */
  private def mkVarSym(sym: Symbol.VarSym, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val nameExp = Expression.Str(sym.text, loc)
    val locExp = mkSourceLocation(loc)
    val innerExp = mkTuple(nameExp :: locExp :: Nil, loc)
    mkTag(VarSym, "VarSym", innerExp, mkVarSymType(), loc)
  }

  /**
    * Boxes the given Bool expression `e`.
    */
  private def boxBool(e: Expression): Expression = boxExp(e, java.lang.Boolean.TYPE, classOf[java.lang.Boolean])

  /**
    * Boxes the given Char expression `e`.
    */
  private def boxChar(e: Expression): Expression = boxExp(e, java.lang.Character.TYPE, classOf[java.lang.Character])

  /**
    * Boxes the given Float32 expression `e`.
    */
  private def boxFloat32(e: Expression): Expression = boxExp(e, java.lang.Float.TYPE, classOf[java.lang.Float])

  /**
    * Boxes the given Float64 expression `e`.
    */
  private def boxFloat64(e: Expression): Expression = boxExp(e, java.lang.Double.TYPE, classOf[java.lang.Double])

  /**
    * Boxes the given Int8 expression `e`.
    */
  private def boxInt8(e: Expression): Expression = boxExp(e, java.lang.Byte.TYPE, classOf[java.lang.Byte])

  /**
    * Boxes the given Int16 expression `e`.
    */
  private def boxInt16(e: Expression): Expression = boxExp(e, java.lang.Short.TYPE, classOf[java.lang.Short])

  /**
    * Boxes the given Int32 expression `e`.
    */
  private def boxInt32(e: Expression): Expression = boxExp(e, java.lang.Integer.TYPE, classOf[java.lang.Integer])

  /**
    * Boxes the given Int64 expression `e`.
    */
  private def boxInt64(e: Expression): Expression = boxExp(e, java.lang.Long.TYPE, classOf[java.lang.Long])

  /**
    * Boxes the given expression `e` with primitive type `primitive` and boxed type `boxed`.
    */
  private def boxExp[T, S](e: Expression, primitive: Class[T], boxed: Class[S]): Expression = {
    val m = boxed.getMethod("valueOf", primitive)
    val tpe = Type.mkNative(boxed)
    val eff = Type.Pure
    Expression.InvokeStaticMethod(m, List(e), tpe, eff, e.loc)
  }

  private def mkTag(sym: Symbol.EnumSym, tag: String, exp: Expression, tpe: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    Expression.Tag(sym, Name.Tag(tag, loc), exp, tpe, Type.Pure, loc)
  }

  private def mkUnitTag(sym: Symbol.EnumSym, tag: String, tpe: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val exp = Expression.Unit(loc)
    mkTag(sym, tag, exp, tpe, loc)
  }

  /**
    * Returns the given expression `exp` wrapped in the `UnsafeBox` value.
    */
  private def mkUnsafeBox(exp: Expression, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    // TODO: Use loc of exp?
    // TODO: Possibly call UnsafeBox.box(?)
    val cmpType = Type.Cst(TypeConstructor.Enum(ComparisonSym, Kind.Star), loc)
    val tpe = Type.mkPureUncurriedArrow(List(exp.tpe, exp.tpe), cmpType)
    val cmpExp = Expression.Null(tpe, loc) // TODO: Dont use null, but pass the actual comparator
    val innerExp = mkTuple(exp :: cmpExp :: Nil, loc)

    val objectType = Type.mkNative(classOf[java.lang.Object])
    val unsafeBoxType = Type.mkEnum(UnsafeBox, objectType :: Nil)

    mkTag(UnsafeBox, "UnsafeBox", innerExp, unsafeBoxType, loc)
  }

  /**
    * Translates the given [[SourceLocation]] to the Flix AST.
    */
  private def mkSourceLocation(loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val name = Expression.Str(loc.source.format, loc)
    val beginLine = Expression.Int32(loc.beginLine, loc)
    val beginCol = Expression.Int32(loc.beginCol, loc)
    val endLine = Expression.Int32(loc.endLine, loc)
    val endCol = Expression.Int32(loc.endCol, loc)
    val innerExp = mkTuple(List(name, beginLine, beginCol, endLine, endCol), loc)
    mkTag(SourceLocationSym, "SourceLocation", innerExp, mkSourceLocationType(), loc)
  }

  /**
    * Returns the type `Fixpoint/Ast.Datalog`.
    */
  private def mkDatalogType(): Type = {
    val objectType = Type.mkNative(classOf[java.lang.Object])
    val innerType = Type.mkEnum(UnsafeBox, objectType :: Nil)
    Type.mkEnum(DatalogSym, innerType :: Nil)
  }

  /**
    * Returns the type `Fixpoint/Ast.Constraint`.
    */
  private def mkConstraintType(): Type = {
    val objectType = Type.mkNative(classOf[java.lang.Object])
    val innerType = Type.mkEnum(UnsafeBox, objectType :: Nil)
    Type.mkEnum(ConstraintSym, innerType :: Nil)
  }

  /**
    * Returns the type `Fixpoint/Ast.HeadPredicate`.
    */
  private def mkHeadPredicateType(): Type = {
    val objectType = Type.mkNative(classOf[java.lang.Object])
    val innerType = Type.mkEnum(UnsafeBox, objectType :: Nil)
    Type.mkEnum(HeadPredicate, innerType :: Nil)
  }

  /**
    * Returns the type `Fixpoint/Ast.BodyPredicate`.
    */
  private def mkBodyPredicateType(): Type = {
    val objectType = Type.mkNative(classOf[java.lang.Object])
    val innerType = Type.mkEnum(UnsafeBox, objectType :: Nil)
    Type.mkEnum(BodyPredicate, innerType :: Nil)
  }

  /**
    * Returns the type `Fixpoint/Ast.PredSym`.
    */
  private def mkPredSymType(): Type = Type.mkEnum(PredSym, Nil)

  /**
    * Returns the type `Fixpoint/Ast.VarSym`.
    */
  private def mkVarSymType(): Type = Type.mkEnum(VarSym, Nil)

  /**
    * Returns the type of `Fixpoint/HeadTerm[UnsafeBox[##java.lang.Object]].`
    */
  private def mkHeadTermType(): Type = {
    val objectType = Type.mkNative(classOf[java.lang.Object])
    val innerType = Type.mkEnum(UnsafeBox, objectType :: Nil)
    Type.mkEnum(HeadTerm, innerType :: Nil)
  }

  /**
    * Returns the type of `Fixpoint/BodyTerm[UnsafeBox[##java.lang.Object]].`
    */
  private def mkBodyTermType(): Type = {
    val objectType = Type.mkNative(classOf[java.lang.Object])
    val innerType = Type.mkEnum(UnsafeBox, objectType :: Nil)
    Type.mkEnum(BodyTerm, innerType :: Nil)
  }

  /**
    * Returns the type of `Fixpoint/Ast.SourceLocation`
    */
  private def mkSourceLocationType()(implicit root: Root, flix: Flix): Type = Type.mkEnum(SourceLocationSym, Nil)

  /**
    * Returns a pure array expression constructed from the given list of expressions `exps`.
    */
  private def mkArray(exps: List[Expression], elmType: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    val tpe = Type.mkArray(elmType)
    val eff = Type.Pure
    Expression.ArrayLit(exps, tpe, eff, loc)
  }

  /**
    * Returns a pure tuple expression constructed from the given list of expressions `exps`.
    */
  private def mkTuple(exps: List[Expression], loc: SourceLocation): Expression = {
    val tpe = Type.mkTuple(exps.map(_.tpe))
    val eff = Type.Pure
    Expression.Tuple(exps, tpe, eff, loc)
  }

}
