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
import ca.uwaterloo.flix.language.ast.Ast.Denotation.{Latticenal, Relational}
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Polarity}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, Scheme, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint/Ast` values (which are ordinary Flix values).
  * This allows the Datalog engine to be implemented as an ordinary Flix program.
  *
  * In addition to translating expressions, types must also be translated from
  * Schema types to enum types.
  *
  * Finally, values must be boxed using the Boxable.
  */

// TODO: Long-term improvements:
// - Return a [[Validation]] from visitExp etc.
// - Decide which expressions to allow as head and body terms.

object Lowering extends Phase[Root, Root] {

  private object Defs {
    lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.solve")
    lazy val Merge: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.union")
    lazy val Filter: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.project")

    lazy val Lift1: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift1")
    lazy val Lift2: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift2")
    lazy val Lift3: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift3")
    lazy val Lift4: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift4")
    lazy val Lift5: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.lift5")

//    lazy val NewChannel: Symbol.DefnSym = Symbol.mkDefnSym("Channel.new")
//    lazy val GetChannel: Symbol.DefnSym = Symbol.mkDefnSym("Channel.get")
//    lazy val PutChannel: Symbol.DefnSym = Symbol.mkDefnSym("Channel.put")

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

    lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Denotation")
    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
    lazy val SourceLocation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.SourceLocation")

    lazy val Comparison: Symbol.EnumSym = Symbol.mkEnumSym("Comparison")
    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym("Boxed")
  }

  private object Sigs {
    private val SL: SourcePosition = SourcePosition.Unknown
    private val RootNS: Name.NName = Name.NName(SL, Nil, SL)

    private def mkIdent(s: String): Name.Ident = Name.Ident(SL, s, SL)

    lazy val Box: Symbol.SigSym = Symbol.mkSigSym(Symbol.mkClassSym(RootNS, mkIdent("Boxable")), mkIdent("box"))
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

    lazy val Denotation: Type = Type.mkEnum(Enums.Denotation, Boxed :: Nil)
    lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil)
    lazy val SourceLocation: Type = Type.mkEnum(Enums.SourceLocation, Nil)

    lazy val Comparison: Type = Type.mkEnum(Enums.Comparison, Nil)
    lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil)


    //
    // Function Types.
    //
    lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog)
    lazy val MergeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog)
    lazy val FilterType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog)

//    lazy val NewChannelType: Type = Type.mkImpureArrow()
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

    case Expression.FixpointMerge(exp1, exp2, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Merge)
      val defExp = Expression.Def(defn.sym, Types.MergeType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Types.Datalog
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointSolve(exp, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Solve)
      val defExp = Expression.Def(defn.sym, Types.SolveType, loc)
      val argExps = visitExp(exp) :: Nil
      val resultType = Types.Datalog
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
      val defn = Defs.lookup(Defs.Filter)
      val defExp = Expression.Def(defn.sym, Types.FilterType, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      Expression.Apply(defExp, argExps, resultType, eff, loc)

    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
      // Compute the arity of the functor F[(a, b, c)] or F[a].
      val arity = exp.tpe match {
        case Type.Apply(_, innerType) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(l)) => l
          case _ => 1
        }
        case _ => throw InternalCompilerException(s"Unexpected non-foldable type: '${exp.tpe}'.")
      }

      // Compute the symbol of the function.
      val sym = Symbol.mkDefnSym(s"Fixpoint.projectInto$arity")

      // The type of the function.
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, exp.tpe), Types.Datalog)

      // Put everything together.
      val defExp = Expression.Def(sym, defTpe, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      Expression.Apply(defExp, argExps, Types.Datalog, eff, loc)

    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
      // Compute the arity of the predicate symbol.
      // The type is either of the form `Array[(a, b, c)]` or `Array[a]`.
      val arity = tpe match {
        case Type.Apply(Type.Cst(TypeConstructor.Array, _), innerType) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(_)) => innerType.typeArguments.length
          case Some(TypeConstructor.Unit) => 0
          case _ => 1
        }
        case _ => throw InternalCompilerException(s"Unexpected non-array type: '$tpe'.")
      }

      // Compute the symbol of the function.
      val sym = Symbol.mkDefnSym(s"Fixpoint.facts$arity")

      // The type of the function.
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), tpe)

      // Put everything together.
      val defExp = Expression.Def(sym, defTpe, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      Expression.Apply(defExp, argExps, tpe, eff, loc)
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
    case Constraint(cparams, head, body, loc) =>
      val headExp = visitHeadPred(cparams, head)
      val bodyExp = mkArray(body.map(visitBodyPred(cparams, _)), Types.BodyPredicate, loc)
      val innerExp = mkTuple(headExp :: bodyExp :: Nil, loc)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(cparams0: List[ConstraintParam], p0: Predicate.Head)(implicit root: Root, flix: Flix): Expression = p0 match {
    case Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val termsExp = mkArray(terms.map(visitHeadTerm(cparams0, _)), Types.HeadTerm, loc)
      val innerExp = mkTuple(predSymExp :: denotationExp :: termsExp :: Nil, loc)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(cparams0: List[ConstraintParam], p0: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p0 match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val termsExp = mkArray(terms.map(visitBodyTerm(cparams0, _)), Types.BodyTerm, loc)
      val innerExp = mkTuple(predSymExp :: denotationExp :: polarityExp :: termsExp :: Nil, loc)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case Body.Guard(exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val quantifiedFreeVars = quantifiedVars(cparams0, exp0)
      mkGuard(quantifiedFreeVars, exp0, loc)
  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def visitHeadTerm(cparams0: List[ConstraintParam], exp0: Expression)(implicit root: Root, flix: Flix): Expression = {
    //
    // We need to consider four cases:
    //
    // Case 1.1: The expression is quantified variable. We translate it to a Var.
    // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case Expression.Var(sym, _, loc) =>
        // Case 1: Variable term.
        if (isQuantifiedVar(sym, cparams0)) {
          // Case 1.1: Quantified variable.
          mkHeadTermVar(sym)
        } else {
          // Case 1.2: Lexically bound variable.
          mkHeadTermLit(box(exp0))
        }

      case _ =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val quantifiedFreeVars = quantifiedVars(cparams0, exp0)

        if (quantifiedFreeVars.isEmpty) {
          // Case 2: No quantified variables. The expression can be reduced to a value.
          mkHeadTermLit(box(exp0))
        } else {
          // Case 3: Quantified variables. The expression is translated to an application term.
          mkAppTerm(quantifiedFreeVars, exp0, exp0.loc)
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def visitBodyTerm(cparams0: List[ConstraintParam], pat0: Pattern)(implicit root: Root, flix: Flix): Expression = pat0 match {
    case Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case Pattern.Var(sym, tpe, loc) =>
      if (isQuantifiedVar(sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(sym)
      } else {
        // Case 2: Lexically bound variable *expression*.
        mkBodyTermLit(box(Expression.Var(sym, tpe, loc)))
      }

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
    val innerExp = mkVarSym(sym)
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: Expression)(implicit root: Root, flix: Flix): Expression = {
    mkTag(Enums.HeadTerm, "Lit", exp, Types.HeadTerm, exp.loc)
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
    val innerExp = mkVarSym(sym)
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: Expression)(implicit root: Root, flix: Flix): Expression = {
    mkTag(Enums.BodyTerm, "Lit", exp, Types.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = d match {
    case Relational =>
      val innerExp = Expression.Unit(loc)
      mkTag(Enums.Denotation, "Relational", innerExp, Types.Denotation, loc)

    case Latticenal =>
      tpeOpt match {
        case None => throw InternalCompilerException("Unexpected nullary lattice predicate.")
        case Some(tpe) =>
          // The type `Denotation[tpe]`.
          val unboxedDenotationType = Type.mkEnum(Enums.Denotation, tpe :: Nil)

          // The type `Denotation[Boxed]`.
          val boxedDenotationType = Types.Denotation

          val Lattice: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Ast.lattice")
          val LatticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType)

          val Box: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Ast.box")
          val BoxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType)

          val innerApply = Expression.Apply(Expression.Def(Lattice, LatticeType, loc), List(Expression.Unit(loc)), unboxedDenotationType, Type.Pure, loc)
          Expression.Apply(Expression.Def(Box, BoxType, loc), List(innerApply), boxedDenotationType, Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation): Expression = p match {
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
      mkTag(Enums.PredSym, "PredSym", nameExp, Types.PredSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): Expression = {
    val nameExp = Expression.Str(sym.text, sym.loc)
    mkTag(Enums.VarSym, "VarSym", nameExp, Types.VarSym, sym.loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: Expression)(implicit root: Root, flix: Flix): Expression = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed)
    val innerExp = Expression.Sig(Sigs.Box, tpe, loc)
    Expression.Apply(innerExp, List(exp), Types.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a `Fixpoint/Ast.BodyPredicate.GuardX`.
    *
    * mkGuard and mkAppTerm are similar and should probably be maintained together.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: Expression, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.")
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      // Construct a lambda that takes the unit argument.
      val fparam = FormalParam(Symbol.freshVarSym("_unit", loc), Ast.Modifiers.Empty, Type.Unit, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe)
      val lambdaExp = Expression.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.BodyPredicate, s"Guard0", lambdaExp, Types.BodyPredicate, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, tpe)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = FormalParam(freshSym, Ast.Modifiers.Empty, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe)
        Expression.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXb(lambdaExp, fvs.map(_._2))

    // Construct the `Fixpoint.Ast/BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps, loc)
    mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast.Term.AppX`.
    *
    * Note: mkGuard and mkAppTerm are similar and should probably be maintained together.
    */
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: Expression, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.")
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      // Construct a lambda that takes the unit argument.
      val fparam = FormalParam(Symbol.freshVarSym("_unit", loc), Ast.Modifiers.Empty, Type.Unit, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe)
      val lambdaExp = Expression.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.HeadTerm, s"App0", lambdaExp, Types.HeadTerm, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, tpe)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = FormalParam(freshSym, Ast.Modifiers.Empty, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe)
        Expression.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs.map(_._2), exp.tpe)

    // Construct the `Fixpoint.Ast/BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps, loc)
    mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: Expression, argTypes: List[Type], resultType: Type): Expression = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Boxable.lift${argTypes.length}")

    //
    // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Boxed`.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> `resultType`.
    val argType = Type.mkPureCurriedArrow(argTypes, resultType)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Boxed.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Types.Boxed)

    // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> Boxed).
    val liftType = Type.mkPureArrow(argType, returnType)

    // Construct a call to the liftX function.
    val defn = Expression.Def(sym, liftType, exp0.loc)
    Expression.Apply(defn, List(exp0), returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftXb(exp0: Expression, argTypes: List[Type]): Expression = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Boxable.lift${argTypes.length}b")

    //
    // The liftX family of functions are of the form: a -> b -> c -> Bool and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Bool.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> Bool.
    val argType = Type.mkPureCurriedArrow(argTypes, Type.Bool)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Bool.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Type.Bool)

    // The type of the overall liftXb function, i.e. (a -> b -> c -> Bool) -> (Boxed -> Boxed -> Boxed -> Bool).
    val liftType = Type.mkPureArrow(argType, returnType)

    // Construct a call to the liftXb function.
    val defn = Expression.Def(sym, liftType, exp0.loc)
    Expression.Apply(defn, List(exp0), returnType, Type.Pure, exp0.loc)
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

  /**
    * Return a list of quantified variables in the given expression `exp0`.
    *
    * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
    * but not in the constraint params `cparams0` of the constraint.
    */
  private def quantifiedVars(cparams0: List[ConstraintParam], exp0: Expression): List[(Symbol.VarSym, Type)] = {
    TypedAstOps.freeVars(exp0).toList.filter {
      case (sym, _) => isQuantifiedVar(sym, cparams0)
    }
  }

  /**
    * Returns `true` if the given variable symbol `sym` is a quantified variable according to the given constraint params `cparams0`.
    *
    * That is, the variable symbol is *NOT* lexically bound.
    */
  private def isQuantifiedVar(sym: Symbol.VarSym, cparams0: List[ConstraintParam]): Boolean =
    cparams0.exists(p => p.sym == sym)


  // TODO: Move into TypedAstOps

  /**
    * Applies the given substitution `subst` to the given expression `exp0`.
    */
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

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.Let(s, e1, e2, tpe, eff, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.Stm(e1, e2, tpe, eff, loc)

    case Expression.Match(exp, rules, tpe, eff, loc) => ??? // TODO

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      val rs = rules map {
        case ChoiceRule(pat, exp) =>
          // TODO: Substitute in patterns?
          ChoiceRule(pat, substExp(exp, subst))
      }
      Expression.Choose(es, rs, tpe, eff, loc)

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Tag(sym, tag, e, tpe, eff, loc)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      val es = elms.map(substExp(_, subst))
      Expression.Tuple(es, tpe, eff, loc)

    case Expression.RecordEmpty(_, _) => exp0

    case Expression.RecordSelect(exp, field, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.RecordSelect(e, field, tpe, eff, loc)

    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      val v = substExp(value, subst)
      val r = substExp(rest, subst)
      Expression.RecordExtend(field, v, r, tpe, eff, loc)

    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      val r = substExp(rest, subst)
      Expression.RecordRestrict(field, r, tpe, eff, loc)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      val es = elms.map(substExp(_, subst))
      Expression.ArrayLit(es, tpe, eff, loc)

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      val e = substExp(elm, subst)
      val l = substExp(len, subst)
      Expression.ArrayNew(e, l, tpe, eff, loc)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      val b = substExp(base, subst)
      val i = substExp(index, subst)
      Expression.ArrayLoad(b, i, tpe, eff, loc)

    case Expression.ArrayLength(base, eff, loc) =>
      val b = substExp(base, subst)
      Expression.ArrayLength(b, eff, loc)

    case Expression.ArrayStore(base, index, elm, loc) =>
      val b = substExp(base, subst)
      val i = substExp(index, subst)
      Expression.ArrayStore(b, i, elm, loc)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = substExp(base, subst)
      val bi = substExp(beginIndex, subst)
      val ei = substExp(endIndex, subst)
      Expression.ArraySlice(b, bi, ei, tpe, loc)

    case Expression.Ref(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Ref(e, tpe, eff, loc)

    case Expression.Deref(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Deref(e, tpe, eff, loc)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.Assign(e1, e2, tpe, eff, loc)

    case Expression.Existential(fparam, exp, loc) =>
      val f = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      Expression.Existential(f, e, loc)

    case Expression.Universal(fparam, exp, loc) =>
      val f = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      Expression.Universal(f, e, loc)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Ascribe(e, tpe, eff, loc)

    case Expression.Cast(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Cast(e, tpe, eff, loc)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // TODO

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      val as = args.map(substExp(_, subst))
      Expression.InvokeConstructor(constructor, as, tpe, eff, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val as = args.map(substExp(_, subst))
      Expression.InvokeMethod(method, e, as, tpe, eff, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      val as = args.map(substExp(_, subst))
      Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.GetField(field, e, tpe, eff, loc)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.PutField(field, e1, e2, tpe, eff, loc)

    case Expression.GetStaticField(field, tpe, eff, loc) => exp0

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.PutStaticField(field, e, tpe, eff, loc)

    case Expression.NewChannel(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.NewChannel(e, tpe, eff, loc)

    case Expression.GetChannel(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.GetChannel(e, tpe, eff, loc)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.PutChannel(e1, e2, tpe, eff, loc)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // TODO

    case Expression.Spawn(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Spawn(e, tpe, eff, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = substExp(exp, subst)
      Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.Force(e, tpe, eff, loc)

    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      Expression.FixpointMerge(e1, e2, stf, tpe, eff, loc)

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.FixpointSolve(e, stf, tpe, eff, loc)

    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.FixpointFilter(pred, e, tpe, eff, loc)

    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.FixpointProjectIn(e, pred, tpe, eff, loc)

    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      Expression.FixpointProjectOut(pred, e, tpe, eff, loc)

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => throw InternalCompilerException(s"Unexpected expression near ${loc.format}.")
  }

  /**
    * Applies the given substitution `subst` to the given formal param `fparam0`.
    */
  private def substFormalParam(fparam0: FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fparam0 match {
    case FormalParam(sym, mod, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      FormalParam(s, mod, tpe, loc)
  }

}
