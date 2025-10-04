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
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.ops.LoweredAstOps
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.{AtomicOp, Kind, LoweredAst, LoweredAst2, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugLoweredAst2
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.annotation.unused

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint.Ast.Datalog` values (which are ordinary Flix values).
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

object Lowering2 {

  /**
    * Returns the definition associated with the given symbol `sym`.
    */
  private def lookup(sym: Symbol.DefnSym)(implicit root: LoweredAst.Root): LoweredAst.Def = root.defs.get(sym) match {
    case None => throw InternalCompilerException(s"Symbol '$sym' not found. Missing library?", sym.loc)
    case Some(d) => d
  }

  private object Enums {
    lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Datalog")
    lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Constraint")

    lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.HeadPredicate")
    lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.BodyPredicate")

    lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.HeadTerm")
    lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.BodyTerm")

    lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Shared.PredSym")
    lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.VarSym")

    lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Shared.Denotation")
    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Polarity")
    lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Fixity")

    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Boxed")

    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")
  }

  private object Types {
    //
    // Data Types
    //
    lazy val Datalog: Type = Type.mkEnum(Enums.Datalog, Nil, SourceLocation.Unknown)
    lazy val Constraint: Type = Type.mkEnum(Enums.Constraint, Nil, SourceLocation.Unknown)

    lazy val HeadPredicate: Type = Type.mkEnum(Enums.HeadPredicate, Nil, SourceLocation.Unknown)
    lazy val BodyPredicate: Type = Type.mkEnum(Enums.BodyPredicate, Nil, SourceLocation.Unknown)

    lazy val HeadTerm: Type = Type.mkEnum(Enums.HeadTerm, Nil, SourceLocation.Unknown)
    lazy val BodyTerm: Type = Type.mkEnum(Enums.BodyTerm, Nil, SourceLocation.Unknown)

    lazy val PredSym: Type = Type.mkEnum(Enums.PredSym, Nil, SourceLocation.Unknown)
    lazy val VarSym: Type = Type.mkEnum(Enums.VarSym, Nil, SourceLocation.Unknown)

    lazy val Denotation: Type = Type.mkEnum(Enums.Denotation, Boxed :: Nil, SourceLocation.Unknown)
    lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil, SourceLocation.Unknown)
    lazy val Fixity: Type = Type.mkEnum(Enums.Fixity, Nil, SourceLocation.Unknown)

    lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil, SourceLocation.Unknown)

    lazy val VectorOfBoxed: Type = Type.mkVector(Types.Boxed, SourceLocation.Unknown)

    def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)

    //
    // Function Types.
    //
    lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog, SourceLocation.Unknown)
    lazy val MergeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog, SourceLocation.Unknown)
    lazy val FilterType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog, SourceLocation.Unknown)
    lazy val RenameType: Type = Type.mkPureUncurriedArrow(List(mkList(PredSym, SourceLocation.Unknown), Datalog), Datalog, SourceLocation.Unknown)

    def mkProvenanceOf(t: Type, loc: SourceLocation): Type =
      Type.mkPureUncurriedArrow(
        List(
          PredSym,
          Type.mkVector(Boxed, loc),
          Type.mkVector(PredSym, loc),
          Type.mkPureCurriedArrow(List(PredSym, Type.mkVector(Boxed, loc)), t, loc),
          Datalog
        ),
        Type.mkVector(t, loc), loc
      )

  }

  /**
    * Translates internal Datalog constraints into Flix Datalog constraints.
    */
  @unused
  def run(root: LoweredAst.Root)(implicit flix: Flix): LoweredAst2.Root = flix.phase("Lowering") {
    implicit val r: LoweredAst.Root = root

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val sigs = ParOps.parMapValues(root.sigs)(visitSig)
    val instances = ParOps.parMapValueList(root.instances)(visitInstance)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val aliases = ParOps.parMapValues(root.typeAliases)(visitTypeAlias)
    val traits = ParOps.parMapValues(root.traits)(t => visitTrait(t, sigs))

    LoweredAst2.Root(traits, instances, sigs, defs, enums, structs, effects, aliases, root.mainEntryPoint, root.entryPoints, root.sources, root.traitEnv, root.eqEnv)
  }

  /**
    * Lowers the given type `alias`.
    */
  private def visitTypeAlias(alias: LoweredAst.TypeAlias)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.TypeAlias = alias match {
    case LoweredAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      LoweredAst2.TypeAlias(doc, mod, sym, tparams, tpe, loc)
  }

  /**
    * Lowers the given `effect`.
    */
  private def visitEffect(effect: LoweredAst.Effect)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Effect = effect match {
    case LoweredAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      // TODO EFFECT-TPARAMS use tparams
      val ops = ops0.map(visitOp)
      LoweredAst2.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Lowers the given `op`.
    */
  private def visitOp(op: LoweredAst.Op)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Op = op match {
    case LoweredAst.Op(sym, spec0, loc) =>
      val spec = visitSpec(spec0)
      LoweredAst2.Op(sym, spec, loc)
  }

  /**
    * Lowers the given struct `struct0`.
    */
  private def visitStruct(struct0: LoweredAst.Struct)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Struct = struct0 match {
    case LoweredAst.Struct(doc, ann, mod, sym, tparams0, fields0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val fields = fields0.map {
        case LoweredAst.StructField(fieldSym, tpe, loc2) => LoweredAst2.StructField(fieldSym, visitType(tpe), loc2)
      }
      LoweredAst2.Struct(doc, ann, mod, sym, tparams, fields, loc)
  }


  /**
    * Lowers the given enum `enum0`.
    */
  private def visitEnum(enum0: LoweredAst.Enum)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Enum = enum0 match {
    case LoweredAst.Enum(doc, ann, mod, sym, tparams0, derives, cases0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val cases = cases0.map {
        case (_, LoweredAst.Case(caseSym, tpes0, caseSc0, caseLoc)) =>
          val tpes = tpes0.map(visitType)
          val caseSc = visitScheme(caseSc0)
          (caseSym, LoweredAst2.Case(caseSym, tpes, caseSc, caseLoc))
      }
      LoweredAst2.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
  }

  /**
    * Lowers the given instance `inst0`.
    */
  private def visitInstance(inst0: LoweredAst.Instance)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Instance = inst0 match {
    case LoweredAst.Instance(doc, ann, mod, sym, tparams0, tpe0, tconstrs0, econstrs0, assocs0, defs0, ns, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTraitConstraint)
      val econstrs = econstrs0.map(visitEqConstraint)
      val assocs = assocs0.map {
        case LoweredAst.AssocTypeDef(defDoc, defMod, defSymUse, args, defTpe, defLoc) => LoweredAst2.AssocTypeDef(defDoc, defMod, defSymUse, args, defTpe, defLoc)
      }
      val defs = defs0.map(visitDef)
      LoweredAst2.Instance(doc, ann, mod, sym, tparams, tpe, tconstrs, econstrs, assocs, defs, ns, loc)
  }

  /**
    * Lowers the given equality constraint `econstr0`.
    */
  private def visitEqConstraint(econstr0: EqualityConstraint)(implicit root: LoweredAst.Root, flix: Flix): EqualityConstraint = econstr0 match {
    case EqualityConstraint(symUse, t1, t2, loc) =>
      val tpe1 = visitType(t1)
      val tpe2 = visitType(t2)
      EqualityConstraint(symUse, tpe1, tpe2, loc)
  }

  /**
    * Lowers the given signature `sig0`.
    */
  private def visitSig(sig0: LoweredAst.Sig)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Sig = sig0 match {
    case LoweredAst.Sig(sym, spec0, exp0, loc) =>
      val spec = visitSpec(spec0)
      val impl = exp0.map(visitExp(_)(Scope.Top, root, flix))
      LoweredAst2.Sig(sym, spec, impl, loc)
  }

  /**
    * Lowers the given definition `defn0`.
    */
  private def visitDef(defn0: LoweredAst.Def)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Def = defn0 match {
    case LoweredAst.Def(sym, spec0, exp0, loc) =>
      val spec = visitSpec(spec0)
      val exp = visitExp(exp0)(Scope.Top, root, flix)
      LoweredAst2.Def(sym, spec, exp, loc)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def visitSpec(spec0: LoweredAst.Spec)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Spec = spec0 match {
    case LoweredAst.Spec(doc, ann, mod, tparams0, fparams, declaredScheme, retTpe, eff, tconstrs) =>
      val tparam = tparams0.map(visitTypeParam)
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      LoweredAst2.Spec(doc, ann, mod, tparam, fs, ds, retTpe, eff, tconstrs)
  }

  /**
    * Lowers the given trait `trt0`, with the given lowered sigs `sigs`.
    */
  private def visitTrait(trt0: LoweredAst.Trait, sigs: Map[Symbol.SigSym, LoweredAst2.Sig])(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Trait = trt0 match {
    case LoweredAst.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, signatures0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superTraits = superTraits0.map(visitTraitConstraint)
      val assocs = assocs0.map {
        case LoweredAst.AssocTypeSig(sigDoc, sigMod, sigSym, sigTparam, kind, sigLoc) => LoweredAst2.AssocTypeSig(sigDoc, sigMod, sigSym, sigTparam, kind, sigLoc)
      }
      val signatures = signatures0.map(sig => sigs(sig.sym))
      val laws = laws0.map(visitDef)
      LoweredAst2.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, signatures, laws, loc)
  }

  /**
    * Lowers the given type constraint `tconstr0`.
    */
  private def visitTraitConstraint(tconstr0: TraitConstraint)(implicit root: LoweredAst.Root, flix: Flix): TraitConstraint = tconstr0 match {
    case TraitConstraint(head, tpe0, loc) =>
      val tpe = visitType(tpe0)
      TraitConstraint(head, tpe, loc)
  }

  /**
    * Lowers the given `tparam`.
    */
  private def visitTypeParam(tparam: LoweredAst.TypeParam): LoweredAst2.TypeParam = tparam match {
    case LoweredAst.TypeParam(name, sym, loc) => LoweredAst2.TypeParam(name, sym, loc)
  }

  /**
    * Lowers the given expression `exp0`.
    */
  private def visitExp(exp0: LoweredAst.Expr)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = exp0 match {
    case LoweredAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst2.Expr.Cst(cst, t, loc)

    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst2.Expr.Var(sym, t, loc)

    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst2.Expr.Lambda(p, e, t, loc)

    case LoweredAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst2.Expr.ApplyClo(e1, e2, t, eff, loc)

    case LoweredAst.Expr.ApplyDef(sym, exps, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val tas = targs.map(visitType)
      val it = visitType(itpe)
      val t = visitType(tpe)
      LoweredAst2.Expr.ApplyDef(sym, es, tas, it, t, eff, loc)

    case LoweredAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst2.Expr.ApplyLocalDef(sym, es, t, eff, loc)

    case LoweredAst.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      LoweredAst2.Expr.ApplyOp(sym, es, tpe, eff, loc)

    case LoweredAst.Expr.ApplySig(sym, exps, targ, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val ta = visitType(targ)
      val tas = targs.map(visitType)
      val it = visitType(itpe)
      val t = visitType(tpe)
      LoweredAst2.Expr.ApplySig(sym, es, ta, tas, it, t, eff, loc)

    case LoweredAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst2.Expr.Let(sym, e1, e2, t, eff, loc)

    case LoweredAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val fps = fparams.map(visitFormalParam)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst2.Expr.LocalDef(sym, fps, e1, e2, t, eff, loc)

    case LoweredAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst2.Expr.Scope(sym, regionVar, e, t, eff, loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst2.Expr.IfThenElse(e1, e2, e3, t, eff, loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst2.Expr.Stm(e1, e2, t, eff, loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst2.Expr.Discard(e, eff, loc)

    case LoweredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe)
      LoweredAst2.Expr.Match(e, rs, t, eff, loc)

    case LoweredAst.Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitTypeMatchRule)
      val t = visitType(tpe)
      LoweredAst2.Expr.TypeMatch(e, rs, t, eff, loc)

    case LoweredAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitExtMatchRule)
      val t = visitType(tpe)
      LoweredAst2.Expr.ExtMatch(e, rs, t, eff, loc)

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst2.Expr.VectorLit(es, t, eff, loc)

    case LoweredAst.Expr.VectorLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      LoweredAst2.Expr.VectorLoad(b, i, t, eff, loc)

    case LoweredAst.Expr.VectorLength(base, loc) =>
      val b = visitExp(base)
      LoweredAst2.Expr.VectorLength(b, loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe)
      LoweredAst2.Expr.TryCatch(e, rs, t, eff, loc)

    case LoweredAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      LoweredAst2.Expr.RunWith(visitExp(exp), effUse, rules.map(visitHandlerRule), tpe, eff, loc)

    case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod)
      LoweredAst2.Expr.NewObject(name, clazz, t, eff, ms, loc)

    case LoweredAst.Expr.FixpointConstraintSet(cs, _, loc) =>
      mkDatalog(cs, loc)

    case LoweredAst.Expr.FixpointLambda(pparams, exp, _, eff, loc) =>
      val defn = lookup(DatalogDefs.Rename)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.PredSym, loc)
      val argExps = predExps :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst2.Expr.ApplyDef(defn.sym, argExps, List.empty, Types.RenameType, resultType, eff, loc)

    case LoweredAst.Expr.FixpointMerge(exp1, exp2, _, eff, loc) =>
      val defn = lookup(DatalogDefs.Merge)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Types.Datalog
      LoweredAst2.Expr.ApplyDef(defn.sym, argExps, List.empty, Types.MergeType, resultType, eff, loc)

    case LoweredAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tpe, eff, loc) =>
      // Create appropriate call to Fixpoint.Solver.provenanceOf. This requires creating a mapping, mkExtVar, from
      // PredSym and terms to an extensible variant.
      val defn = lookup(DatalogDefs.ProvenanceOf)
      val mergedExp = mergeExps(exps.map(visitExp), loc)
      val (goalPredSym, goalTerms) = select match {
        case LoweredAst.Predicate.Head.Atom(pred, _, terms, _, loc1) =>
          val boxedTerms = terms.map(t => box(visitExp(t)))
          (mkPredSym(pred), mkVector(boxedTerms, Types.Boxed, loc1))
      }
      val withPredSyms = mkVector(withh.map(mkPredSym), Types.PredSym, loc)
      val extVarType = unwrapVectorType(tpe, loc)
      val preds = predicatesOfExtVar(extVarType, loc)
      val lambdaExp = visitExp(mkExtVarLambda(preds, extVarType, loc))
      val argExps = goalPredSym :: goalTerms :: withPredSyms :: lambdaExp :: mergedExp :: Nil
      val itpe = Types.mkProvenanceOf(extVarType, loc)
      LoweredAst2.Expr.ApplyDef(defn.sym, argExps, List.empty, itpe, tpe, eff, loc)

    case LoweredAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, _, _, pred, tpe, eff, loc) =>
      val loweredExps = exps.map(visitExp)
      val loweredQueryExp = visitExp(queryExp)

      // Compute the arity of the predicate symbol.
      // The type is either of the form `Vector[(a, b, c)]` or `Vector[a]`.
      val (_, targs) = Type.eraseAliases(tpe) match {
        case Type.Apply(tycon, innerType, _) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(_)) => (tycon, innerType.typeArguments)
          case Some(TypeConstructor.Unit) => (tycon, Nil)
          case _ => (innerType, List(innerType))
        }
        case t => throw InternalCompilerException(s"Unexpected non-foldable type: '${t}'.", loc)
      }

      val predArity = selects.length

      // Define the name and type of the appropriate factsX function in Solver.flix
      val sym = DatalogDefs.Facts(predArity)
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), tpe, loc)

      // Merge and solve exps
      val mergedExp = mergeExps(loweredQueryExp :: loweredExps, loc)
      val solvedExp = LoweredAst2.Expr.ApplyDef(DatalogDefs.Solve, mergedExp :: Nil, List.empty, Types.SolveType, Types.Datalog, eff, loc)

      // Put everything together
      val argExps = mkPredSym(pred) :: solvedExp :: Nil
      LoweredAst2.Expr.ApplyDef(sym, argExps, targs, defTpe, tpe, eff, loc)

    case LoweredAst.Expr.FixpointSolveWithProject(exps0, optPreds, mode, _, eff, loc) =>
      // Rewrites
      //     solve e₁, e₂, e₃ project P₁, P₂, P₃
      // to
      //     let tmp% = solve e₁ <+> e₂ <+> e₃;
      //     merge (project P₁ tmp%, project P₂ tmp%, project P₃ tmp%)
      //
      val defn = mode match {
        case SolveMode.Default => lookup(DatalogDefs.Solve)
        case SolveMode.WithProvenance => lookup(DatalogDefs.SolveWithProvenance)
      }
      val exps = exps0.map(visitExp)
      val mergedExp = mergeExps(exps, loc)
      val argExps = mergedExp :: Nil
      val solvedExp = LoweredAst2.Expr.ApplyDef(defn.sym, argExps, List.empty, Types.SolveType, Types.Datalog, eff, loc)
      val tmpVarSym = Symbol.freshVarSym("tmp%", BoundBy.Let, loc)
      val letBodyExp = optPreds match {
        case Some(preds) =>
          mergeExps(preds.map(pred => {
            val varExp = LoweredAst2.Expr.Var(tmpVarSym, Types.Datalog, loc)
            projectSym(mkPredSym(pred), varExp, loc)
          }), loc)
        case None => LoweredAst2.Expr.Var(tmpVarSym, Types.Datalog, loc)
      }
      LoweredAst2.Expr.Let(tmpVarSym, solvedExp, letBodyExp, Types.Datalog, eff, loc)

    case LoweredAst.Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
      val loweredExps = exps.zip(predsAndArities).map {
        case (exp, PredicateAndArity(pred, _)) =>
          // Compute the types arguments of the functor F[(a, b, c)] or F[a].
          val (targ, targs) = Type.eraseAliases(exp.tpe) match {
            case Type.Apply(tycon, innerType, _) => innerType.typeConstructor match {
              case Some(TypeConstructor.Tuple(_)) => (tycon, innerType.typeArguments)
              case Some(TypeConstructor.Unit) => (tycon, Nil)
              case _ => (tycon, List(innerType))
            }
            case _ => throw InternalCompilerException(s"Unexpected non-foldable type: '${exp.tpe}'.", loc)
          }

          // Compute the symbol of the function.
          val sym = DatalogDefs.ProjectInto(targs.length)

          // The type of the function.
          val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, exp.tpe), Types.Datalog, loc)

          // Put everything together.
          val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
          LoweredAst2.Expr.ApplyDef(sym, argExps, targ :: targs, defTpe, Types.Datalog, exp.eff, loc)
      }
      mergeExps(loweredExps, loc)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      LoweredAst2.Expr.ApplyAtomic(op, exps.map(visitExp), visitType(tpe), visitType(eff), loc)

    case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      LoweredAst2.Expr.Ascribe(visitExp(exp), visitType(tpe), visitType(eff), loc)

    case LoweredAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      LoweredAst2.Expr.Cast(visitExp(exp), declaredType.map(visitType), declaredEff.map(visitType), tpe, eff, loc)
  }

  /**
    * Lowers the given pattern `pat0`.
    */
  private def visitPat(pat0: LoweredAst.Pattern)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Pattern = pat0 match {
    case LoweredAst.Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst2.Pattern.Wild(t, loc)

    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst2.Pattern.Var(sym, t, loc)

    case LoweredAst.Pattern.Cst(cst, tpe, loc) =>
      LoweredAst2.Pattern.Cst(cst, tpe, loc)

    case LoweredAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(visitPat)
      val t = visitType(tpe)
      LoweredAst2.Pattern.Tag(symUse, ps, t, loc)

    case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      LoweredAst2.Pattern.Tuple(es, t, loc)

    case LoweredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val patsVal = pats.map {
        case LoweredAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val p1 = visitPat(pat1)
          val t1 = visitType(tpe1)
          LoweredAst2.Pattern.Record.RecordLabelPattern(label, p1, t1, loc1)
      }
      val patVal = visitPat(pat)
      val t = visitType(tpe)
      LoweredAst2.Pattern.Record(patsVal, patVal, t, loc)
  }

  /**
    * Lowers the given scheme `sc0`.
    */
  private def visitScheme(sc0: Scheme)(implicit root: LoweredAst.Root, flix: Flix): Scheme = sc0 match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      // TODO: What about constraints?
      val b = visitType(base)
      Scheme(quantifiers, tconstrs, econstrs, b)
  }

  /**
    * Lowers the given type `tpe0`.
    */
  private def visitType(tpe0: Type)(implicit root: LoweredAst.Root, flix: Flix): Type = tpe0.typeConstructor match {
    case Some(TypeConstructor.Schema) =>
      // We replace any Schema type, no matter the number of polymorphic type applications, with the erased Datalog type.
      Types.Datalog
    case _ => visitTypeNonSchema(tpe0)
  }

  /**
    * Lowers the given type `tpe0` which must not be a schema type.
    *
    * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
    */
  private def visitTypeNonSchema(tpe0: Type)(implicit root: LoweredAst.Root, flix: Flix): Type = tpe0 match {
    case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

    case Type.Var(_, _) => tpe0

    case Type.Apply(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      // Performance: Reuse tpe0, if possible.
      if ((t1 eq tpe1) && (t2 eq tpe2)) {
        tpe0
      } else {
        Type.Apply(t1, t2, loc)
      }

    case Type.Alias(sym, args, t, loc) =>
      Type.Alias(sym, args.map(visitType), visitType(t), loc)

    case Type.AssocType(cst, args, kind, loc) =>
      Type.AssocType(cst, args.map(visitType), kind, loc) // TODO ASSOC-TYPES can't put lowered stuff on right side of assoc type def...

    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)

    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
  }


  /**
    * Lowers the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: LoweredAst.FormalParam)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.FormalParam = fparam0 match {
    case LoweredAst.FormalParam(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst2.FormalParam(sym, t, loc)
  }

  private def visitExtMatchRule(rule0: LoweredAst.ExtMatchRule)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.ExtMatchRule = rule0 match {
    case LoweredAst.ExtMatchRule(pat, exp, loc) =>
      val p = visitExtPat(pat)
      val e = visitExp(exp)
      LoweredAst2.ExtMatchRule(p, e, loc)
  }

  private def visitExtPat(pat0: LoweredAst.ExtPattern): LoweredAst2.ExtPattern = pat0 match {
    case LoweredAst.ExtPattern.Default(loc) =>
      LoweredAst2.ExtPattern.Default(loc)

    case LoweredAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(visitExtTagPat)
      LoweredAst2.ExtPattern.Tag(label, ps, loc)
  }

  private def visitExtTagPat(pat0: LoweredAst.ExtTagPattern): LoweredAst2.ExtTagPattern = pat0 match {
    case LoweredAst.ExtTagPattern.Wild(tpe, loc) => LoweredAst2.ExtTagPattern.Wild(tpe, loc)
    case LoweredAst.ExtTagPattern.Var(sym, tpe, loc) => LoweredAst2.ExtTagPattern.Var(sym, tpe, loc)
    case LoweredAst.ExtTagPattern.Unit(tpe, loc) => LoweredAst2.ExtTagPattern.Unit(tpe, loc)
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def visitCatchRule(rule0: LoweredAst.CatchRule)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.CatchRule = rule0 match {
    case LoweredAst.CatchRule(sym, clazz, exp) =>
      val e = visitExp(exp)
      LoweredAst2.CatchRule(sym, clazz, e)
  }

  /**
    * Lowers the given handler rule `rule0`.
    */
  private def visitHandlerRule(rule0: LoweredAst.HandlerRule)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.HandlerRule = rule0 match {
    case LoweredAst.HandlerRule(symUse, fparams0, exp) =>
      val fparams = fparams0.map(visitFormalParam)
      val e = visitExp(exp)
      LoweredAst2.HandlerRule(symUse, fparams, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: LoweredAst.MatchRule)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.MatchRule = rule0 match {
    case LoweredAst.MatchRule(pat, guard, exp) =>
      val p = visitPat(pat)
      val g = guard.map(visitExp)
      val e = visitExp(exp)
      LoweredAst2.MatchRule(p, g, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitTypeMatchRule(rule0: LoweredAst.TypeMatchRule)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.TypeMatchRule = rule0 match {
    case LoweredAst.TypeMatchRule(sym, tpe, exp) =>
      val e = visitExp(exp)
      LoweredAst2.TypeMatchRule(sym, tpe, e)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Datalog` value from the given list of Datalog constraints `cs`.
    */
  private def mkDatalog(cs: List[LoweredAst.Constraint], loc: SourceLocation)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = {
    val factExps = cs.filter(c => c.body.isEmpty).map(visitConstraint)
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(visitConstraint)

    val factListExp = mkVector(factExps, Types.Constraint, loc)
    val ruleListExp = mkVector(ruleExps, Types.Constraint, loc)

    val innerExp = List(factListExp, ruleListExp)
    mkTag(Enums.Datalog, "Datalog", innerExp, Types.Datalog, loc)
  }

  /**
    * Lowers the given constraint `c0`.
    */
  private def visitConstraint(c0: LoweredAst.Constraint)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = c0 match {
    case LoweredAst.Constraint(cparams, head, body, loc) =>
      val headExp = visitHeadPred(cparams, head)
      val bodyExp = mkVector(body.map(visitBodyPred(cparams, _)), Types.BodyPredicate, loc)
      val innerExp = List(headExp, bodyExp)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(cparams0: List[LoweredAst.ConstraintParam], p0: LoweredAst.Predicate.Head)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = p0 match {
    case LoweredAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val termsExp = mkVector(terms.map(visitHeadTerm(cparams0, _)), Types.HeadTerm, loc)
      val innerExp = List(predSymExp, denotationExp, termsExp)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(cparams0: List[LoweredAst.ConstraintParam], p0: LoweredAst.Predicate.Body)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = p0 match {
    case LoweredAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val fixityExp = mkFixity(fixity, loc)
      val termsExp = mkVector(terms.map(visitBodyTerm(cparams0, _)), Types.BodyTerm, loc)
      val innerExp = List(predSymExp, denotationExp, polarityExp, fixityExp, termsExp)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case LoweredAst.Predicate.Body.Functional(outVars, exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val inVars = quantifiedVars(cparams0, exp0)
      val exp = visitExp(exp0)
      mkFunctional(outVars, inVars, exp, loc)

    case LoweredAst.Predicate.Body.Guard(exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val quantifiedFreeVars = quantifiedVars(cparams0, exp0)
      val exp = visitExp(exp0)
      mkGuard(quantifiedFreeVars, exp, loc)

  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def visitHeadTerm(cparams0: List[LoweredAst.ConstraintParam], exp0: LoweredAst.Expr)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = {
    //
    // We need to consider four cases:
    //
    // Case 1.1: The expression is quantified variable. We translate it to a Var.
    // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case LoweredAst.Expr.Var(sym, _, _) =>
        // Case 1: Variable term.
        if (isQuantifiedVar(sym, cparams0)) {
          // Case 1.1: Quantified variable.
          mkHeadTermVar(sym)
        } else {
          // Case 1.2: Lexically bound variable.
          mkHeadTermLit(box(visitExp(exp0)))
        }

      case _ =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val quantifiedFreeVars = quantifiedVars(cparams0, exp0)

        if (quantifiedFreeVars.isEmpty) {
          // Case 2: No quantified variables. The expression can be reduced to a value.
          mkHeadTermLit(box(visitExp(exp0)))
        } else {
          // Case 3: Quantified variables. The expression is translated to an application term.
          mkAppTerm(quantifiedFreeVars, visitExp(exp0), exp0.loc)(flix)
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def visitBodyTerm(cparams0: List[LoweredAst.ConstraintParam], pat0: LoweredAst.Pattern): LoweredAst2.Expr = pat0 match {
    case LoweredAst.Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      if (isQuantifiedVar(sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(sym)
      } else {
        // Case 2: Lexically bound variable *expression*.
        mkBodyTermLit(box(LoweredAst2.Expr.Var(sym, tpe, loc)))
      }

    case LoweredAst.Pattern.Cst(cst, tpe, loc) =>
      mkBodyTermLit(box(LoweredAst2.Expr.Cst(cst, tpe, loc)))

    case LoweredAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case LoweredAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case LoweredAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

  }

  /**
    * Lowers the given JvmMethod `method`.
    */
  private def visitJvmMethod(method: LoweredAst.JvmMethod)(implicit scope: Scope, root: LoweredAst.Root, flix: Flix): LoweredAst2.JvmMethod = method match {
    case LoweredAst.JvmMethod(ident, fparams, exp, retTyp, eff, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      val t = visitType(retTyp)
      LoweredAst2.JvmMethod(ident, fs, e, t, eff, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym): LoweredAst2.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: LoweredAst2.Expr): LoweredAst2.Expr = {
    mkTag(Enums.HeadTerm, "Lit", List(exp), Types.HeadTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation): LoweredAst2.Expr = {
    mkTag(Enums.BodyTerm, "Wild", Nil, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym): LoweredAst2.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: LoweredAst2.Expr): LoweredAst2.Expr = {
    mkTag(Enums.BodyTerm, "Lit", List(exp), Types.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit root: LoweredAst.Root, flix: Flix): LoweredAst2.Expr = d match {
    case Denotation.Relational =>
      mkTag(Enums.Denotation, "Relational", Nil, Types.Denotation, loc)

    case Denotation.Latticenal =>
      tpeOpt match {
        case None => throw InternalCompilerException("Unexpected nullary lattice predicate.", loc)
        case Some(tpe) =>
          val innerType = visitType(tpe)
          // The type `Denotation[tpe]`.
          val unboxedDenotationType = Type.mkEnum(Enums.Denotation, innerType :: Nil, loc)

          // The type `Denotation[Boxed]`.
          val boxedDenotationType = Types.Denotation

          val latticeSym: Symbol.DefnSym = DatalogDefs.lattice
          val latticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)

          val boxSym: Symbol.DefnSym = DatalogDefs.box
          val boxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)

          val innerApply = LoweredAst2.Expr.ApplyDef(latticeSym, List(LoweredAst2.Expr.Cst(Constant.Unit, Type.Unit, loc)), List(innerType), latticeType, unboxedDenotationType, Type.Pure, loc)
          LoweredAst2.Expr.ApplyDef(boxSym, List(innerApply), List(innerType), boxType, boxedDenotationType, Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation): LoweredAst2.Expr = p match {
    case Polarity.Positive =>
      mkTag(Enums.Polarity, "Positive", Nil, Types.Polarity, loc)

    case Polarity.Negative =>
      mkTag(Enums.Polarity, "Negative", Nil, Types.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Fixity` from the given fixity `f`.
    */
  private def mkFixity(f: Fixity, loc: SourceLocation): LoweredAst2.Expr = f match {
    case Fixity.Loose =>
      mkTag(Enums.Fixity, "Loose", Nil, Types.Fixity, loc)

    case Fixity.Fixed =>
      mkTag(Enums.Fixity, "Fixed", Nil, Types.Fixity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred): LoweredAst2.Expr = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = LoweredAst2.Expr.Cst(Constant.Str(sym), Type.Str, loc)
      val idExp = LoweredAst2.Expr.Cst(Constant.Int64(0), Type.Int64, loc)
      val inner = List(nameExp, idExp)
      mkTag(Enums.PredSym, "PredSym", inner, Types.PredSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): LoweredAst2.Expr = {
    val nameExp = LoweredAst2.Expr.Cst(Constant.Str(sym.text), Type.Str, sym.loc)
    mkTag(Enums.VarSym, "VarSym", List(nameExp), Types.VarSym, sym.loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: LoweredAst2.Expr): LoweredAst2.Expr = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed, loc)
    LoweredAst2.Expr.ApplyDef(DatalogDefs.Box, List(exp), List(exp.tpe), tpe, Types.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.GuardX`.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst2.Expr, loc: SourceLocation)(implicit scope: Scope, flix: Flix): LoweredAst2.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      val sym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, loc)
      // Construct a lambda that takes the unit argument.
      val fparam = LoweredAst2.FormalParam(sym, Type.Unit, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = LoweredAst2.Expr.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.BodyPredicate, s"Guard0", List(lambdaExp), Types.BodyPredicate, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = LoweredAst2.FormalParam(freshSym, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst2.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXb(lambdaExp, fvs.map(_._2))

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.Functional`.
    */
  private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: LoweredAst2.Expr, loc: SourceLocation)(implicit flix: Flix): LoweredAst2.Expr = {
    // Compute the number of in and out variables.
    val numberOfInVars = inVars.length
    val numberOfOutVars = outVars.length

    if (numberOfInVars > 5) {
      throw InternalCompilerException("Does not support more than 5 in variables.", loc)
    }
    if (numberOfOutVars == 0) {
      throw InternalCompilerException("Requires at least one out variable.", loc)
    }
    if (numberOfOutVars > 5) {
      throw InternalCompilerException("Does not support more than 5 out variables.", loc)
    }

    // Introduce a fresh variable for each in variable.
    val freshVars = inVars.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = inVars.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = LoweredAst2.FormalParam(freshSym, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst2.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXY(outVars, lambdaExp, inVars.map(_._2), exp.tpe, exp.loc)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val boundVarVector = mkVector(outVars.map(mkVarSym), Types.VarSym, loc)
    val freeVarVector = mkVector(inVars.map(kv => mkVarSym(kv._1)), Types.VarSym, loc)
    val innerExp = List(boundVarVector, liftedExp, freeVarVector)
    mkTag(Enums.BodyPredicate, s"Functional", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.HeadTerm.AppX`.
    */
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst2.Expr, loc: SourceLocation)(implicit flix: Flix): LoweredAst2.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = LoweredAst2.FormalParam(freshSym, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst2.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs.map(_._2), exp.tpe)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: LoweredAst2.Expr, argTypes: List[Type], resultType: Type): LoweredAst2.Expr = {
    // Compute the liftXb symbol.
    val sym = DatalogDefs.liftX(argTypes.length)

    //
    // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Boxed`.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> `resultType`.
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Boxed.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Types.Boxed, exp0.loc)

    // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> Boxed).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Construct a call to the liftX function.
    LoweredAst2.Expr.ApplyDef(sym, List(exp0), argTypes :+ resultType, liftType, returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    */
  private def liftXb(exp0: LoweredAst2.Expr, argTypes: List[Type]): LoweredAst2.Expr = {
    // Compute the liftXb symbol.
    val sym = DatalogDefs.liftXb(argTypes.length)

    //
    // The liftX family of functions are of the form: a -> b -> c -> Bool and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Bool.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> Bool.
    val argType = Type.mkPureCurriedArrow(argTypes, Type.Bool, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Bool.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Type.Bool, exp0.loc)

    // The type of the overall liftXb function, i.e. (a -> b -> c -> Bool) -> (Boxed -> Boxed -> Boxed -> Bool).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Construct a call to the liftXb function.
    LoweredAst2.Expr.ApplyDef(sym, List(exp0), argTypes, liftType, returnType, Type.Pure, exp0.loc)
  }


  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
    */
  private def liftXY(outVars: List[Symbol.VarSym], exp0: LoweredAst2.Expr, argTypes: List[Type], resultType: Type, loc: SourceLocation): LoweredAst2.Expr = {
    // Compute the number of bound ("output") and free ("input") variables.
    val numberOfInVars = argTypes.length
    val numberOfOutVars = outVars.length

    // Compute the liftXY symbol.
    // For example, lift3X2 is a function from three arguments to a Vector of pairs.
    val sym = DatalogDefs.liftXY(numberOfInVars, numberOfOutVars)

    //
    // The liftXY family of functions are of the form: i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)] and
    // returns a function of the form Vector[Boxed] -> Vector[Vector[Boxed]].
    // That is, the function accepts a *curried* function and an uncurried function that takes
    // its input as a boxed Vector and return its output as a vector of vectors.
    //
    val targs = argTypes ::: extractTuplishTypes(resultType)

    // The type of the function argument, i.e. i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)].
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, loc)

    // The type of the returned function, i.e. Vector[Boxed] -> Vector[Vector[Boxed]].
    val returnType = Type.mkPureArrow(Type.mkVector(Types.Boxed, loc), Type.mkVector(Type.mkVector(Types.Boxed, loc), loc), loc)

    // The type of the overall liftXY function, i.e. (i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)]) -> (Vector[Boxed] -> Vector[Vector[Boxed]]).
    val liftType = Type.mkPureArrow(argType, returnType, loc)

    // Construct a call to the liftXY function.
    LoweredAst2.Expr.ApplyDef(sym, List(exp0), targs, liftType, returnType, Type.Pure, loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkList(exps: List[LoweredAst2.Expr], elmType: Type, loc: SourceLocation): LoweredAst2.Expr = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, loc)
    }
  }

  /**
    * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkVector(exps: List[LoweredAst2.Expr], elmType: Type, loc: SourceLocation): LoweredAst2.Expr = {
    LoweredAst2.Expr.VectorLit(exps, Type.mkVector(elmType, loc), Type.Pure, loc)
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    */
  private def mkNil(elmType: Type, loc: SourceLocation): LoweredAst2.Expr = {
    mkTag(Enums.FList, "Nil", Nil, Types.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
    */
  private def mkCons(hd: LoweredAst2.Expr, tail: LoweredAst2.Expr, loc: SourceLocation): LoweredAst2.Expr = {
    mkTag(Enums.FList, "Cons", List(hd, tail), tail.tpe, loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exps: List[LoweredAst2.Expr], tpe: Type, loc: SourceLocation): LoweredAst2.Expr = {
    val caseSym = new Symbol.CaseSym(sym, tag, loc.asSynthetic)
    LoweredAst2.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, tpe, Type.Pure, loc)
  }

  /**
    * Returns an expression merging `exps` using `Defs.Merge`.
    */
  private def mergeExps(exps: List[LoweredAst2.Expr], loc: SourceLocation)(implicit root: LoweredAst.Root): LoweredAst2.Expr =
    exps.reduceRight {
      (exp, acc) =>
        val defn = lookup(DatalogDefs.Merge)
        val argExps = exp :: acc :: Nil
        val resultType = Types.Datalog
        val itpe = Types.MergeType
        LoweredAst2.Expr.ApplyDef(defn.sym, argExps, List.empty, itpe, resultType, exp.eff, loc)
    }

  /**
    * Returns a new `Datalog` from `datalogExp` containing only facts from the predicate given by the `PredSym` `predSymExp`
    * using `Defs.Filter`.
    */
  private def projectSym(predSymExp: LoweredAst2.Expr, datalogExp: LoweredAst2.Expr, loc: SourceLocation)(implicit root: LoweredAst.Root): LoweredAst2.Expr = {
    val defn = lookup(DatalogDefs.Filter)
    val argExps = predSymExp :: datalogExp :: Nil
    val resultType = Types.Datalog
    val itpe = Types.FilterType
    LoweredAst2.Expr.ApplyDef(defn.sym, argExps, List.empty, itpe, resultType, datalogExp.eff, loc)
  }

  /**
    * Returns `t` from the Flix type `Vector[t]`.
    */
  private def unwrapVectorType(tpe: Type, loc: SourceLocation): Type = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), extType, _) => extType
    case t => throw InternalCompilerException(
      s"Expected Type.Apply(Type.Cst(TypeConstructor.Vector, _), _, _), but got ${t}",
      loc
    )
  }

  /**
    * Returns the pairs consisting of predicates and their term types from the extensible variant
    * type `tpe`.
    */
  private def predicatesOfExtVar(tpe: Type, loc: SourceLocation): List[(Name.Pred, List[Type])] = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.Extensible, _), tpe1, loc1) =>
      predicatesOfSchemaRow(tpe1, loc1)
    case t => throw InternalCompilerException(
      s"Expected Type.Apply(Type.Cst(TypeConstructor.Extensible, _), _, _), but got ${t}",
      loc
    )
  }

  /**
    * Returns the pairs consisting of predicates and their term types from the SchemaRow `row`.
    */
  private def predicatesOfSchemaRow(row: Type, loc: SourceLocation): List[(Name.Pred, List[Type])] = row match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), rel, loc2), tpe2, loc1) =>
      (pred, termTypesOfRelation(rel, loc2)) :: predicatesOfSchemaRow(tpe2, loc1)
    case Type.Var(_, _) => Nil
    case Type.SchemaRowEmpty => Nil
    case t => throw InternalCompilerException(s"Got unexpected ${t}", loc)
  }

  /**
    * Returns the types constituting a `Type.Relation`.
    */
  private def termTypesOfRelation(rel: Type, loc: SourceLocation): List[Type] = {
    def f(rel0: Type, loc0: SourceLocation): List[Type] = rel0 match {
      case Type.Cst(TypeConstructor.Relation(_), _) => Nil
      case Type.Apply(rest, t, loc1) => t :: f(rest, loc1)
      case t => throw InternalCompilerException(s"Expected Type.Apply(_, _, _), but got ${t}", loc0)
    }

    f(rel, loc).reverse
  }

  /**
    * Returns the `LoweredAst` lambda expression
    * {{{
    *   predSym: PredSym -> terms: Vector[Boxed] -> match predSym {
    *     case PredSym.PredSym(name, _) => match name {
    *       case "P1" => xvar P1(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       case "P2" => xvar P2(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       ...
    *     }
    *   }
    * }}}
    * where `P1, P2, ...` are in `preds` with their respective term types.
    */
  private def mkExtVarLambda(preds: List[(Name.Pred, List[Type])], tpe: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): LoweredAst.Expr = {
    val predSymVar = Symbol.freshVarSym("predSym", BoundBy.FormalParam, loc)
    val termsVar = Symbol.freshVarSym("terms", BoundBy.FormalParam, loc)
    mkLambdaExp(predSymVar, Types.PredSym,
      mkLambdaExp(termsVar, Types.VectorOfBoxed,
        mkExtVarBody(preds, predSymVar, termsVar, tpe, loc),
        tpe, Type.Pure, loc
      ),
      Type.mkPureArrow(Types.VectorOfBoxed, tpe, loc), Type.Pure, loc
    )
  }

  /**
    * Returns the `LoweredAst` lambda expression
    * {{{
    *   paramName -> exp
    * }}}
    * where `"paramName" == param.text` and `exp` has type `expType` and effect `eff`.
    */
  private def mkLambdaExp(param: Symbol.VarSym, paramTpe: Type, exp: LoweredAst.Expr, expTpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr =
    LoweredAst.Expr.Lambda(
      LoweredAst.FormalParam(param, paramTpe, loc),
      exp,
      Type.mkArrowWithEffect(paramTpe, eff, expTpe, loc),
      loc
    )

  /**
    * Returns the `LoweredAst` match expression
    * {{{
    *   match predSym {
    *     case PredSym.PredSym(name, _) => match name {
    *       case "P1" => xvar P1(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       case "P2" => xvar P2(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       ...
    *     }
    *   }
    * }}}
    * where `P1, P2, ...` are in `preds` with their respective term types, `"predSym" == predSymVar.text`
    * and `"terms" == termsVar.text`.
    */
  private def mkExtVarBody(preds: List[(Name.Pred, List[Type])], predSymVar: Symbol.VarSym, termsVar: Symbol.VarSym, tpe: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): LoweredAst.Expr = {
    val nameVar = Symbol.freshVarSym(Name.Ident("name", loc), BoundBy.Pattern)
    LoweredAst.Expr.Match(
      exp = LoweredAst.Expr.Var(predSymVar, Types.PredSym, loc),
      rules = List(
        LoweredAst.MatchRule(
          pat = LoweredAst.Pattern.Tag(
            symUse = CaseSymUse(Symbol.mkCaseSym(Enums.PredSym, Name.Ident("PredSym", loc)), loc),
            pats = List(
              LoweredAst.Pattern.Var(nameVar, Type.Str, loc),
              LoweredAst.Pattern.Wild(Type.Int64, loc)
            ),
            tpe = Types.PredSym, loc = loc
          ),
          guard = None,
          exp = LoweredAst.Expr.Match(
            exp = LoweredAst.Expr.Var(nameVar, Type.Str, loc),
            rules = preds.map {
              case (p, types) => mkProvenanceMatchRule(termsVar, tpe, p, types, loc)
            },
            tpe = tpe, eff = Type.Pure, loc = loc
          ),
        )
      ),
      tpe = tpe, eff = Type.Pure, loc
    )
  }

  /**
    * Returns the pattern match rule
    * {{{
    *   case "P" => xvar P(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    * }}}
    * where `"P" == p.name`
    */
  private def mkProvenanceMatchRule(termsVar: Symbol.VarSym, tpe: Type, p: Name.Pred, types: List[Type], loc: SourceLocation): LoweredAst.MatchRule = {
    val termsExps = types.zipWithIndex.map {
      case (tpe1, i) => mkUnboxedTerm(termsVar, tpe1, i, loc)
    }
    LoweredAst.MatchRule(
      pat = LoweredAst.Pattern.Cst(Constant.Str(p.name), Type.Str, loc),
      guard = None,
      exp = LoweredAst.Expr.ApplyAtomic(
        op = AtomicOp.ExtTag(Name.Label(p.name, loc)),
        exps = termsExps,
        tpe = tpe, eff = Type.Pure, loc = loc
      )
    )
  }

  /**
    * Returns the `LoweredAst` expression
    * {{{
    *   unbox(Vector.get(i, terms))
    * }}}
    * where `"terms" == termsVar.text`.
    */
  private def mkUnboxedTerm(termsVar: Symbol.VarSym, tpe: Type, i: Int, loc: SourceLocation): LoweredAst.Expr = {
    LoweredAst.Expr.ApplyDef(
      sym = DatalogDefs.Unbox,
      exps = List(
        LoweredAst.Expr.ApplyDef(
          sym = Symbol.mkDefnSym(s"Vector.get"),
          exps = List(
            LoweredAst.Expr.Cst(Constant.Int32(i), Type.Int32, loc),
            LoweredAst.Expr.Var(termsVar, Types.VectorOfBoxed, loc)
          ),
          targs = List.empty,
          itpe = Type.mkPureUncurriedArrow(List(Type.Int32, Types.VectorOfBoxed), Types.Boxed, loc),
          tpe = Types.Boxed, eff = Type.Pure, loc = loc
        )
      ),
      targs = List.empty,
      itpe = Type.mkPureUncurriedArrow(List(Types.Boxed), tpe, loc),
      tpe = tpe, eff = Type.Pure, loc = loc
    )
  }

  /**
    * Extracts the fields of the given type, treating it as a tuple.
    *
    * If the given type is Unit, returns Nil.
    * If the given type is not a tuple, returns the given type.
    */
  private def extractTuplishTypes(tpe0: Type): List[Type] = {
    val tpe = eraseAliases(tpe0)
    tpe.typeConstructor match {
      case Some(TypeConstructor.Tuple(_)) => tpe.typeArguments
      case Some(TypeConstructor.Unit) => Nil
      case _ => List(tpe)
    }
  }

  /**
    * Return a list of quantified variables in the given expression `exp0`.
    *
    * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
    * but not in the constraint params `cparams0` of the constraint.
    */
  private def quantifiedVars(cparams0: List[LoweredAst.ConstraintParam], exp0: LoweredAst.Expr): List[(Symbol.VarSym, Type)] = {
    LoweredAstOps.freeVars(exp0).toList.filter {
      case (sym, _) => isQuantifiedVar(sym, cparams0)
    }
  }

  /**
    * Returns `true` if the given variable symbol `sym` is a quantified variable according to the given constraint params `cparams0`.
    *
    * That is, the variable symbol is *NOT* lexically bound.
    */
  private def isQuantifiedVar(sym: Symbol.VarSym, cparams0: List[LoweredAst.ConstraintParam]): Boolean =
    cparams0.exists(p => p.sym == sym)

  /**
    * Applies the given substitution `subst` to the given expression `exp0`.
    */
  private def substExp(exp0: LoweredAst2.Expr, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst2.Expr = exp0 match {
    case LoweredAst2.Expr.Cst(_, _, _) => exp0

    case LoweredAst2.Expr.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst2.Expr.Var(s, tpe, loc)

    case LoweredAst2.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      LoweredAst2.Expr.Lambda(p, e, tpe, loc)

    case LoweredAst2.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst2.Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case LoweredAst2.Expr.ApplyDef(sym, exps, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst2.Expr.ApplyDef(sym, es, targs, itpe, tpe, eff, loc)

    case LoweredAst2.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst2.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case LoweredAst2.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst2.Expr.ApplyOp(sym, es, tpe, eff, loc)

    case LoweredAst2.Expr.ApplySig(sym, exps, targ, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst2.Expr.ApplySig(sym, es, targ, targs, itpe, tpe, eff, loc)

    case LoweredAst2.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst2.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case LoweredAst2.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst2.Expr.Let(s, e1, e2, tpe, eff, loc)

    case LoweredAst2.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val fps = fparams.map(substFormalParam(_, subst))
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst2.Expr.LocalDef(s, fps, e1, e2, tpe, eff, loc)

    case LoweredAst2.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e = substExp(exp, subst)
      LoweredAst2.Expr.Scope(s, regionVar, e, tpe, eff, loc)

    case LoweredAst2.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      LoweredAst2.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case LoweredAst2.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst2.Expr.Stm(e1, e2, tpe, eff, loc)

    case LoweredAst2.Expr.Discard(exp, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst2.Expr.Discard(e, eff, loc)

    case LoweredAst2.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst2.MatchRule(pat, guard, exp1) =>
          val p = substPattern(pat, subst)
          val g = guard.map(substExp(_, subst))
          val e1 = substExp(exp1, subst)
          LoweredAst2.MatchRule(p, g, e1)
      }
      LoweredAst2.Expr.Match(e, rs, tpe, eff, loc)

    case LoweredAst2.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst2.ExtMatchRule(pat, exp1, loc1) =>
          val p = substExtPattern(pat, subst)
          val e1 = substExp(exp1, subst)
          LoweredAst2.ExtMatchRule(p, e1, loc1)
      }
      LoweredAst2.Expr.ExtMatch(e, rs, tpe, eff, loc)

    case LoweredAst2.Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst2.TypeMatchRule(sym, tpe1, exp1) =>
          val s = subst.getOrElse(sym, sym)
          val e1 = substExp(exp1, subst)
          LoweredAst2.TypeMatchRule(s, tpe1, e1)
      }
      LoweredAst2.Expr.TypeMatch(e, rs, tpe, eff, loc)

    case LoweredAst2.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst2.Expr.VectorLit(es, tpe, eff, loc)

    case LoweredAst2.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst2.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case LoweredAst2.Expr.VectorLength(exp, loc) =>
      val e = substExp(exp, subst)
      LoweredAst2.Expr.VectorLength(e, loc)

    case LoweredAst2.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst2.Expr.Ascribe(e, tpe, eff, loc)

    case LoweredAst2.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst2.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case LoweredAst2.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst2.CatchRule(sym, clazz, exp1) =>
          val s = subst.getOrElse(sym, sym)
          val e1 = substExp(exp1, subst)
          LoweredAst2.CatchRule(s, clazz, e1)
      }
      LoweredAst2.Expr.TryCatch(e, rs, tpe, eff, loc)

    case LoweredAst2.Expr.RunWith(exp, effSymUse, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst2.HandlerRule(opSymUse, fparams, hexp) =>
          val fps = fparams.map(substFormalParam(_, subst))
          val he = substExp(hexp, subst)
          LoweredAst2.HandlerRule(opSymUse, fps, he)
      }
      LoweredAst2.Expr.RunWith(e, effSymUse, rs, tpe, eff, loc)

    case LoweredAst2.Expr.NewObject(_, _, _, _, _, _) => exp0

  }

  /**
    * Applies the given substitution `subst` to the given formal param `fparam0`.
    */
  private def substFormalParam(fparam0: LoweredAst2.FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst2.FormalParam = fparam0 match {
    case LoweredAst2.FormalParam(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst2.FormalParam(s, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given pattern `pattern0`.
    */
  private def substPattern(pattern0: LoweredAst2.Pattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst2.Pattern = pattern0 match {
    case LoweredAst2.Pattern.Wild(tpe, loc) =>
      LoweredAst2.Pattern.Wild(tpe, loc)

    case LoweredAst2.Pattern.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst2.Pattern.Var(s, tpe, loc)

    case LoweredAst2.Pattern.Cst(cst, tpe, loc) =>
      LoweredAst2.Pattern.Cst(cst, tpe, loc)

    case LoweredAst2.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(substPattern(_, subst))
      LoweredAst2.Pattern.Tag(symUse, ps, tpe, loc)

    case LoweredAst2.Pattern.Tuple(pats, tpe, loc) =>
      val ps = pats.map(substPattern(_, subst))
      LoweredAst2.Pattern.Tuple(ps, tpe, loc)

    case LoweredAst2.Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map(substRecordLabelPattern(_, subst))
      val p = substPattern(pat, subst)
      LoweredAst2.Pattern.Record(ps, p, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given record label pattern `pattern0`.
    */
  private def substRecordLabelPattern(pattern0: LoweredAst2.Pattern.Record.RecordLabelPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst2.Pattern.Record.RecordLabelPattern = pattern0 match {
    case LoweredAst2.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val p = substPattern(pat, subst)
      LoweredAst2.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given ext pattern `pattern0`.
    */
  private def substExtPattern(pattern0: LoweredAst2.ExtPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst2.ExtPattern = pattern0 match {
    case LoweredAst2.ExtPattern.Default(loc) =>
      LoweredAst2.ExtPattern.Default(loc)

    case LoweredAst2.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(substVarOrWild(_, subst))
      LoweredAst2.ExtPattern.Tag(label, ps, loc)
  }

  /**
    * Applies the given substitution `subst` to the given ext tag pattern `pattern0`.
    */
  private def substVarOrWild(pattern0: LoweredAst2.ExtTagPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst2.ExtTagPattern = pattern0 match {
    case LoweredAst2.ExtTagPattern.Wild(tpe, loc) =>
      LoweredAst2.ExtTagPattern.Wild(tpe, loc)

    case LoweredAst2.ExtTagPattern.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst2.ExtTagPattern.Var(s, tpe, loc)

    case LoweredAst2.ExtTagPattern.Unit(tpe, loc) =>
      LoweredAst2.ExtTagPattern.Unit(tpe, loc)
  }

}
