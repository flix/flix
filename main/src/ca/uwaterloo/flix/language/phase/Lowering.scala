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
import ca.uwaterloo.flix.language.ast.LoweredAst.Expr
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.{AtomicOp, Kind, LoweredAst, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugLoweredAst
import ca.uwaterloo.flix.util.collection.{ListOps, Nel}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

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

object Lowering {

  private object Defs {
    val version: String = "3"
    lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Boxable.box")
    lazy val Unbox: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Boxable.unbox")
    lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.runSolver")
    lazy val SolveWithProvenance: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.runSolverWithProvenance")
    lazy val Merge: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.union")
    lazy val Filter: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.projectSym")
    lazy val Rename: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.rename")
    lazy val ProvenanceOf: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint3.Solver.provenanceOf")

    def ProjectInto(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.injectInto$arity")

    def Facts(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.facts$arity")

    lazy val ChannelNew: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannel")
    lazy val ChannelPut: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.put")
    lazy val ChannelGet: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.get")

    /**
      * Returns the definition associated with the given symbol `sym`.
      */
    def lookup(sym: Symbol.DefnSym)(implicit root: TypedAst.Root): TypedAst.Def = root.defs.get(sym) match {
      case None => throw InternalCompilerException(s"Symbol '$sym' not found. Missing library?", sym.loc)
      case Some(d) => d
    }
  }

  private object Enums {
    lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Datalog")
    lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Constraint")

    lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.HeadPredicate")
    lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.BodyPredicate")

    lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.HeadTerm")
    lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.BodyTerm")

    lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Shared.PredSym")
    lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.VarSym")

    lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Shared.Denotation")
    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Polarity")
    lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Fixity")

    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Boxed")

    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")

    lazy val ChannelMpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.Mpmc")
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

    lazy val ChannelMpmc: Type = Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Eff ->: Kind.Star), SourceLocation.Unknown)

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
  def run(root: TypedAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("Lowering") {
    implicit val r: TypedAst.Root = root

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val sigs = ParOps.parMapValues(root.sigs)(visitSig)
    val instances = ParOps.parMapValueList(root.instances)(visitInstance)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val restrictableEnums = ParOps.parMapValues(root.restrictableEnums)(visitRestrictableEnum)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val aliases = ParOps.parMapValues(root.typeAliases)(visitTypeAlias)

    // TypedAst.Sigs are shared between the `sigs` field and the `classes` field.
    // Instead of visiting twice, we visit the `sigs` field and then look up the results when visiting traits.
    val traits = ParOps.parMapValues(root.traits)(t => visitTrait(t, sigs))

    val newEnums = enums ++ restrictableEnums.map {
      case (_, v) => v.sym -> v
    }

    LoweredAst.Root(traits, instances, sigs, defs, newEnums, structs, effects, aliases, root.mainEntryPoint, root.entryPoints, root.sources, root.traitEnv, root.eqEnv)
  }

  /**
    * Lowers the given definition `defn0`.
    */
  private def visitDef(defn0: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Def = defn0 match {
    case TypedAst.Def(sym, spec0, exp0, loc) =>
      val spec = visitSpec(spec0)
      val exp = visitExp(exp0)(Scope.Top, root, flix)
      LoweredAst.Def(sym, spec, exp, loc)
  }

  /**
    * Lowers the given signature `sig0`.
    */
  private def visitSig(sig0: TypedAst.Sig)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Sig = sig0 match {
    case TypedAst.Sig(sym, spec0, exp0, loc) =>
      val spec = visitSpec(spec0)
      val impl = exp0.map(visitExp(_)(Scope.Top, root, flix))
      LoweredAst.Sig(sym, spec, impl, loc)
  }

  /**
    * Lowers the given instance `inst0`.
    */
  private def visitInstance(inst0: TypedAst.Instance)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Instance = inst0 match {
    case TypedAst.Instance(doc, ann, mod, sym, tparams0, tpe0, tconstrs0, econstrs0, assocs0, defs0, ns, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTraitConstraint)
      val econstrs = econstrs0.map(visitEqConstraint)
      val assocs = assocs0.map {
        case TypedAst.AssocTypeDef(defDoc, defMod, defSymUse, args, defTpe, defLoc) => LoweredAst.AssocTypeDef(defDoc, defMod, defSymUse, args, defTpe, defLoc)
      }
      val defs = defs0.map(visitDef)
      LoweredAst.Instance(doc, ann, mod, sym, tparams, tpe, tconstrs, econstrs, assocs, defs, ns, loc)
  }

  /**
    * Lowers the given enum `enum0`.
    */
  private def visitEnum(enum0: TypedAst.Enum)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Enum = enum0 match {
    case TypedAst.Enum(doc, ann, mod, sym, tparams0, derives, cases0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val cases = cases0.map {
        case (_, TypedAst.Case(caseSym, tpes0, caseSc0, caseLoc)) =>
          val tpes = tpes0.map(visitType)
          val caseSc = visitScheme(caseSc0)
          (caseSym, LoweredAst.Case(caseSym, tpes, caseSc, caseLoc))
      }
      LoweredAst.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
  }

  /**
    * Lowers the given struct `struct0`.
    */
  private def visitStruct(struct0: TypedAst.Struct)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Struct = struct0 match {
    case TypedAst.Struct(doc, ann, mod, sym, tparams0, _, fields0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val fields = fields0.map {
        case (fieldSym, field) =>
          LoweredAst.StructField(fieldSym, visitType(field.tpe), loc)
      }
      LoweredAst.Struct(doc, ann, mod, sym, tparams, fields.toList, loc)
  }

  /**
    * Lowers the given enum `enum0` from a restrictable enum into a regular enum.
    */
  private def visitRestrictableEnum(enum0: TypedAst.RestrictableEnum)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Enum = enum0 match {
    case TypedAst.RestrictableEnum(doc, ann, mod, sym0, index0, tparams0, derives, cases0, loc) =>
      // index is erased since related checking has concluded.
      // Restrictable tag is lowered into a regular tag
      val index = visitTypeParam(index0)
      val tparams = tparams0.map(visitTypeParam)
      val cases = cases0.map {
        case (_, TypedAst.RestrictableCase(caseSym0, tpes0, caseSc0, caseLoc)) =>
          val tpes = tpes0.map(visitType)
          val caseSc = visitScheme(caseSc0)
          val caseSym = visitRestrictableCaseSym(caseSym0)
          (caseSym, LoweredAst.Case(caseSym, tpes, caseSc, caseLoc))
      }
      val sym = visitRestrictableEnumSym(sym0)
      LoweredAst.Enum(doc, ann, mod, sym, index :: tparams, derives, cases, loc)
  }

  /**
    * Lowers `sym` from a restrictable case sym into a regular case sym.
    */
  private def visitRestrictableCaseSym(sym: Symbol.RestrictableCaseSym): Symbol.CaseSym = {
    val enumSym = visitRestrictableEnumSym(sym.enumSym)
    new Symbol.CaseSym(enumSym, sym.name, sym.loc)
  }

  /**
    * Lowers `sym` from a restrictable case sym use into a regular case sym use.
    */
  private def visitRestrictableCaseSymUse(symUse: RestrictableCaseSymUse): CaseSymUse = {
    CaseSymUse(visitRestrictableCaseSym(symUse.sym), symUse.sym.loc)
  }

  /**
    * Lowers `sym` from a restrictable enum sym into a regular enum sym.
    */
  private def visitRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): Symbol.EnumSym =
    new Symbol.EnumSym(None, sym.namespace, sym.name, sym.loc)

  /**
    * Lowers the given `effect`.
    */
  private def visitEffect(effect: TypedAst.Effect)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Effect = effect match {
    case TypedAst.Effect(doc, ann, mod, sym, _, ops0, loc) =>
      // TODO EFFECT-TPARAMS use tparams
      val ops = ops0.map(visitOp)
      LoweredAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Lowers the given `op`.
    */
  private def visitOp(op: TypedAst.Op)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Op = op match {
    case TypedAst.Op(sym, spec0, loc) =>
      val spec = visitSpec(spec0)
      LoweredAst.Op(sym, spec, loc)
  }

  /**
    * Lowers the given type `alias`.
    */
  private def visitTypeAlias(alias: TypedAst.TypeAlias)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.TypeAlias = alias match {
    case TypedAst.TypeAlias(doc, _, mod, sym, tparams0, tpe0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      LoweredAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
  }

  /**
    * Lowers the given type constraint `tconstr0`.
    */
  private def visitTraitConstraint(tconstr0: TraitConstraint)(implicit root: TypedAst.Root, flix: Flix): TraitConstraint = tconstr0 match {
    case TraitConstraint(head, tpe0, loc) =>
      val tpe = visitType(tpe0)
      TraitConstraint(head, tpe, loc)
  }

  /**
    * Lowers the given equality constraint `econstr0`.
    */
  private def visitEqConstraint(econstr0: EqualityConstraint)(implicit root: TypedAst.Root, flix: Flix): EqualityConstraint = econstr0 match {
    case EqualityConstraint(symUse, t1, t2, loc) =>
      val tpe1 = visitType(t1)
      val tpe2 = visitType(t2)
      EqualityConstraint(symUse, tpe1, tpe2, loc)
  }

  /**
    * Lowers the given trait `trt0`, with the given lowered sigs `sigs`.
    */
  private def visitTrait(trt0: TypedAst.Trait, sigs: Map[Symbol.SigSym, LoweredAst.Sig])(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Trait = trt0 match {
    case TypedAst.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, signatures0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superTraits = superTraits0.map(visitTraitConstraint)
      val assocs = assocs0.map {
        case TypedAst.AssocTypeSig(sigDoc, sigMod, sigSym, sigTparam, kind, _, sigLoc) => LoweredAst.AssocTypeSig(sigDoc, sigMod, sigSym, sigTparam, kind, sigLoc)
      }
      val signatures = signatures0.map(sig => sigs(sig.sym))
      val laws = laws0.map(visitDef)
      LoweredAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, signatures, laws, loc)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def visitSpec(spec0: TypedAst.Spec)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Spec = spec0 match {
    case TypedAst.Spec(doc, ann, mod, tparams0, fparams, declaredScheme, retTpe, eff, tconstrs, _) =>
      val tparam = tparams0.map(visitTypeParam)
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      LoweredAst.Spec(doc, ann, mod, tparam, fs, ds, retTpe, eff, tconstrs)
  }

  /**
    * Lowers the given `tparam`.
    */
  private def visitTypeParam(tparam: TypedAst.TypeParam): LoweredAst.TypeParam = tparam match {
    case TypedAst.TypeParam(name, sym, loc) => LoweredAst.TypeParam(name, sym, loc)
  }

  /**
    * Lowers the given expression `exp0`.
    */
  private def visitExp(exp0: TypedAst.Expr)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = exp0 match {
    case TypedAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.Cst(cst, t, loc)

    case TypedAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.Var(sym, t, loc)

    case TypedAst.Expr.Hole(sym, _, tpe, eff, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, eff, loc)

    case TypedAst.Expr.HoleWithExp(_, _, tpe, _, loc) =>
      val sym = Symbol.freshHoleSym(loc)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp) // TODO RESTR-VARS maybe add to loweredAST

    case TypedAst.Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case TypedAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Lambda(p, e, t, loc)

    case TypedAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyClo(e1, e2, t, eff, loc)

    case TypedAst.Expr.ApplyDef(DefSymUse(sym, _), exps, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val tas = targs.map(visitType)
      val it = visitType(itpe)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyDef(sym, es, tas, it, t, eff, loc)

    case TypedAst.Expr.ApplyLocalDef(LocalDefSymUse(sym, _), exps, _, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyLocalDef(sym, es, t, eff, loc)

    case TypedAst.Expr.ApplyOp(OpSymUse(sym, _), exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      LoweredAst.Expr.ApplyOp(sym, es, tpe, eff, loc)

    case TypedAst.Expr.ApplySig(SigSymUse(sym, _), exps, targ, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val ta = visitType(targ)
      val tas = targs.map(visitType)
      val it = visitType(itpe)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplySig(sym, es, ta, tas, it, t, eff, loc)

    case TypedAst.Expr.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Unary(sop), List(e), t, eff, loc)

    case TypedAst.Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.Let(bnd.sym, e1, e2, t, eff, loc)

    case TypedAst.Expr.LocalDef(TypedAst.Binder(sym, _), fparams, exp1, exp2, tpe, eff, loc) =>
      val fps = fparams.map(visitFormalParam)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.LocalDef(sym, fps, e1, e2, t, eff, loc)

    case TypedAst.Expr.Region(TypedAst.Binder(sym, _), regionVar, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Region(sym, regionVar, e, t, eff, loc)

    case TypedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst.Expr.IfThenElse(e1, e2, e3, t, eff, loc)

    case TypedAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.Stm(e1, e2, t, eff, loc)

    case TypedAst.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.Discard(e, eff, loc)

    case TypedAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.Match(e, rs, t, eff, loc)

    case TypedAst.Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitTypeMatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.TypeMatch(e, rs, t, eff, loc)

    case TypedAst.Expr.RestrictableChoose(_, exp, rules, tpe, eff, loc) =>
      // lower into an ordinary match
      val e = visitExp(exp)
      val rs = rules.map(visitRestrictableChooseRule)
      val t = visitType(tpe)
      LoweredAst.Expr.Match(e, rs, t, eff, loc)

    case TypedAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitExtMatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.ExtMatch(e, rs, t, eff, loc)

    case TypedAst.Expr.Tag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Tag(symUse.sym), es, t, eff, loc)

    case TypedAst.Expr.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      // Lower a restrictable tag into a normal tag.
      val caseSym = visitRestrictableCaseSym(symUse.sym)
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), es, t, eff, loc)

    case TypedAst.Expr.ExtTag(label, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ExtTag(label), es, t, eff, loc)

    case TypedAst.Expr.Tuple(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Tuple, es, t, eff, loc)

    case TypedAst.Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordSelect(label), List(e), t, eff, loc)

    case TypedAst.Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordExtend(label), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordRestrict(label), List(e), t, eff, loc)

    case TypedAst.Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayLit, e :: es, t, eff, loc)

    case TypedAst.Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayNew, List(e1, e2, e3), t, eff, loc)

    case TypedAst.Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayLoad, List(e1, e2), t, eff, loc)

    case TypedAst.Expr.ArrayLength(exp, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayLength, List(e), Type.Int32, eff, loc)

    case TypedAst.Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayStore, List(e1, e2, e3), Type.Unit, eff, loc)

    case TypedAst.Expr.StructNew(sym, fields0, region0, tpe, eff, loc) =>
      val fields = fields0.map { case (k, v) => (k, visitExp(v)) }
      val (names0, es) = fields.unzip
      val names = names0.map(_.sym)
      region0.map(visitExp) match {
        case Some(region) =>
          LoweredAst.Expr.ApplyAtomic(AtomicOp.StructNew(sym, Mutability.Mutable, names), region :: es, tpe, eff, loc)
        case None =>
          LoweredAst.Expr.ApplyAtomic(AtomicOp.StructNew(sym, Mutability.Immutable, names), es, tpe, eff, loc)
      }

    case TypedAst.Expr.StructGet(exp, field, tpe, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.StructGet(field.sym), List(e), tpe, eff, loc)

    case TypedAst.Expr.StructPut(exp, field, exp1, tpe, eff, loc) =>
      val struct = visitExp(exp)
      val rhs = visitExp(exp1)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.StructPut(field.sym), List(struct, rhs), tpe, eff, loc)

    case TypedAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.VectorLit(es, t, eff, loc)

    case TypedAst.Expr.VectorLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      LoweredAst.Expr.VectorLoad(b, i, t, eff, loc)

    case TypedAst.Expr.VectorLength(base, loc) =>
      val b = visitExp(base)
      LoweredAst.Expr.VectorLength(b, loc)

    case TypedAst.Expr.Ascribe(exp, _, _, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Ascribe(e, t, eff, loc)

    case TypedAst.Expr.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InstanceOf(clazz), List(e), Type.Bool, e.eff, loc)

    case TypedAst.Expr.CheckedCast(_, exp, tpe, eff, loc) =>
      // Note: We do *NOT* erase checked (i.e. safe) casts.
      // In Java, `String` is a subtype of `Object`, but the Flix IR makes this upcast _explicit_.
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Cast(e, Some(t), None, t, eff, loc)

    case TypedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(visitType)
      val t = visitType(tpe)
      LoweredAst.Expr.Cast(e, dt, declaredEff, t, eff, loc)

    case TypedAst.Expr.Unsafe(exp, _, tpe, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.Cast(e, None, Some(eff), tpe, eff, loc)

    case TypedAst.Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case TypedAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.TryCatch(e, rs, t, eff, loc)

    case TypedAst.Expr.Throw(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Throw, List(e), t, eff, loc)

    case TypedAst.Expr.Handler(symUse, rules, bodyTpe, bodyEff, handledEff, tpe, loc) =>
      // handler sym { rules }
      // is lowered to
      // handlerBody -> try handlerBody() with sym { rules }
      val bodySym = Symbol.freshVarSym("handlerBody", BoundBy.FormalParam, loc.asSynthetic)
      val rs = rules.map(visitHandlerRule)
      val bt = visitType(bodyTpe)
      val t = visitType(tpe)
      val bodyThunkType = Type.mkArrowWithEffect(Type.Unit, bodyEff, bt, loc.asSynthetic)
      val bodyVar = LoweredAst.Expr.Var(bodySym, bodyThunkType, loc.asSynthetic)
      val body = LoweredAst.Expr.ApplyClo(bodyVar, LoweredAst.Expr.Cst(Constant.Unit, Type.Unit, loc.asSynthetic), bt, bodyEff, loc.asSynthetic)
      val RunWith = LoweredAst.Expr.RunWith(body, symUse, rs, bt, handledEff, loc)
      val param = LoweredAst.FormalParam(bodySym, bodyThunkType, loc.asSynthetic)
      LoweredAst.Expr.Lambda(param, RunWith, t, loc)

    case TypedAst.Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      // run exp1 with exp2
      // is lowered to
      // exp2(_runWith -> exp1)
      val unitParam = LoweredAst.FormalParam(Symbol.freshVarSym("_runWith", BoundBy.FormalParam, loc.asSynthetic), Type.Unit, loc.asSynthetic)
      val thunkType = Type.mkArrowWithEffect(Type.Unit, exp1.eff, exp1.tpe, loc.asSynthetic)
      val thunk = LoweredAst.Expr.Lambda(unitParam, visitExp(exp1), thunkType, loc.asSynthetic)
      LoweredAst.Expr.ApplyClo(visitExp(exp2), thunk, tpe, eff, loc)

    case TypedAst.Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InvokeConstructor(constructor), es, t, eff, loc)

    case TypedAst.Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InvokeMethod(method), e :: es, t, eff, loc)

    case TypedAst.Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InvokeStaticMethod(method), es, t, eff, loc)

    case TypedAst.Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.GetField(field), List(e), t, eff, loc)

    case TypedAst.Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.PutField(field), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.GetStaticField(field), List.empty, t, eff, loc)

    case TypedAst.Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.PutStaticField(field), List(e), t, eff, loc)

    case TypedAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod)
      LoweredAst.Expr.NewObject(name, clazz, t, eff, ms, loc)

    case TypedAst.Expr.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.NewChannel(e, t, eff, loc)

    case TypedAst.Expr.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.GetChannel(e, t, eff, loc)

    case TypedAst.Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.PutChannel(e1, e2, t, eff, loc)

    case TypedAst.Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map { case TypedAst.SelectChannelRule(TypedAst.Binder(sym, _), chan, exp, loc0) =>
        LoweredAst.SelectChannelRule(sym, visitExp(chan), visitExp(exp), loc0)
      }
      val d = default.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.SelectChannel(rs, d, t, eff, loc)

    case TypedAst.Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e1, e2), t, eff, loc)

    case TypedAst.Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val fs = frags.map {
        case TypedAst.ParYieldFragment(pat, fragExp, fragLoc) => LoweredAst.ParYieldFragment(visitPat(pat), visitExp(fragExp), fragLoc)
      }
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkParYield(fs, e, t, eff, loc)

    case TypedAst.Expr.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Lazy, List(e), t, Type.Pure, loc)

    case TypedAst.Expr.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Force, List(e), t, eff, loc)

    case TypedAst.Expr.FixpointConstraintSet(cs, _, loc) =>
      mkDatalog(cs, loc)

    case TypedAst.Expr.FixpointLambda(pparams, exp, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Rename)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.PredSym, loc)
      val argExps = predExps :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expr.ApplyDef(defn.sym, argExps, List.empty, Types.RenameType, resultType, eff, loc)

    case TypedAst.Expr.FixpointMerge(exp1, exp2, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Merge)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expr.ApplyDef(defn.sym, argExps, List.empty, Types.MergeType, resultType, eff, loc)

    case TypedAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tpe, eff, loc) =>
      // Create appropriate call to Fixpoint.Solver.provenanceOf. This requires creating a mapping, mkExtVar, from
      // PredSym and terms to an extensible variant.
      val defn = Defs.lookup(Defs.ProvenanceOf)
      val mergedExp = mergeExps(exps.map(visitExp), loc)
      val (goalPredSym, goalTerms) = select match {
        case TypedAst.Predicate.Head.Atom(pred, _, terms, _, loc1) =>
          val boxedTerms = terms.map(t => box(visitExp(t)))
          (mkPredSym(pred), mkVector(boxedTerms, Types.Boxed, loc1))
      }
      val withPredSyms = mkVector(withh.map(mkPredSym), Types.PredSym, loc)
      val extVarType = unwrapVectorType(tpe, loc)
      val preds = predicatesOfExtVar(extVarType, loc)
      val lambdaExp = mkExtVarLambda(preds, extVarType, loc)
      val argExps = goalPredSym :: goalTerms :: withPredSyms :: lambdaExp :: mergedExp :: Nil
      val itpe = Types.mkProvenanceOf(extVarType, loc)
      LoweredAst.Expr.ApplyDef(defn.sym, argExps, List.empty, itpe, tpe, eff, loc)

    case TypedAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, _, _, pred, tpe, eff, loc) =>
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
      val sym = Defs.Facts(predArity)
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), tpe, loc)

      // Merge and solve exps
      val mergedExp = mergeExps(loweredQueryExp :: loweredExps, loc)
      val solvedExp = LoweredAst.Expr.ApplyDef(Defs.Solve, mergedExp :: Nil, List.empty, Types.SolveType, Types.Datalog, eff, loc)

      // Put everything together
      val argExps = mkPredSym(pred) :: solvedExp :: Nil
      LoweredAst.Expr.ApplyDef(sym, argExps, targs, defTpe, tpe, eff, loc)

    case TypedAst.Expr.FixpointSolveWithProject(exps0, optPreds, mode, _, eff, loc) =>
      // Rewrites
      //     solve e₁, e₂, e₃ project P₁, P₂, P₃
      // to
      //     let tmp% = solve e₁ <+> e₂ <+> e₃;
      //     merge (project P₁ tmp%, project P₂ tmp%, project P₃ tmp%)
      //
      val defn = mode match {
        case SolveMode.Default => Defs.lookup(Defs.Solve)
        case SolveMode.WithProvenance => Defs.lookup(Defs.SolveWithProvenance)
      }
      val exps = exps0.map(visitExp)
      val mergedExp = mergeExps(exps, loc)
      val argExps = mergedExp :: Nil
      val solvedExp = LoweredAst.Expr.ApplyDef(defn.sym, argExps, List.empty, Types.SolveType, Types.Datalog, eff, loc)
      val tmpVarSym = Symbol.freshVarSym("tmp%", BoundBy.Let, loc)
      val letBodyExp = optPreds match {
        case Some(preds) =>
          mergeExps(preds.map(pred => {
            val varExp = LoweredAst.Expr.Var(tmpVarSym, Types.Datalog, loc)
            projectSym(mkPredSym(pred), varExp, loc)
          }), loc)
        case None => LoweredAst.Expr.Var(tmpVarSym, Types.Datalog, loc)
      }
      LoweredAst.Expr.Let(tmpVarSym, solvedExp, letBodyExp, Types.Datalog, eff, loc)

    case TypedAst.Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
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
          val sym = Defs.ProjectInto(targs.length)

          // The type of the function.
          val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, exp.tpe), Types.Datalog, loc)

          // Put everything together.
          val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
          LoweredAst.Expr.ApplyDef(sym, argExps, targ :: targs, defTpe, Types.Datalog, exp.eff, loc)
      }
      mergeExps(loweredExps, loc)

    case TypedAst.Expr.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)

  }

  /**
    * Lowers the given pattern `pat0`.
    */
  private def visitPat(pat0: TypedAst.Pattern)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Pattern = pat0 match {
    case TypedAst.Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Pattern.Wild(t, loc)

    case TypedAst.Pattern.Var(TypedAst.Binder(sym, _), tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Pattern.Var(sym, t, loc)

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      LoweredAst.Pattern.Cst(cst, tpe, loc)

    case TypedAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(visitPat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Tag(symUse, ps, t, loc)

    case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Tuple(es, t, loc)

    case TypedAst.Pattern.Record(pats, pat, tpe, loc) =>
      val patsVal = pats.map {
        case TypedAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val p1 = visitPat(pat1)
          val t1 = visitType(tpe1)
          LoweredAst.Pattern.Record.RecordLabelPattern(label, p1, t1, loc1)
      }
      val patVal = visitPat(pat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Record(patsVal, patVal, t, loc)

    case TypedAst.Pattern.Error(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)
  }

  /**
    * Lowers the given scheme `sc0`.
    */
  private def visitScheme(sc0: Scheme)(implicit root: TypedAst.Root, flix: Flix): Scheme = sc0 match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      // TODO: What about constraints?
      val b = visitType(base)
      Scheme(quantifiers, tconstrs, econstrs, b)
  }

  /**
    * Lowers the given type `tpe0`.
    */
  private def visitType(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = tpe0.typeConstructor match {
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
  private def visitTypeNonSchema(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = tpe0 match {
    case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

    case Type.Var(_, _) => tpe0

    // Rewrite Sender[t] to Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe, _) =>
      val t = visitType(tpe)
      mkChannelTpe(t, loc)

    // Rewrite Receiver[t] to Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Receiver, loc), tpe, _) =>
      val t = visitType(tpe)
      mkChannelTpe(t, loc)

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
  private def visitFormalParam(fparam0: TypedAst.FormalParam)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.FormalParam = fparam0 match {
    case TypedAst.FormalParam(bnd, tpe, _, loc) =>
      val t = visitType(tpe)
      LoweredAst.FormalParam(bnd.sym, t, loc)
  }

  /**
    * Lowers the given restrictable choice rule `rule0` to a match rule.
    */
  private def visitRestrictableChooseRule(rule0: TypedAst.RestrictableChooseRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.MatchRule = rule0 match {
    case TypedAst.RestrictableChooseRule(pat, exp) =>
      val e = visitExp(exp)
      pat match {
        case TypedAst.RestrictableChoosePattern.Tag(symUse, pat0, tpe, loc) =>
          val termPatterns = pat0.map {
            case TypedAst.RestrictableChoosePattern.Var(TypedAst.Binder(varSym, _), varTpe, varLoc) => LoweredAst.Pattern.Var(varSym, varTpe, varLoc)
            case TypedAst.RestrictableChoosePattern.Wild(wildTpe, wildLoc) => LoweredAst.Pattern.Wild(wildTpe, wildLoc)
            case TypedAst.RestrictableChoosePattern.Error(_, errLoc) => throw InternalCompilerException("unexpected restrictable choose variable", errLoc)
          }
          val tagSymUse = visitRestrictableCaseSymUse(symUse)
          val p = LoweredAst.Pattern.Tag(tagSymUse, termPatterns, tpe, loc)
          LoweredAst.MatchRule(p, None, e)
        case TypedAst.RestrictableChoosePattern.Error(_, loc) => throw InternalCompilerException("unexpected error restrictable choose pattern", loc)
      }
  }

  private def visitExtMatchRule(rule0: TypedAst.ExtMatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.ExtMatchRule = rule0 match {
    case TypedAst.ExtMatchRule(pat, exp, loc) =>
      val p = visitExtPat(pat)
      val e = visitExp(exp)
      LoweredAst.ExtMatchRule(p, e, loc)
  }

  private def visitExtPat(pat0: TypedAst.ExtPattern): LoweredAst.ExtPattern = pat0 match {
    case TypedAst.ExtPattern.Default(loc) =>
      LoweredAst.ExtPattern.Default(loc)

    case TypedAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(visitExtTagPat)
      LoweredAst.ExtPattern.Tag(label, ps, loc)

    case TypedAst.ExtPattern.Error(loc) => throw InternalCompilerException("unexpected error ext pattern", loc)

  }

  private def visitExtTagPat(pat0: TypedAst.ExtTagPattern): LoweredAst.ExtTagPattern = pat0 match {
    case TypedAst.ExtTagPattern.Wild(tpe, loc) => LoweredAst.ExtTagPattern.Wild(tpe, loc)
    case TypedAst.ExtTagPattern.Var(bnd, tpe, loc) => LoweredAst.ExtTagPattern.Var(bnd.sym, tpe, loc)
    case TypedAst.ExtTagPattern.Unit(tpe, loc) => LoweredAst.ExtTagPattern.Unit(tpe, loc)
    case TypedAst.ExtTagPattern.Error(_, loc) => throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def visitCatchRule(rule0: TypedAst.CatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.CatchRule = rule0 match {
    case TypedAst.CatchRule(bnd, clazz, exp, _) =>
      val e = visitExp(exp)
      LoweredAst.CatchRule(bnd.sym, clazz, e)
  }

  /**
    * Lowers the given handler rule `rule0`.
    */
  private def visitHandlerRule(rule0: TypedAst.HandlerRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.HandlerRule = rule0 match {
    case TypedAst.HandlerRule(symUse, fparams0, exp, _) =>
      val fparams = fparams0.map(visitFormalParam)
      val e = visitExp(exp)
      LoweredAst.HandlerRule(symUse, fparams, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: TypedAst.MatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.MatchRule = rule0 match {
    case TypedAst.MatchRule(pat, guard, exp, _) =>
      val p = visitPat(pat)
      val g = guard.map(visitExp)
      val e = visitExp(exp)
      LoweredAst.MatchRule(p, g, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitTypeMatchRule(rule0: TypedAst.TypeMatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.TypeMatchRule = rule0 match {
    case TypedAst.TypeMatchRule(bnd, tpe, exp, _) =>
      val e = visitExp(exp)
      LoweredAst.TypeMatchRule(bnd.sym, tpe, e)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Datalog` value from the given list of Datalog constraints `cs`.
    */
  private def mkDatalog(cs: List[TypedAst.Constraint], loc: SourceLocation)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
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
  private def visitConstraint(c0: TypedAst.Constraint)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = c0 match {
    case TypedAst.Constraint(cparams, head, body, loc) =>
      val headExp = visitHeadPred(cparams, head)
      val bodyExp = mkVector(body.map(visitBodyPred(cparams, _)), Types.BodyPredicate, loc)
      val innerExp = List(headExp, bodyExp)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Head)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = p0 match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val termsExp = mkVector(terms.map(visitHeadTerm(cparams0, _)), Types.HeadTerm, loc)
      val innerExp = List(predSymExp, denotationExp, termsExp)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Body)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = p0 match {
    case TypedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val fixityExp = mkFixity(fixity, loc)
      val termsExp = mkVector(terms.map(visitBodyTerm(cparams0, _)), Types.BodyTerm, loc)
      val innerExp = List(predSymExp, denotationExp, polarityExp, fixityExp, termsExp)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case TypedAst.Predicate.Body.Functional(outBnds, exp0, loc) =>
      val outVars = outBnds.map(_.sym)
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val inVars = quantifiedVars(cparams0, exp0)
      val exp = visitExp(exp0)
      mkFunctional(outVars, inVars, exp, loc)

    case TypedAst.Predicate.Body.Guard(exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val quantifiedFreeVars = quantifiedVars(cparams0, exp0)
      val exp = visitExp(exp0)
      mkGuard(quantifiedFreeVars, exp, loc)

  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def visitHeadTerm(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    //
    // We need to consider four cases:
    //
    // Case 1.1: The expression is quantified variable. We translate it to a Var.
    // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case TypedAst.Expr.Var(sym, _, _) =>
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
          mkAppTerm(quantifiedFreeVars, visitExp(exp0), exp0.loc)
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def visitBodyTerm(cparams0: List[TypedAst.ConstraintParam], pat0: TypedAst.Pattern): LoweredAst.Expr = pat0 match {
    case TypedAst.Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case TypedAst.Pattern.Var(TypedAst.Binder(sym, _), tpe, loc) =>
      if (isQuantifiedVar(sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(sym)
      } else {
        // Case 2: Lexically bound variable *expression*.
        mkBodyTermLit(box(LoweredAst.Expr.Var(sym, tpe, loc)))
      }

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      mkBodyTermLit(box(LoweredAst.Expr.Cst(cst, tpe, loc)))

    case TypedAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Error(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

  }

  /**
    * Lowers the given JvmMethod `method`.
    */
  private def visitJvmMethod(method: TypedAst.JvmMethod)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, retTyp, eff, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      val t = visitType(retTyp)
      LoweredAst.JvmMethod(ident, fs, e, t, eff, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym): LoweredAst.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: LoweredAst.Expr): LoweredAst.Expr = {
    mkTag(Enums.HeadTerm, "Lit", List(exp), Types.HeadTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation): LoweredAst.Expr = {
    mkTag(Enums.BodyTerm, "Wild", Nil, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym): LoweredAst.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: LoweredAst.Expr): LoweredAst.Expr = {
    mkTag(Enums.BodyTerm, "Lit", List(exp), Types.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = d match {
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

          val latticeSym: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Ast.Shared.lattice")
          val latticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)

          val boxSym: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Ast.Shared.box")
          val boxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)

          val innerApply = LoweredAst.Expr.ApplyDef(latticeSym, List(LoweredAst.Expr.Cst(Constant.Unit, Type.Unit, loc)), List(innerType), latticeType, unboxedDenotationType, Type.Pure, loc)
          LoweredAst.Expr.ApplyDef(boxSym, List(innerApply), List(innerType), boxType, boxedDenotationType, Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation): LoweredAst.Expr = p match {
    case Polarity.Positive =>
      mkTag(Enums.Polarity, "Positive", Nil, Types.Polarity, loc)

    case Polarity.Negative =>
      mkTag(Enums.Polarity, "Negative", Nil, Types.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Fixity` from the given fixity `f`.
    */
  private def mkFixity(f: Fixity, loc: SourceLocation): LoweredAst.Expr = f match {
    case Fixity.Loose =>
      mkTag(Enums.Fixity, "Loose", Nil, Types.Fixity, loc)

    case Fixity.Fixed =>
      mkTag(Enums.Fixity, "Fixed", Nil, Types.Fixity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred): LoweredAst.Expr = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = LoweredAst.Expr.Cst(Constant.Str(sym), Type.Str, loc)
      val idExp = LoweredAst.Expr.Cst(Constant.Int64(0), Type.Int64, loc)
      val inner = List(nameExp, idExp)
      mkTag(Enums.PredSym, "PredSym", inner, Types.PredSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): LoweredAst.Expr = {
    val nameExp = LoweredAst.Expr.Cst(Constant.Str(sym.text), Type.Str, sym.loc)
    mkTag(Enums.VarSym, "VarSym", List(nameExp), Types.VarSym, sym.loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: LoweredAst.Expr): LoweredAst.Expr = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed, loc)
    LoweredAst.Expr.ApplyDef(Defs.Box, List(exp), List(exp.tpe), tpe, Types.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.GuardX`.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation)(implicit scope: Scope, flix: Flix): LoweredAst.Expr = {
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
      val fparam = LoweredAst.FormalParam(sym, Type.Unit, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = LoweredAst.Expr.Lambda(fparam, exp, tpe, loc)
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
        val fparam = LoweredAst.FormalParam(freshSym, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expr.Lambda(fparam, acc, lambdaType, loc)
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
  private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation)(implicit flix: Flix): LoweredAst.Expr = {
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
        val fparam = LoweredAst.FormalParam(freshSym, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expr.Lambda(fparam, acc, lambdaType, loc)
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
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation)(implicit flix: Flix): LoweredAst.Expr = {
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
        val fparam = LoweredAst.FormalParam(freshSym, tpe, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs.map(_._2), exp.tpe)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Make a new channel expression
    */
  private def mkNewChannel(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val (targ, _) = extractChannelTpe(tpe)
    LoweredAst.Expr.ApplyDef(Defs.ChannelNew, exp :: Nil, List(targ), itpe, tpe, eff, loc)
  }

  /**
    * Make a channel get expression
    */
  private def mkGetChannel(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    LoweredAst.Expr.ApplyDef(Defs.ChannelGet, exp :: Nil, List(tpe), itpe, tpe, eff, loc)
  }

  /**
    * Make a channel put expression
    */
  private def mkPutChannel(exp1: LoweredAst.Expr, exp2: LoweredAst.Expr, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val itpe = Type.mkIoUncurriedArrow(List(exp2.tpe, exp1.tpe), Type.Unit, loc)
    val targ = exp2.tpe
    LoweredAst.Expr.ApplyDef(Defs.ChannelPut, List(exp2, exp1), List(targ), itpe, Type.Unit, eff, loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: LoweredAst.Expr, argTypes: List[Type], resultType: Type): LoweredAst.Expr = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Boxable.lift${argTypes.length}")

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
    LoweredAst.Expr.ApplyDef(sym, List(exp0), argTypes :+ resultType, liftType, returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    */
  private def liftXb(exp0: LoweredAst.Expr, argTypes: List[Type]): LoweredAst.Expr = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Boxable.lift${argTypes.length}b")

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
    LoweredAst.Expr.ApplyDef(sym, List(exp0), argTypes, liftType, returnType, Type.Pure, exp0.loc)
  }


  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
    */
  private def liftXY(outVars: List[Symbol.VarSym], exp0: LoweredAst.Expr, argTypes: List[Type], resultType: Type, loc: SourceLocation): LoweredAst.Expr = {
    // Compute the number of bound ("output") and free ("input") variables.
    val numberOfInVars = argTypes.length
    val numberOfOutVars = outVars.length

    // Compute the liftXY symbol.
    // For example, lift3X2 is a function from three arguments to a Vector of pairs.
    val sym = Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Boxable.lift${numberOfInVars}X$numberOfOutVars")

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
    LoweredAst.Expr.ApplyDef(sym, List(exp0), targs, liftType, returnType, Type.Pure, loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkList(exps: List[LoweredAst.Expr], elmType: Type, loc: SourceLocation): LoweredAst.Expr = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, loc)
    }
  }

  /**
    * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkVector(exps: List[LoweredAst.Expr], elmType: Type, loc: SourceLocation): LoweredAst.Expr = {
    LoweredAst.Expr.VectorLit(exps, Type.mkVector(elmType, loc), Type.Pure, loc)
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    */
  private def mkNil(elmType: Type, loc: SourceLocation): LoweredAst.Expr = {
    mkTag(Enums.FList, "Nil", Nil, Types.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
    */
  private def mkCons(hd: LoweredAst.Expr, tail: LoweredAst.Expr, loc: SourceLocation): LoweredAst.Expr = {
    mkTag(Enums.FList, "Cons", List(hd, tail), tail.tpe, loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exps: List[LoweredAst.Expr], tpe: Type, loc: SourceLocation): LoweredAst.Expr = {
    val caseSym = new Symbol.CaseSym(sym, tag, loc.asSynthetic)
    LoweredAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, tpe, Type.Pure, loc)
  }

  /**
    * Returns a new `VarSym` for use in a let-binding.
    *
    * This function is called `mkLetSym` to avoid confusion with [[mkVarSym]].
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit scope: Scope, flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)
  }

  /**
    * Returns an expression merging `exps` using `Defs.Merge`.
    */
  private def mergeExps(exps: List[LoweredAst.Expr], loc: SourceLocation)(implicit root: TypedAst.Root): LoweredAst.Expr =
    exps.reduceRight {
      (exp, acc) =>
        val defn = Defs.lookup(Defs.Merge)
        val argExps = exp :: acc :: Nil
        val resultType = Types.Datalog
        val itpe = Types.MergeType
        LoweredAst.Expr.ApplyDef(defn.sym, argExps, List.empty, itpe, resultType, exp.eff, loc)
    }

  /**
    * Returns a new `Datalog` from `datalogExp` containing only facts from the predicate given by the `PredSym` `predSymExp`
    * using `Defs.Filter`.
    */
  private def projectSym(predSymExp: LoweredAst.Expr, datalogExp: LoweredAst.Expr, loc: SourceLocation)(implicit root: TypedAst.Root): LoweredAst.Expr = {
    val defn = Defs.lookup(Defs.Filter)
    val argExps = predSymExp :: datalogExp :: Nil
    val resultType = Types.Datalog
    val itpe = Types.FilterType
    LoweredAst.Expr.ApplyDef(defn.sym, argExps, List.empty, itpe, resultType, datalogExp.eff, loc)
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
      sym = Defs.Unbox,
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
    * The type of a channel which can transmit variables of type `tpe`.
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    mkChannelTpe(tpe, Type.IO, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe1` in region `tpe2`.
    */
  private def mkChannelTpe(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Apply(Types.ChannelMpmc, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns `(t1, t2)` where `tpe = Concurrent.Channel.Mpmc[t1, t2]`.
    */
  private def extractChannelTpe(tpe: Type): (Type, Type) = eraseAliases(tpe) match {
    case Type.Apply(Type.Apply(Types.ChannelMpmc, elmType, _), regionType, _) => (elmType, regionType)
    case _ => throw InternalCompilerException(s"Cannot interpret '$tpe' as a channel type", tpe.loc)
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
    * An expression for a channel variable called `sym`
    */
  private def mkChannelExp(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation): LoweredAst.Expr = {
    LoweredAst.Expr.Var(sym, mkChannelTpe(tpe, loc), loc)
  }

  /**
    * Returns a full `par yield` expression.
    */
  private def mkParChannels(exp: LoweredAst.Expr, chanSymsWithExps: List[(Symbol.VarSym, LoweredAst.Expr)]): LoweredAst.Expr = {
    // Make spawn expressions `spawn ch <- exp`.
    val spawns = chanSymsWithExps.foldRight(exp: LoweredAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val e1 = mkChannelExp(sym, e.tpe, loc) // The channel `ch`
        val e2 = mkPutChannel(e1, e, Type.IO, loc) // The put exp: `ch <- exp0`.
        val e3 = LoweredAst.Expr.Cst(Constant.Static, Type.mkRegionToStar(Type.IO, loc), loc)
        val e4 = LoweredAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e2, e3), Type.Unit, Type.IO, loc) // Spawn the put expression from above i.e. `spawn ch <- exp0`.
        LoweredAst.Expr.Stm(e4, acc, acc.tpe, Type.mkUnion(e4.eff, acc.eff, loc), loc) // Return a statement expression containing the other spawn expressions along with this one.
    }

    // Make let bindings `let ch = chan 1;`.
    chanSymsWithExps.foldRight(spawns: LoweredAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val chan = mkNewChannel(LoweredAst.Expr.Cst(Constant.Int32(1), Type.Int32, loc), mkChannelTpe(e.tpe, loc), Type.IO, loc) // The channel exp `chan 1`
        LoweredAst.Expr.Let(sym, chan, acc, acc.tpe, Type.mkUnion(e.eff, acc.eff, loc), loc) // The let-binding `let ch = chan 1`
    }
  }

  /**
    * Returns a desugared let-match expression, i.e.
    * {{{
    *   let pattern = exp;
    *   body
    * }}}
    * is desugared to
    * {{{
    *   match exp {
    *     case pattern => body
    *   }
    * }}}
    */
  private def mkLetMatch(pat: LoweredAst.Pattern, exp: LoweredAst.Expr, body: LoweredAst.Expr): LoweredAst.Expr = {
    val loc = exp.loc.asSynthetic
    val rule = List(LoweredAst.MatchRule(pat, None, body))
    val eff = Type.mkUnion(exp.eff, body.eff, loc)
    LoweredAst.Expr.Match(exp, rule, body.tpe, eff, loc)
  }

  /**
    * Returns an expression where the pattern variables used in `exp` are
    * bound to [[TypedAst.Expr.GetChannel]] expressions,
    * i.e.
    * {{{
    *   let pat1 = <- ch1;
    *   let pat2 = <- ch2;
    *   let pat3 = <- ch3;
    *   ...
    *   let patn = <- chn;
    *   exp
    * }}}
    */
  private def mkBoundParWaits(patSymExps: List[(LoweredAst.Pattern, Symbol.VarSym, LoweredAst.Expr)], exp: LoweredAst.Expr): LoweredAst.Expr =
    patSymExps.map {
      case (p, sym, e) =>
        val loc = e.loc.asSynthetic
        val chExp = mkChannelExp(sym, e.tpe, loc)
        (p, mkGetChannel(chExp, e.tpe, Type.IO, loc))
    }.foldRight(exp) {
      case ((pat, chan), e) => mkLetMatch(pat, chan, e)
    }

  /**
    * Returns a desugared [[TypedAst.Expr.ParYield]] expression as a nested match-expression.
    */
  private def mkParYield(frags: List[LoweredAst.ParYieldFragment], exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): LoweredAst.Expr = {
    // Only generate channels for n-1 fragments. We use the current thread for the last fragment.
    val fs = frags.init
    val last = frags.last

    // Generate symbols for each channel.
    val chanSymsWithPatAndExp = fs.map { case LoweredAst.ParYieldFragment(p, e, l) => (p, mkLetSym("channel", l.asSynthetic), e) }

    // Make `GetChannel` exps for the spawnable exps.
    val waitExps = mkBoundParWaits(chanSymsWithPatAndExp, exp)

    // Evaluate the last expression in the current thread (so just make let-binding)
    val desugaredYieldExp = mkLetMatch(last.pat, last.exp, waitExps)

    // Generate channels and spawn exps.
    val chanSymsWithExp = chanSymsWithPatAndExp.map { case (_, s, e) => (s, e) }
    val blockExp = mkParChannels(desugaredYieldExp, chanSymsWithExp)

    // Wrap everything in a purity cast,
    LoweredAst.Expr.Cast(blockExp, None, Some(Type.Pure), tpe, eff, loc.asSynthetic)
  }

  /**
    * Return a list of quantified variables in the given expression `exp0`.
    *
    * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
    * but not in the constraint params `cparams0` of the constraint.
    */
  private def quantifiedVars(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr): List[(Symbol.VarSym, Type)] = {
    TypedAstOps.freeVars(exp0).toList.filter {
      case (sym, _) => isQuantifiedVar(sym, cparams0)
    }
  }

  /**
    * Returns `true` if the given variable symbol `sym` is a quantified variable according to the given constraint params `cparams0`.
    *
    * That is, the variable symbol is *NOT* lexically bound.
    */
  private def isQuantifiedVar(sym: Symbol.VarSym, cparams0: List[TypedAst.ConstraintParam]): Boolean =
    cparams0.exists(p => p.bnd.sym == sym)

  /**
    * Applies the given substitution `subst` to the given expression `exp0`.
    */
  private def substExp(exp0: LoweredAst.Expr, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.Expr = exp0 match {
    case LoweredAst.Expr.Cst(_, _, _) => exp0

    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.Expr.Var(s, tpe, loc)

    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      LoweredAst.Expr.Lambda(p, e, tpe, loc)

    case LoweredAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.ApplyDef(sym, exps, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.ApplyDef(sym, es, targs, itpe, tpe, eff, loc)

    case LoweredAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case LoweredAst.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.ApplyOp(sym, es, tpe, eff, loc)

    case LoweredAst.Expr.ApplySig(sym, exps, targ, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.ApplySig(sym, es, targ, targs, itpe, tpe, eff, loc)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case LoweredAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.Let(s, e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val fps = fparams.map(substFormalParam(_, subst))
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.LocalDef(s, fps, e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.Region(sym, regionVar, exp, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e = substExp(exp, subst)
      LoweredAst.Expr.Region(s, regionVar, e, tpe, eff, loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      LoweredAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.Discard(e, eff, loc)

    case LoweredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.MatchRule(pat, guard, exp1) =>
          val p = substPattern(pat, subst)
          val g = guard.map(substExp(_, subst))
          val e1 = substExp(exp1, subst)
          LoweredAst.MatchRule(p, g, e1)
      }
      LoweredAst.Expr.Match(e, rs, tpe, eff, loc)

    case LoweredAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.ExtMatchRule(pat, exp1, loc1) =>
          val p = substExtPattern(pat, subst)
          val e1 = substExp(exp1, subst)
          LoweredAst.ExtMatchRule(p, e1, loc1)
      }
      LoweredAst.Expr.ExtMatch(e, rs, tpe, eff, loc)

    case LoweredAst.Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe1, exp1) =>
          val s = subst.getOrElse(sym, sym)
          val e1 = substExp(exp1, subst)
          LoweredAst.TypeMatchRule(s, tpe1, e1)
      }
      LoweredAst.Expr.TypeMatch(e, rs, tpe, eff, loc)

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.VectorLit(es, tpe, eff, loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.VectorLength(exp, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.VectorLength(e, loc)

    case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.Ascribe(e, tpe, eff, loc)

    case LoweredAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.CatchRule(sym, clazz, exp1) =>
          val s = subst.getOrElse(sym, sym)
          val e1 = substExp(exp1, subst)
          LoweredAst.CatchRule(s, clazz, e1)
      }
      LoweredAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case LoweredAst.Expr.RunWith(exp, effSymUse, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.HandlerRule(opSymUse, fparams, hexp) =>
          val fps = fparams.map(substFormalParam(_, subst))
          val he = substExp(hexp, subst)
          LoweredAst.HandlerRule(opSymUse, fps, he)
      }
      LoweredAst.Expr.RunWith(e, effSymUse, rs, tpe, eff, loc)

    case LoweredAst.Expr.NewObject(_, _, _, _, _, _) => exp0

    case Expr.NewChannel(_, _, _, loc) =>
      throw InternalCompilerException("not implemented yet", loc)

    case Expr.GetChannel(_, _, _, loc) =>
      throw InternalCompilerException("not implemented yet", loc)

    case Expr.PutChannel(_, _, _, _, loc) =>
      throw InternalCompilerException("not implemented yet", loc)

    case Expr.SelectChannel(_, _, _, _, loc) =>
      throw InternalCompilerException("not implemented yet", loc)

  }

  /**
    * Applies the given substitution `subst` to the given formal param `fparam0`.
    */
  private def substFormalParam(fparam0: LoweredAst.FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.FormalParam = fparam0 match {
    case LoweredAst.FormalParam(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.FormalParam(s, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given pattern `pattern0`.
    */
  private def substPattern(pattern0: LoweredAst.Pattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.Pattern = pattern0 match {
    case LoweredAst.Pattern.Wild(tpe, loc) =>
      LoweredAst.Pattern.Wild(tpe, loc)

    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.Pattern.Var(s, tpe, loc)

    case LoweredAst.Pattern.Cst(cst, tpe, loc) =>
      LoweredAst.Pattern.Cst(cst, tpe, loc)

    case LoweredAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(substPattern(_, subst))
      LoweredAst.Pattern.Tag(symUse, ps, tpe, loc)

    case LoweredAst.Pattern.Tuple(pats, tpe, loc) =>
      val ps = pats.map(substPattern(_, subst))
      LoweredAst.Pattern.Tuple(ps, tpe, loc)

    case LoweredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map(substRecordLabelPattern(_, subst))
      val p = substPattern(pat, subst)
      LoweredAst.Pattern.Record(ps, p, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given record label pattern `pattern0`.
    */
  private def substRecordLabelPattern(pattern0: LoweredAst.Pattern.Record.RecordLabelPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.Pattern.Record.RecordLabelPattern = pattern0 match {
    case LoweredAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val p = substPattern(pat, subst)
      LoweredAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given ext pattern `pattern0`.
    */
  private def substExtPattern(pattern0: LoweredAst.ExtPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.ExtPattern = pattern0 match {
    case LoweredAst.ExtPattern.Default(loc) =>
      LoweredAst.ExtPattern.Default(loc)

    case LoweredAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(substVarOrWild(_, subst))
      LoweredAst.ExtPattern.Tag(label, ps, loc)
  }

  /**
    * Applies the given substitution `subst` to the given ext tag pattern `pattern0`.
    */
  private def substVarOrWild(pattern0: LoweredAst.ExtTagPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.ExtTagPattern = pattern0 match {
    case LoweredAst.ExtTagPattern.Wild(tpe, loc) =>
      LoweredAst.ExtTagPattern.Wild(tpe, loc)

    case LoweredAst.ExtTagPattern.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.ExtTagPattern.Var(s, tpe, loc)

   case LoweredAst.ExtTagPattern.Unit(tpe, loc) =>
      LoweredAst.ExtTagPattern.Unit(tpe, loc)
  }

}
