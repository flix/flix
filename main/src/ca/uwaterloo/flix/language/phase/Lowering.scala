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
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.{AtomicOp, Kind, LoweredAst, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugLoweredAst
import ca.uwaterloo.flix.util.collection.Nel
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
    lazy val ChannelNew: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannel")
    lazy val ChannelNewTuple: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannelTuple")
    lazy val ChannelPut: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.put")
    lazy val ChannelGet: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.get")
    lazy val ChannelMpmcAdmin: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.mpmcAdmin")
    lazy val ChannelSelectFrom: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.selectFrom")
    lazy val ChannelUnsafeGetAndUnlock: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.unsafeGetAndUnlock")

  }

  private object Enums {
    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")

    lazy val ChannelMpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.Mpmc")
    lazy val ChannelMpmcAdmin: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.MpmcAdmin")

    lazy val ConcurrentReentrantLock: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.ReentrantLock")
  }

  private object Types {
    //
    // Data Types
    //
    lazy val ChannelMpmcAdmin: Type = Type.mkEnum(Enums.ChannelMpmcAdmin, Nil, SourceLocation.Unknown)
    lazy val ChannelMpmc: Type = Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Eff ->: Kind.Star), SourceLocation.Unknown)

    lazy val ConcurrentReentrantLock: Type = Type.mkEnum(Enums.ConcurrentReentrantLock, Nil, SourceLocation.Unknown)

    def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)

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
    new Symbol.EnumSym(sym.namespace, sym.name, sym.loc)

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
      val region = visitExp(region0)
      val (names0, es) = fields.unzip
      val names = names0.map(_.sym)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.StructNew(sym, names), region :: es, tpe, eff, loc)

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

    // New channel expressions are rewritten as follows:
    //     %%CHANNEL_NEW%%(m)
    // becomes a call to the standard library function:
    //     Concurrent/Channel.newChannel(10)
    //
    case TypedAst.Expr.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkNewChannelTuple(e, t, eff, loc)

    // Channel get expressions are rewritten as follows:
    //     <- c
    // becomes a call to the standard library function:
    //     Concurrent/Channel.get(c)
    //
    case TypedAst.Expr.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkGetChannel(e, t, eff, loc)

    // Channel put expressions are rewritten as follows:
    //     c <- 42
    // becomes a call to the standard library function:
    //     Concurrent/Channel.put(42, c)
    //
    case TypedAst.Expr.PutChannel(exp1, exp2, _, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      mkPutChannel(e1, e2, eff, loc)

    // Channel select expressions are rewritten as follows:
    //     select {
    //         case x <- ?ch1 => ?handlech1
    //         case y <- ?ch2 => ?handlech2
    //         case _ => ?default
    //     }
    // becomes:
    //     let ch1 = ?ch1;
    //     let ch2 = ?ch2;
    //     match selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: Nil, false) {  // true if no default
    //         case (0, locks) =>
    //             let x = unsafeGetAndUnlock(ch1, locks);
    //             ?handlech1
    //         case (1, locks) =>
    //             let y = unsafeGetAndUnlock(ch2, locks);
    //             ?handlech2
    //         case (-1, _) =>                                                  // Omitted if no default
    //             ?default                                                     // Unlock is handled by selectFrom
    //     }
    // Note: match is not exhaustive: we're relying on the simplifier to handle this for us
    //
    case TypedAst.Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val d = default.map(visitExp)
      val t = visitType(tpe)

      val channels = rs map { case LoweredAst.SelectChannelRule(_, c, _) => (mkLetSym("chan", loc), c) }
      val admins = mkChannelAdminList(rs, channels, loc)
      val selectExp = mkChannelSelect(admins, d, loc)
      val cases = mkChannelCases(rs, channels, eff, loc)
      val defaultCase = mkSelectDefaultCase(d, loc)
      val matchExp = LoweredAst.Expr.Match(selectExp, cases ++ defaultCase, t, eff, loc)

      channels.foldRight[LoweredAst.Expr](matchExp) {
        case ((sym, c), e) => LoweredAst.Expr.Let(sym, c, e, t, eff, loc)
      }

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

    case TypedAst.Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      val cs = cs0.map(visitConstraint)
      val t = visitType(tpe)
      LoweredAst.Expr.FixpointConstraintSet(cs, t, loc)

    case TypedAst.Expr.FixpointLambda(pparams0, exp, tpe, eff, loc) =>
      val pparams = pparams0.map {
        case TypedAst.PredicateParam(pred, tpe0, loc0) => LoweredAst.PredicateParam(pred, tpe0, loc0)
      }
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.FixpointLambda(pparams, e, t, eff, loc)

    case TypedAst.Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.FixpointMerge(e1, e2, t, eff, loc)

    case TypedAst.Expr.FixpointQueryWithProvenance(exps0, select0, withh, tpe, eff, loc) =>
      val exps = exps0.map(visitExp)
      val select = visitHeadPred(select0)
      LoweredAst.Expr.FixpointQueryWithProvenance(exps, select, withh, visitType(tpe), eff, loc)

    case TypedAst.Expr.FixpointQueryWithSelect(exps0, queryExp0, selects0, from0, where0, pred, tpe, eff, loc) =>
      val exps = exps0.map(visitExp)
      val queryExp = visitExp(queryExp0)
      val selects = selects0.map(visitExp)
      val t = visitType(tpe)
      val from = from0.map(visitBodyPred)
      val where = where0.map(visitExp)
      LoweredAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, t, eff, loc)

    case TypedAst.Expr.FixpointSolveWithProject(exps0, optPreds, mode, tpe, eff, loc) =>
      val exps = exps0.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.FixpointSolveWithProject(exps, optPreds, mode, t, eff, loc)

    case TypedAst.Expr.FixpointInjectInto(exps0, predsAndArities, tpe, eff, loc) =>
      val exps = exps0.map(visitExp)
      val t = visitType(tpe)
      LoweredAst.Expr.FixpointInjectInto(exps, predsAndArities, t, eff, loc)

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
    case Some(TypeConstructor.Schema) => tpe0
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
    * Lowers the given select channel rule `rule0`.
    */
  private def visitSelectChannelRule(rule0: TypedAst.SelectChannelRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.SelectChannelRule = rule0 match {
    case TypedAst.SelectChannelRule(TypedAst.Binder(sym, _), chan, exp, _) =>
      val c = visitExp(chan)
      val e = visitExp(exp)
      LoweredAst.SelectChannelRule(sym, c, e)
  }

  /**
    * Lowers the given constraint `c0`.
    */
  private def visitConstraint(c0: TypedAst.Constraint)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Constraint = c0 match {
    case TypedAst.Constraint(cparams0, head0, body0, loc0) =>
      val cparams = cparams0.map {
        case TypedAst.ConstraintParam(bnd, tpe, loc) =>
          LoweredAst.ConstraintParam(bnd.sym, visitType(tpe), loc)
      }
      val head = visitHeadPred(head0)
      val body = body0.map(visitBodyPred)
      LoweredAst.Constraint(cparams, head, body, loc0)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(p0: TypedAst.Predicate.Head)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Predicate.Head = p0 match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
      val t = visitType(tpe)
      val visitedTerms = terms.map(visitExp)
      LoweredAst.Predicate.Head.Atom(pred, den, visitedTerms, t, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(p0: TypedAst.Predicate.Body)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Predicate.Body = p0 match {
    case TypedAst.Predicate.Body.Atom(pred0, den, polarity, fixity, terms0, tpe, loc) =>
      val terms = terms0.map(visitPat)
      val t = visitType(tpe)
      LoweredAst.Predicate.Body.Atom(pred0, den, polarity, fixity, terms, t, loc)
    case TypedAst.Predicate.Body.Functional(outBnds, exp, loc) =>
      val outSyms = outBnds.map(_.sym)
      val e = visitExp(exp)
      LoweredAst.Predicate.Body.Functional(outSyms, e, loc)
    case TypedAst.Predicate.Body.Guard(exp, loc) =>
      LoweredAst.Predicate.Body.Guard(visitExp(exp), loc)

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
    * Make a new channel expression
    */
  private def mkNewChannel(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val (targ, _) = extractChannelTpe(tpe)
    LoweredAst.Expr.ApplyDef(Defs.ChannelNew, exp :: Nil, List(targ), itpe, tpe, eff, loc)
  }

  /**
    * Make a new channel tuple (sender, receiver) expression
    */
  private def mkNewChannelTuple(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val (targ, _) = extractChannelTpe(tpe.typeArguments.head) // TODO make helper
    LoweredAst.Expr.ApplyDef(Defs.ChannelNewTuple, exp :: Nil, List(targ), itpe, tpe, eff, loc)
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
    * Make the list of MpmcAdmin objects which will be passed to `selectFrom`
    */
  private def mkChannelAdminList(rs: List[LoweredAst.SelectChannelRule], channels: List[(Symbol.VarSym, LoweredAst.Expr)], loc: SourceLocation): LoweredAst.Expr = {
    val admins = rs.zip(channels) map {
      case (LoweredAst.SelectChannelRule(_, c, _), (chanSym, _)) =>
        val (targ, _) = extractChannelTpe(c.tpe)
        val itpe = Type.mkPureArrow(c.tpe, Types.ChannelMpmcAdmin, loc)
        LoweredAst.Expr.ApplyDef(Defs.ChannelMpmcAdmin, List(LoweredAst.Expr.Var(chanSym, c.tpe, loc)), List(targ), itpe, Types.ChannelMpmcAdmin, Type.Pure, loc)
    }
    mkList(admins, Types.ChannelMpmcAdmin, loc)
  }

  /**
    * Construct a call to `selectFrom` given a list of MpmcAdmin objects and optional default
    */
  private def mkChannelSelect(admins: LoweredAst.Expr, default: Option[LoweredAst.Expr], loc: SourceLocation): LoweredAst.Expr = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    val selectRetTpe = Type.mkTuple(List(Type.Int32, locksType), loc)
    val itpe = Type.mkIoUncurriedArrow(List(admins.tpe, Type.Bool), selectRetTpe, loc)
    val blocking = default match {
      case Some(_) => LoweredAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc)
      case None => LoweredAst.Expr.Cst(Constant.Bool(true), Type.Bool, loc)
    }
    LoweredAst.Expr.ApplyDef(Defs.ChannelSelectFrom, List(admins, blocking), List.empty, itpe, selectRetTpe, Type.IO, loc)
  }

  /**
    * Construct a sequence of MatchRules corresponding to the given SelectChannelRules
    */
  private def mkChannelCases(rs: List[LoweredAst.SelectChannelRule], channels: List[(Symbol.VarSym, LoweredAst.Expr)], eff: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): List[LoweredAst.MatchRule] = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    rs.zip(channels).zipWithIndex map {
      case ((LoweredAst.SelectChannelRule(sym, chan, exp), (chSym, _)), i) =>
        val locksSym = mkLetSym("locks", loc)
        val pat = mkTuplePattern(Nel(LoweredAst.Pattern.Cst(Constant.Int32(i), Type.Int32, loc), List(LoweredAst.Pattern.Var(locksSym, locksType, loc))), loc)
        val (getTpe, _) = extractChannelTpe(chan.tpe)
        val itpe = Type.mkIoUncurriedArrow(List(chan.tpe, locksType), getTpe, loc)
        val args = List(LoweredAst.Expr.Var(chSym, chan.tpe, loc), LoweredAst.Expr.Var(locksSym, locksType, loc))
        val getExp = LoweredAst.Expr.ApplyDef(Defs.ChannelUnsafeGetAndUnlock, args, List(getTpe), itpe, getTpe, eff, loc)
        val e = LoweredAst.Expr.Let(sym, getExp, exp, exp.tpe, eff, loc)
        LoweredAst.MatchRule(pat, None, e)
    }
  }

  /**
    * Construct additional MatchRule to handle the (optional) default case
    * NB: Does not need to unlock because that is handled inside Concurrent/Channel.selectFrom.
    */
  private def mkSelectDefaultCase(default: Option[LoweredAst.Expr], loc: SourceLocation): List[LoweredAst.MatchRule] = {
    default match {
      case Some(defaultExp) =>
        val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)
        val pat = mkTuplePattern(Nel(LoweredAst.Pattern.Cst(Constant.Int32(-1), Type.Int32, loc), List(LoweredAst.Pattern.Wild(locksType, loc))), loc)
        val defaultMatch = LoweredAst.MatchRule(pat, None, defaultExp)
        List(defaultMatch)
      case _ =>
        List()
    }
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
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit scope: Scope, flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)
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
    * Returns a TypedAst.Pattern representing a tuple of patterns.
    */
  private def mkTuplePattern(patterns: Nel[LoweredAst.Pattern], loc: SourceLocation): LoweredAst.Pattern = {
    LoweredAst.Pattern.Tuple(patterns, Type.mkTuple(patterns.map(_.tpe), loc), loc)
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

}
