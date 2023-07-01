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
import ca.uwaterloo.flix.language.ast.Ast.Denotation.{Latticenal, Relational}
import ca.uwaterloo.flix.language.ast.Ast._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, Kind, LoweredAst, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

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

object Lowering {

  private object Defs {
    lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym("Boxable.box")

    lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.solve")
    lazy val Merge: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.union")
    lazy val Filter: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.project")
    lazy val Rename: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.rename")

    def ProjectInto(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint.injectInto$arity")

    def Facts(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint.facts$arity")

    lazy val DebugWithPrefix: Symbol.DefnSym = Symbol.mkDefnSym("Debug.debugWithPrefix")

    lazy val ChannelNew: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.newChannel")
    lazy val ChannelNewTuple: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.newChannelTuple")
    lazy val ChannelPut: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.put")
    lazy val ChannelGet: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.get")
    lazy val ChannelMpmcAdmin: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.mpmcAdmin")
    lazy val ChannelSelectFrom: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.selectFrom")
    lazy val ChannelUnsafeGetAndUnlock: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent/Channel.unsafeGetAndUnlock")

    /**
      * Returns the definition associated with the given symbol `sym`.
      */
    def lookup(sym: Symbol.DefnSym)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Def = root.defs.get(sym) match {
      case None => throw InternalCompilerException(s"Symbol '$sym' not found. Missing library?", sym.loc)
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

    lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Shared.PredSym")
    lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.VarSym")

    lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Denotation")
    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
    lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Fixity")
    lazy val SourceLocation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.SourceLocation")

    lazy val Comparison: Symbol.EnumSym = Symbol.mkEnumSym("Comparison")
    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym("Boxed")

    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")

    lazy val ChannelMpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent/Channel.Mpmc")
    lazy val ChannelMpmcAdmin: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent/Channel.MpmcAdmin")

    lazy val ConcurrentReentrantLock: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.ReentrantLock")
  }

  private object Types {
    //
    // Data Types
    //
    lazy val Datalog: Type = Type.mkEnum(Enums.Datalog, Boxed :: Nil, SourceLocation.Unknown)
    lazy val Constraint: Type = Type.mkEnum(Enums.Constraint, Boxed :: Nil, SourceLocation.Unknown)

    lazy val HeadPredicate: Type = Type.mkEnum(Enums.HeadPredicate, Boxed :: Nil, SourceLocation.Unknown)
    lazy val BodyPredicate: Type = Type.mkEnum(Enums.BodyPredicate, Boxed :: Nil, SourceLocation.Unknown)

    lazy val HeadTerm: Type = Type.mkEnum(Enums.HeadTerm, Boxed :: Nil, SourceLocation.Unknown)
    lazy val BodyTerm: Type = Type.mkEnum(Enums.BodyTerm, Boxed :: Nil, SourceLocation.Unknown)

    lazy val PredSym: Type = Type.mkEnum(Enums.PredSym, Nil, SourceLocation.Unknown)
    lazy val VarSym: Type = Type.mkEnum(Enums.VarSym, Nil, SourceLocation.Unknown)

    lazy val Denotation: Type = Type.mkEnum(Enums.Denotation, Boxed :: Nil, SourceLocation.Unknown)
    lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil, SourceLocation.Unknown)
    lazy val Fixity: Type = Type.mkEnum(Enums.Fixity, Nil, SourceLocation.Unknown)
    lazy val SL: Type = Type.mkEnum(Enums.SourceLocation, Nil, SourceLocation.Unknown)

    lazy val Comparison: Type = Type.mkEnum(Enums.Comparison, Nil, SourceLocation.Unknown)
    lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil, SourceLocation.Unknown)

    lazy val ChannelMpmcAdmin: Type = Type.mkEnum(Enums.ChannelMpmcAdmin, Nil, SourceLocation.Unknown)

    lazy val ConcurrentReentrantLock: Type = Type.mkEnum(Enums.ConcurrentReentrantLock, Nil, SourceLocation.Unknown)

    def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)

    //
    // Function Types.
    //
    lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog, SourceLocation.Unknown)
    lazy val MergeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog, SourceLocation.Unknown)
    lazy val FilterType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog, SourceLocation.Unknown)
    lazy val RenameType: Type = Type.mkPureUncurriedArrow(List(mkList(PredSym, SourceLocation.Unknown), Datalog), Datalog, SourceLocation.Unknown)
  }

  /**
    * Translates internal Datalog constraints into Flix Datalog constraints.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("Lowering") {
    val defs = ParOps.parMap(root.defs.values)((d: TypedAst.Def) => visitDef(d)(root, flix))
    val sigs = ParOps.parMap(root.sigs.values)((s: TypedAst.Sig) => visitSig(s)(root, flix))
    val instances = ParOps.parMap(root.instances.values)((insts: List[TypedAst.Instance]) => insts.map(i => visitInstance(i)(root, flix)))
    val enums = ParOps.parMap(root.enums.values)((e: TypedAst.Enum) => visitEnum(e)(root, flix))
    val restrictableEnums = ParOps.parMap(root.restrictableEnums.values)((e: TypedAst.RestrictableEnum) => visitRestrictableEnum(e)(root, flix))
    val effects = ParOps.parMap(root.effects.values)((e: TypedAst.Effect) => visitEffect(e)(root, flix))
    val aliases = ParOps.parMap(root.typeAliases.values)((a: TypedAst.TypeAlias) => visitTypeAlias(a)(root, flix))

    val newDefs = defs.map(kv => kv.sym -> kv).toMap
    val newSigs = sigs.map(kv => kv.sym -> kv).toMap
    val newInstances = instances.map(kv => kv.head.clazz.sym -> kv).toMap
    val newEnums = (enums ++ restrictableEnums).map(kv => kv.sym -> kv).toMap
    val newEffects = effects.map(kv => kv.sym -> kv).toMap
    val newAliases = aliases.map(kv => kv.sym -> kv).toMap

    // TypedAst.Sigs are shared between the `sigs` field and the `classes` field.
    // Instead of visiting twice, we visit the `sigs` field and then look up the results when visiting classes.
    val classes = ParOps.parMap(root.classes.values)((c: TypedAst.Class) => visitClass(c, newSigs)(root, flix))
    val newClasses = classes.map(kv => kv.sym -> kv).toMap

    LoweredAst.Root(newClasses, newInstances, newSigs, newDefs, newEnums, newEffects, newAliases, root.entryPoint, root.sources, root.classEnv, root.eqEnv)
  }

  /**
    * Lowers the given definition `defn0`.
    */
  private def visitDef(defn0: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Def = defn0 match {
    case TypedAst.Def(sym, spec0, impl0) =>
      val spec = visitSpec(spec0)
      val impl = visitImpl(impl0)
      LoweredAst.Def(sym, spec, impl)
  }

  /**
    * Lowers the given signature `sig0`.
    */
  private def visitSig(sig0: TypedAst.Sig)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Sig = sig0 match {
    case TypedAst.Sig(sym, spec0, impl0) =>
      val spec = visitSpec(spec0)
      val impl = impl0.map(visitImpl)
      LoweredAst.Sig(sym, spec, impl)
  }

  /**
    * Lowers the given instance `inst0`.
    */
  private def visitInstance(inst0: TypedAst.Instance)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Instance = inst0 match {
    case TypedAst.Instance(doc, ann, mod, sym, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      val assocs = assocs0.map {
        case TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) => LoweredAst.AssocTypeDef(doc, mod, sym, args, tpe, loc)
      }
      val defs = defs0.map(visitDef)
      LoweredAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs, defs, ns, loc)
  }

  /**
    * Lowers the given enum `enum0`.
    */
  private def visitEnum(enum0: TypedAst.Enum)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Enum = enum0 match {
    case TypedAst.Enum(doc, ann, mod, sym, tparams0, derives, cases0, tpe0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      val cases = cases0.map {
        case (_, TypedAst.Case(caseSym, caseTpeDeprecated0, caseSc0, loc)) =>
          val caseTpeDeprecated = visitType(caseTpeDeprecated0)
          val caseSc = visitScheme(caseSc0)
          (caseSym, LoweredAst.Case(caseSym, caseTpeDeprecated, caseSc, loc))
      }
      LoweredAst.Enum(doc, ann, mod, sym, tparams, derives, cases, tpe, loc)
  }

  /**
    * Lowers the given enum `enum0` from a restrictable enum into a regular enum.
    */
  private def visitRestrictableEnum(enum0: TypedAst.RestrictableEnum)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Enum = enum0 match {
    case TypedAst.RestrictableEnum(doc, ann, mod, sym0, index0, tparams0, derives, cases0, tpe0, loc) =>
      // index is erased since related checking has concluded.
      // Restrictable tag is lowered into a regular tag
      val index = visitTypeParam(index0)
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      val cases = cases0.map {
        case (_, TypedAst.RestrictableCase(caseSym0, caseTpeDeprecated0, caseSc0, loc)) =>
          val caseTpeDeprecated = visitType(caseTpeDeprecated0)
          val caseSc = visitScheme(caseSc0)
          val caseSym = visitRestrictableCaseSym(caseSym0)
          (caseSym, LoweredAst.Case(caseSym, caseTpeDeprecated, caseSc, loc))
      }
      val sym = visitRestrictableEnumSym(sym0)
      LoweredAst.Enum(doc, ann, mod, sym, index :: tparams, derives, cases, tpe, loc)
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
  private def visitRestrictableCaseSymUse(sym: Ast.RestrictableCaseSymUse): Ast.CaseSymUse = {
    Ast.CaseSymUse(visitRestrictableCaseSym(sym.sym), sym.sym.loc)
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
    case TypedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp)
      LoweredAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Lowers the given `op`.
    */
  private def visitOp(op: TypedAst.Op)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Op = op match {
    case TypedAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0)
      LoweredAst.Op(sym, spec)
  }

  /**
    * Lowers the given type `alias`.
    */
  private def visitTypeAlias(alias: TypedAst.TypeAlias)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.TypeAlias = alias match {
    case TypedAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      LoweredAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
  }

  /**
    * Lowers the given type constraint `tconstr0`.
    */
  private def visitTypeConstraint(tconstr0: Ast.TypeConstraint)(implicit root: TypedAst.Root, flix: Flix): Ast.TypeConstraint = tconstr0 match {
    case Ast.TypeConstraint(head, tpe0, loc) =>
      val tpe = visitType(tpe0)
      Ast.TypeConstraint(head, tpe, loc)
  }

  /**
    * Lowers the given class `clazz0`, with the given lowered sigs `sigs`.
    */
  private def visitClass(clazz0: TypedAst.Class, sigs: Map[Symbol.SigSym, LoweredAst.Sig])(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Class = clazz0 match {
    case TypedAst.Class(doc, ann, mod, sym, tparam0, superClasses0, assocs0, signatures0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superClasses = superClasses0.map(visitTypeConstraint)
      val assocs = assocs0.map {
        case TypedAst.AssocTypeSig(doc, mod, sym, tparam, kind, loc) => LoweredAst.AssocTypeSig(doc, mod, sym, tparam, kind, loc)
      }
      val signatures = signatures0.map(sig => sigs(sig.sym))
      val laws = laws0.map(visitDef)
      LoweredAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, signatures, laws, loc)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def visitSpec(spec0: TypedAst.Spec)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Spec = spec0 match {
    case TypedAst.Spec(doc, ann, mod, tparams0, fparams, declaredScheme, retTpe, eff, tconstrs, loc) =>
      val tparam = tparams0.map(visitTypeParam)
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      LoweredAst.Spec(doc, ann, mod, tparam, fs, ds, retTpe, eff, tconstrs, loc)
  }

  /**
    * Lowers the given `impl0`.
    */
  private def visitImpl(impl0: TypedAst.Impl)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Impl = impl0 match {
    case TypedAst.Impl(exp, inferredScheme) =>
      val e = visitExp(exp)
      val s = visitScheme(inferredScheme)
      LoweredAst.Impl(e, s)
  }

  /**
    * Lowers the given `tparam`.
    */
  private def visitTypeParam(tparam: TypedAst.TypeParam)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.TypeParam = tparam match {
    case TypedAst.TypeParam(name, sym, loc) => LoweredAst.TypeParam(name, sym, loc)
  }

  /**
    * Lowers the given expression `exp0`.
    */
  private def visitExp(exp0: TypedAst.Expression)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = exp0 match {
    case TypedAst.Expression.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.Cst(cst, t, loc)

    case TypedAst.Expression.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.Var(sym, t, loc)

    case TypedAst.Expression.Def(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.Def(sym, t, loc)

    case TypedAst.Expression.Sig(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.Sig(sym, t, loc)

    case TypedAst.Expression.Hole(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.Hole(sym, t, loc)

    case TypedAst.Expression.HoleWithExp(exp, tpe, eff, loc) =>
      val sym = Symbol.freshHoleSym(loc)
      val t = visitType(tpe)
      LoweredAst.Expression.Hole(sym, t, loc)

    case TypedAst.Expression.OpenAs(sym, exp, tpe, loc) =>
      visitExp(exp) // TODO RESTR-VARS maybe add to loweredAST

    case TypedAst.Expression.Use(_, _, exp, _) =>
      visitExp(exp)

    case TypedAst.Expression.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Lambda(p, e, t, loc)

    case TypedAst.Expression.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expression.Apply(e, es, t, eff, loc)

    case TypedAst.Expression.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.ApplyAtomic(AtomicOp.Unary(sop), List(e), t, eff, loc)

    case TypedAst.Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), t, eff, loc)

    case TypedAst.Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.Let(sym, mod, e1, e2, t, eff, loc)

    case TypedAst.Expression.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.LetRec(sym, mod, e1, e2, t, eff, loc)

    case TypedAst.Expression.Region(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.Region(t, loc)

    case TypedAst.Expression.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Scope(sym, regionVar, e, t, eff, loc)

    case TypedAst.Expression.ScopeExit(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.ScopeExit(e1, e2, t, eff, loc)

    case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst.Expression.IfThenElse(e1, e2, e3, t, eff, loc)

    case TypedAst.Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.Stm(e1, e2, t, eff, loc)

    case TypedAst.Expression.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expression.Discard(e, eff, loc)

    case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe)
      LoweredAst.Expression.Match(e, rs, t, eff, loc)

    case TypedAst.Expression.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchTypeRule)
      val t = visitType(tpe)
      LoweredAst.Expression.TypeMatch(e, rs, t, eff, loc)

    case TypedAst.Expression.RelationalChoose(exps, rules, tpe, eff, loc) =>
      val es = visitExps(exps)
      val rs = rules.map(visitRelationalChooseRule)
      val t = visitType(tpe)
      LoweredAst.Expression.RelationalChoose(es, rs, t, eff, loc)

    case TypedAst.Expression.RestrictableChoose(_, exp, rules, tpe, eff, loc) =>
      // lower into an ordinary match
      val e = visitExp(exp)
      val rs = rules.map(visitRestrictableChooseRule)
      val t = visitType(tpe)
      LoweredAst.Expression.Match(e, rs, t, eff, loc)

    case TypedAst.Expression.Tag(sym, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Tag(sym, e, t, eff, loc)

    case TypedAst.Expression.RestrictableTag(sym0, exp, tpe, eff, loc) =>
      // Lower a restrictable tag into a normal tag.
      val caseSym = visitRestrictableCaseSym(sym0.sym)
      val sym = CaseSymUse(caseSym, sym0.loc)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Tag(sym, e, t, eff, loc)

    case TypedAst.Expression.Tuple(elms, tpe, eff, loc) =>
      val es = visitExps(elms)
      val t = visitType(tpe)
      LoweredAst.Expression.Tuple(es, t, eff, loc)

    case TypedAst.Expression.RecordEmpty(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.RecordEmpty(t, loc)

    case TypedAst.Expression.RecordSelect(exp, field, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.RecordSelect(e, field, t, eff, loc)

    case TypedAst.Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val t = visitType(tpe)
      LoweredAst.Expression.RecordExtend(field, v, r, t, eff, loc)

    case TypedAst.Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      val r = visitExp(rest)
      val t = visitType(tpe)
      LoweredAst.Expression.RecordRestrict(field, r, t, eff, loc)

    case TypedAst.Expression.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = visitExps(exps)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.ArrayLit(es, e, t, eff, loc)

    case TypedAst.Expression.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst.Expression.ArrayNew(e1, e2, e3, t, eff, loc)

    case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      LoweredAst.Expression.ArrayLoad(b, i, t, eff, loc)

    case TypedAst.Expression.ArrayLength(base, eff, loc) =>
      val b = visitExp(base)
      LoweredAst.Expression.ArrayLength(b, eff, loc)

    case TypedAst.Expression.ArrayStore(base, index, elm, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val e = visitExp(elm)
      LoweredAst.Expression.ArrayStore(b, i, e, eff, loc)

    case TypedAst.Expression.VectorLit(exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expression.VectorLit(es, t, eff, loc)

    case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      LoweredAst.Expression.VectorLoad(b, i, t, eff, loc)

    case TypedAst.Expression.VectorLength(base, loc) =>
      val b = visitExp(base)
      LoweredAst.Expression.VectorLength(b, loc)

    case TypedAst.Expression.Ref(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.Ref(e1, e2, t, eff, loc)

    case TypedAst.Expression.Deref(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Deref(e, t, eff, loc)

    case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.Assign(e1, e2, t, eff, loc)

    case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Ascribe(e, t, eff, loc)

    case TypedAst.Expression.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expression.InstanceOf(e, clazz, loc)

    case TypedAst.Expression.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case TypedAst.Expression.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(visitType)
      val t = visitType(tpe)
      LoweredAst.Expression.Cast(e, dt, declaredEff, t, eff, loc)

    case TypedAst.Expression.UncheckedMaskingCast(exp, _, _, _) =>
      visitExp(exp)

    case TypedAst.Expression.Without(exp, sym, tpe, eff, loc) =>
      visitExp(exp)

    case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe)
      LoweredAst.Expression.TryCatch(e, rs, t, eff, loc)

    case TypedAst.Expression.TryWith(exp, sym, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitHandlerRule)
      val t = visitType(tpe)
      LoweredAst.Expression.TryWith(e, sym, rs, t, eff, loc)

    case TypedAst.Expression.Do(sym, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      LoweredAst.Expression.Do(sym, es, tpe, eff, loc)

    case TypedAst.Expression.Resume(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Resume(e, t, loc)

    case TypedAst.Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      val as = visitExps(args)
      val t = visitType(tpe)
      LoweredAst.Expression.InvokeConstructor(constructor, as, t, eff, loc)

    case TypedAst.Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      val e = visitExp(exp)
      val as = visitExps(args)
      val t = visitType(tpe)
      LoweredAst.Expression.InvokeMethod(method, e, as, t, eff, loc)

    case TypedAst.Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      val as = visitExps(args)
      val t = visitType(tpe)
      LoweredAst.Expression.InvokeStaticMethod(method, as, t, eff, loc)

    case TypedAst.Expression.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.GetField(field, e, t, eff, loc)

    case TypedAst.Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.PutField(field, e1, e2, t, eff, loc)

    case TypedAst.Expression.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expression.GetStaticField(field, t, eff, loc)

    case TypedAst.Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.PutStaticField(field, e, t, eff, loc)

    case TypedAst.Expression.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod)
      LoweredAst.Expression.NewObject(name, clazz, t, eff, ms, loc)

    // New channel expressions are rewritten as follows:
    //     chan Int32 10
    // becomes a call to the standard library function:
    //     Concurrent/Channel.newChannel(10)
    //
    case TypedAst.Expression.NewChannel(_, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkNewChannelTuple(e, t, eff, loc)

    // Channel get expressions are rewritten as follows:
    //     <- c
    // becomes a call to the standard library function:
    //     Concurrent/Channel.get(c)
    //
    case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkGetChannel(e, t, eff, loc)

    // Channel put expressions are rewritten as follows:
    //     c <- 42
    // becomes a call to the standard library function:
    //     Concurrent/Channel.put(42, c)
    //
    case TypedAst.Expression.PutChannel(exp1, exp2, _, eff, loc) =>
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
    case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val d = default.map(visitExp)
      val t = visitType(tpe)

      val channels = rs map { case LoweredAst.SelectChannelRule(_, c, _) => (mkLetSym("chan", loc), c) }
      val admins = mkChannelAdminList(rs, channels, loc)
      val selectExp = mkChannelSelect(admins, d, loc)
      val cases = mkChannelCases(rs, channels, eff, loc)
      val defaultCase = mkSelectDefaultCase(d, t, loc)
      val matchExp = LoweredAst.Expression.Match(selectExp, cases ++ defaultCase, t, eff, loc)

      channels.foldRight[LoweredAst.Expression](matchExp) {
        case ((sym, c), e) => LoweredAst.Expression.Let(sym, Modifiers.Empty, c, e, t, eff, loc)
      }

    case TypedAst.Expression.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expression.Spawn(e1, e2, t, eff, loc)

    case TypedAst.Expression.ParYield(frags, exp, tpe, eff, loc) =>
      val fs = frags.map {
        case TypedAst.ParYieldFragment(pat, e, loc) => LoweredAst.ParYieldFragment(visitPat(pat), visitExp(e), loc)
      }
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkParYield(fs, e, t, eff, loc)

    case TypedAst.Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Lazy(e, t, loc)

    case TypedAst.Expression.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expression.Force(e, t, eff, loc)

    case TypedAst.Expression.FixpointConstraintSet(cs, _, _, loc) =>
      mkDatalog(cs, loc)

    case TypedAst.Expression.FixpointLambda(pparams, exp, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Rename)
      val defExp = LoweredAst.Expression.Def(defn.sym, Types.RenameType, loc)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.mkList(Types.PredSym, loc), loc)
      val argExps = predExps :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expression.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expression.FixpointMerge(exp1, exp2, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Merge)
      val defExp = LoweredAst.Expression.Def(defn.sym, Types.MergeType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expression.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expression.FixpointSolve(exp, _, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Solve)
      val defExp = LoweredAst.Expression.Def(defn.sym, Types.SolveType, loc)
      val argExps = visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expression.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expression.FixpointFilter(pred, exp, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Filter)
      val defExp = LoweredAst.Expression.Def(defn.sym, Types.FilterType, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expression.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expression.FixpointInject(exp, pred, _, eff, loc) =>
      // Compute the arity of the functor F[(a, b, c)] or F[a].
      val arity = Type.eraseAliases(exp.tpe) match {
        case Type.Apply(_, innerType, _) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(l)) => l
          case Some(TypeConstructor.Unit) => 0
          case _ => 1
        }
        case _ => throw InternalCompilerException(s"Unexpected non-foldable type: '${exp.tpe}'.", loc)
      }

      // Compute the symbol of the function.
      val sym = Defs.ProjectInto(arity)

      // The type of the function.
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, exp.tpe), Types.Datalog, loc)

      // Put everything together.
      val defExp = LoweredAst.Expression.Def(sym, defTpe, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      LoweredAst.Expression.Apply(defExp, argExps, Types.Datalog, eff, loc)

    case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
      // Compute the arity of the predicate symbol.
      // The type is either of the form `Array[(a, b, c)]` or `Array[a]`.
      val arity = Type.eraseAliases(tpe) match {
        case Type.Apply(Type.Cst(_, _), innerType, _) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(_)) => innerType.typeArguments.length
          case Some(TypeConstructor.Unit) => 0
          case _ => 1
        }
        case _ => throw InternalCompilerException(s"Unexpected non-list type: '$tpe'.", loc)
      }

      // Compute the symbol of the function.
      val sym = Defs.Facts(arity)

      // The type of the function.
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), tpe, loc)

      // Put everything together.
      val defExp = LoweredAst.Expression.Def(sym, defTpe, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      LoweredAst.Expression.Apply(defExp, argExps, tpe, eff, loc)

    case TypedAst.Expression.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)

  }

  /**
    * Lowers the given list of expressions `exps0`.
    */
  private def visitExps(exps0: List[TypedAst.Expression])(implicit root: TypedAst.Root, flix: Flix): List[LoweredAst.Expression] = exps0.map(visitExp)

  /**
    * Lowers the given pattern `pat0`.
    */
  private def visitPat(pat0: TypedAst.Pattern)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Pattern = pat0 match {
    case TypedAst.Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Pattern.Wild(t, loc)

    case TypedAst.Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Pattern.Var(sym, t, loc)

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      LoweredAst.Pattern.Cst(cst, tpe, loc)

    case TypedAst.Pattern.Tag(sym, pat, tpe, loc) =>
      val p = visitPat(pat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Tag(sym, p, t, loc)

    case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Tuple(es, t, loc)
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
  private def visitType(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = {
    def visit(tpe: Type): Type = tpe match {
      case Type.Var(sym, loc) => sym.kind match {
        case Kind.SchemaRow => Type.Var(sym.withKind(Kind.Star), loc)
        case _ => tpe
      }

      // Special case for Sender[t, _] and Receiver[t, _], both of which are rewritten to Concurrent/Channel.Mpmc[t]
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe, _), _, _) =>
        val t = visitType(tpe)
        Type.Apply(Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Star), loc), t, loc)

      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Receiver, loc), tpe, _), _, _) =>
        val t = visitType(tpe)
        Type.Apply(Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Star), loc), t, loc)

      case Type.Cst(_, _) => tpe

      case Type.Apply(tpe1, tpe2, loc) =>
        val t1 = visitType(tpe1)
        val t2 = visitType(tpe2)
        Type.Apply(t1, t2, loc)

      case Type.Alias(sym, args, t, loc) => Type.Alias(sym, args.map(visit), visit(t), loc)

      case Type.AssocType(cst, args, kind, loc) =>
        Type.AssocType(cst, args.map(visit), kind, loc) // TODO ASSOC-TYPES can't put lowered stuff on right side of assoc type def...
    }

    if (tpe0.typeConstructor.contains(TypeConstructor.Schema))
      Types.Datalog
    else
      visit(tpe0)
  }

  /**
    * Lowers the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: TypedAst.FormalParam)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.FormalParam = fparam0 match {
    case TypedAst.FormalParam(sym, mod, tpe, src, loc) =>
      val t = visitType(tpe)
      LoweredAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Lowers the given relational choice rule `rule0`.
    */
  private def visitRelationalChooseRule(rule0: TypedAst.RelationalChooseRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.RelationalChooseRule = rule0 match {
    case TypedAst.RelationalChooseRule(pat, exp) =>
      val p = pat.map {
        case TypedAst.RelationalChoosePattern.Wild(loc) => LoweredAst.RelationalChoosePattern.Wild(loc)
        case TypedAst.RelationalChoosePattern.Absent(loc) => LoweredAst.RelationalChoosePattern.Absent(loc)
        case TypedAst.RelationalChoosePattern.Present(sym, tpe, loc) =>
          val t = visitType(tpe)
          LoweredAst.RelationalChoosePattern.Present(sym, t, loc)
      }
      val e = visitExp(exp)
      LoweredAst.RelationalChooseRule(p, e)
  }

  /**
    * Lowers the given restrictable choice rule `rule0` to a match rule.
    */
  private def visitRestrictableChooseRule(rule0: TypedAst.RestrictableChooseRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.MatchRule = rule0 match {
    case TypedAst.RestrictableChooseRule(pat, exp) =>
      val e = visitExp(exp)
      pat match {
        case TypedAst.RestrictableChoosePattern.Tag(sym, pat0, tpe, loc) =>
          val termPatterns = pat0.map {
            case TypedAst.RestrictableChoosePattern.Var(sym, tpe, loc) => LoweredAst.Pattern.Var(sym, tpe, loc)
            case TypedAst.RestrictableChoosePattern.Wild(tpe, loc) => LoweredAst.Pattern.Wild(tpe, loc)
          }
          val pat1 = termPatterns match {
            case Nil => LoweredAst.Pattern.Cst(Constant.Unit, Type.mkUnit(loc), loc)
            case singular :: Nil => singular
            case _ => LoweredAst.Pattern.Tuple(termPatterns, Type.mkTuple(termPatterns.map(_.tpe), loc.asSynthetic), loc.asSynthetic)
          }
          val tagSym = visitRestrictableCaseSymUse(sym)
          val p = LoweredAst.Pattern.Tag(tagSym, pat1, tpe, loc)
          LoweredAst.MatchRule(p, None, e)
      }
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def visitCatchRule(rule0: TypedAst.CatchRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.CatchRule = rule0 match {
    case TypedAst.CatchRule(sym, clazz, exp) =>
      val e = visitExp(exp)
      LoweredAst.CatchRule(sym, clazz, e)
  }

  /**
    * Lowers the given handler rule `rule0`.
    */
  private def visitHandlerRule(rule0: TypedAst.HandlerRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.HandlerRule = rule0 match {
    case TypedAst.HandlerRule(sym, fparams0, exp) =>
      val fparams = fparams0.map(visitFormalParam)
      val e = visitExp(exp)
      LoweredAst.HandlerRule(sym, fparams, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: TypedAst.MatchRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.MatchRule = rule0 match {
    case TypedAst.MatchRule(pat, guard, exp) =>
      val p = visitPat(pat)
      val g = guard.map(visitExp)
      val e = visitExp(exp)
      LoweredAst.MatchRule(p, g, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchTypeRule(rule0: TypedAst.TypeMatchRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.TypeMatchRule = rule0 match {
    case TypedAst.TypeMatchRule(sym, tpe, exp) =>
      val e = visitExp(exp)
      LoweredAst.TypeMatchRule(sym, tpe, e)
  }

  /**
    * Lowers the given select channel rule `rule0`.
    */
  private def visitSelectChannelRule(rule0: TypedAst.SelectChannelRule)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.SelectChannelRule = rule0 match {
    case TypedAst.SelectChannelRule(sym, chan, exp) =>
      val c = visitExp(chan)
      val e = visitExp(exp)
      LoweredAst.SelectChannelRule(sym, c, e)
  }

  /**
    * Constructs a `Fixpoint/Ast.Datalog` value from the given list of Datalog constraints `cs`.
    */
  private def mkDatalog(cs: List[TypedAst.Constraint], loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    val factExps = cs.filter(c => c.body.isEmpty).map(visitConstraint)
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(visitConstraint)

    val factListExp = mkVector(factExps, Types.Constraint, loc)
    val ruleListExp = mkVector(ruleExps, Types.Constraint, loc)

    val innerExp = mkTuple(List(factListExp, ruleListExp), loc)
    mkTag(Enums.Datalog, "Datalog", innerExp, Types.Datalog, loc)
  }

  /**
    * Lowers the given constraint `c0`.
    */
  private def visitConstraint(c0: TypedAst.Constraint)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = c0 match {
    case TypedAst.Constraint(cparams, head, body, loc) =>
      val headExp = visitHeadPred(cparams, head)
      val bodyExp = mkVector(body.map(visitBodyPred(cparams, _)), Types.BodyPredicate, loc)
      val innerExp = mkTuple(headExp :: bodyExp :: Nil, loc)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Head)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = p0 match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val termsExp = mkVector(terms.map(visitHeadTerm(cparams0, _)), Types.HeadTerm, loc)
      val innerExp = mkTuple(predSymExp :: denotationExp :: termsExp :: Nil, loc)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Body)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = p0 match {
    case TypedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val fixityExp = mkFixity(fixity, loc)
      val termsExp = mkVector(terms.map(visitBodyTerm(cparams0, _)), Types.BodyTerm, loc)
      val innerExp = mkTuple(predSymExp :: denotationExp :: polarityExp :: fixityExp :: termsExp :: Nil, loc)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case TypedAst.Predicate.Body.Functional(outVars, exp0, loc) =>
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
  private def visitHeadTerm(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expression)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    //
    // We need to consider four cases:
    //
    // Case 1.1: The expression is quantified variable. We translate it to a Var.
    // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case TypedAst.Expression.Var(sym, _, _) =>
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
  private def visitBodyTerm(cparams0: List[TypedAst.ConstraintParam], pat0: TypedAst.Pattern)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = pat0 match {
    case TypedAst.Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case TypedAst.Pattern.Var(sym, tpe, loc) =>
      if (isQuantifiedVar(sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(sym)
      } else {
        // Case 2: Lexically bound variable *expression*.
        mkBodyTermLit(box(LoweredAst.Expression.Var(sym, tpe, loc)))
      }

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      mkBodyTermLit(box(LoweredAst.Expression.Cst(cst, tpe, loc)))

    case TypedAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

  }

  /**
    * Lowers the given JvmMethod `method`.
    */
  private def visitJvmMethod(method: TypedAst.JvmMethod)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, retTyp, eff, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      val t = visitType(retTyp)
      LoweredAst.JvmMethod(ident, fs, e, t, eff, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    val innerExp = mkVarSym(sym)
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: LoweredAst.Expression)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    mkTag(Enums.HeadTerm, "Lit", exp, Types.HeadTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation): LoweredAst.Expression = {
    val innerExp = LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)
    mkTag(Enums.BodyTerm, "Wild", innerExp, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym): LoweredAst.Expression = {
    val innerExp = mkVarSym(sym)
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: LoweredAst.Expression)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    mkTag(Enums.BodyTerm, "Lit", exp, Types.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = d match {
    case Relational =>
      val innerExp = LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Denotation, "Relational", innerExp, Types.Denotation, loc)

    case Latticenal =>
      tpeOpt match {
        case None => throw InternalCompilerException("Unexpected nullary lattice predicate.", loc)
        case Some(tpe) =>
          // The type `Denotation[tpe]`.
          val unboxedDenotationType = Type.mkEnum(Enums.Denotation, tpe :: Nil, loc)

          // The type `Denotation[Boxed]`.
          val boxedDenotationType = Types.Denotation

          val Lattice: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Ast.lattice")
          val LatticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)

          val Box: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint/Ast.box")
          val BoxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)

          val innerApply = LoweredAst.Expression.Apply(LoweredAst.Expression.Def(Lattice, LatticeType, loc), List(LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)), unboxedDenotationType, Type.Pure, loc)
          LoweredAst.Expression.Apply(LoweredAst.Expression.Def(Box, BoxType, loc), List(innerApply), boxedDenotationType, Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation): LoweredAst.Expression = p match {
    case Polarity.Positive =>
      val innerExp = LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Polarity, "Positive", innerExp, Types.Polarity, loc)

    case Polarity.Negative =>
      val innerExp = LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Polarity, "Negative", innerExp, Types.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.Fixity` from the given fixity `f`.
    */
  private def mkFixity(f: Ast.Fixity, loc: SourceLocation): LoweredAst.Expression = f match {
    case Fixity.Loose =>
      val innerExp = LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Fixity, "Loose", innerExp, Types.Fixity, loc)

    case Fixity.Fixed =>
      val innerExp = LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Fixity, "Fixed", innerExp, Types.Fixity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred): LoweredAst.Expression = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = LoweredAst.Expression.Cst(Ast.Constant.Str(sym), Type.Str, loc)
      val idExp = LoweredAst.Expression.Cst(Ast.Constant.Int64(0), Type.Int64, loc)
      val inner = mkTuple(List(nameExp, idExp), loc)
      mkTag(Enums.PredSym, "PredSym", inner, Types.PredSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): LoweredAst.Expression = {
    val nameExp = LoweredAst.Expression.Cst(Ast.Constant.Str(sym.text), Type.Str, sym.loc)
    mkTag(Enums.VarSym, "VarSym", nameExp, Types.VarSym, sym.loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: LoweredAst.Expression)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed, loc)
    val innerExp = LoweredAst.Expression.Def(Defs.Box, tpe, loc)
    LoweredAst.Expression.Apply(innerExp, List(exp), Types.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a `Fixpoint/Ast.BodyPredicate.GuardX`.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expression, loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
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
      val fparam = LoweredAst.FormalParam(sym, Ast.Modifiers.Empty, Type.Unit, Ast.TypeSource.Ascribed, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = LoweredAst.Expression.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.BodyPredicate, s"Guard0", lambdaExp, Types.BodyPredicate, loc)
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
        val fparam = LoweredAst.FormalParam(freshSym, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expression.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXb(lambdaExp, fvs.map(_._2))

    // Construct the `Fixpoint.Ast/BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps, loc)
    mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint.Ast.BodyPredicate.Functional`.
    */
  private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expression, loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
    // Compute the number of in and out variables.
    val numberOfInVars = inVars.length
    val numberOfOutVars = outVars.length

    if (numberOfInVars == 0) {
      throw InternalCompilerException("Requires at least one in variable.", loc)
    }
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
        val fparam = LoweredAst.FormalParam(freshSym, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expression.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXY(outVars, lambdaExp, inVars.map(_._2), exp.tpe, exp.loc)

    // Construct the `Fixpoint.Ast.BodyPredicate` value.
    val boundVarVector = mkVector(outVars.map(mkVarSym), Types.VarSym, loc)
    val freeVarVector = mkVector(inVars.map(kv => mkVarSym(kv._1)), Types.VarSym, loc)
    val innerExp = mkTuple(boundVarVector :: liftedExp :: freeVarVector :: Nil, loc)
    mkTag(Enums.BodyPredicate, s"Functional", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast.Term.AppX`.
    */
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expression, loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expression = {
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
      val fparam = LoweredAst.FormalParam(sym, Ast.Modifiers.Empty, Type.Unit, Ast.TypeSource.Ascribed, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = LoweredAst.Expression.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.HeadTerm, s"App0", lambdaExp, Types.HeadTerm, loc)
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
        val fparam = LoweredAst.FormalParam(freshSym, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expression.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs.map(_._2), exp.tpe)

    // Construct the `Fixpoint.Ast/BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps, loc)
    mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Make a new channel expression
    */
  private def mkNewChannel(exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expression = {
    val newChannel = LoweredAst.Expression.Def(Defs.ChannelNew, Type.mkImpureArrow(exp.tpe, tpe, loc), loc)
    LoweredAst.Expression.Apply(newChannel, exp :: Nil, tpe, eff, loc)
  }

  /**
    * Make a new channel tuple (sender, receiver) expression
    */
  private def mkNewChannelTuple(exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expression = {
    val newChannel = LoweredAst.Expression.Def(Defs.ChannelNewTuple, Type.mkImpureArrow(exp.tpe, tpe, loc), loc)
    LoweredAst.Expression.Apply(newChannel, exp :: Nil, tpe, eff, loc)
  }

  /**
    * Make a channel get expression
    */
  private def mkGetChannel(exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expression = {
    val getChannel = LoweredAst.Expression.Def(Defs.ChannelGet, Type.mkImpureArrow(exp.tpe, tpe, loc), loc)
    LoweredAst.Expression.Apply(getChannel, exp :: Nil, tpe, eff, loc)
  }

  /**
    * Make a channel put expression
    */
  private def mkPutChannel(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, eff: Type, loc: SourceLocation): LoweredAst.Expression = {
    val putChannel = LoweredAst.Expression.Def(Defs.ChannelPut, Type.mkImpureUncurriedArrow(List(exp2.tpe, exp1.tpe), Type.Unit, loc), loc)
    LoweredAst.Expression.Apply(putChannel, List(exp2, exp1), Type.Unit, eff, loc)
  }

  /**
    * Make the list of MpmcAdmin objects which will be passed to `selectFrom`
    */
  private def mkChannelAdminList(rs: List[LoweredAst.SelectChannelRule], channels: List[(Symbol.VarSym, LoweredAst.Expression)], loc: SourceLocation): LoweredAst.Expression = {
    val admins = rs.zip(channels) map {
      case (LoweredAst.SelectChannelRule(_, c, _), (chanSym, _)) =>
        val admin = LoweredAst.Expression.Def(Defs.ChannelMpmcAdmin, Type.mkPureArrow(c.tpe, Types.ChannelMpmcAdmin, loc), loc)
        LoweredAst.Expression.Apply(admin, List(LoweredAst.Expression.Var(chanSym, c.tpe, loc)), Types.ChannelMpmcAdmin, Type.Pure, loc)
    }
    mkList(admins, Types.ChannelMpmcAdmin, loc)
  }

  /**
    * Construct a call to `selectFrom` given a list of MpmcAdmin objects and optional default
    */
  private def mkChannelSelect(admins: LoweredAst.Expression, default: Option[LoweredAst.Expression], loc: SourceLocation): LoweredAst.Expression = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    val selectRetTpe = Type.mkTuple(List(Type.Int32, locksType), loc)
    val selectTpe = Type.mkImpureUncurriedArrow(List(admins.tpe, Type.Bool), selectRetTpe, loc)
    val select = LoweredAst.Expression.Def(Defs.ChannelSelectFrom, selectTpe, loc)
    val blocking = default match {
      case Some(_) => LoweredAst.Expression.Cst(Ast.Constant.Bool(false), Type.Bool, loc)
      case None => LoweredAst.Expression.Cst(Ast.Constant.Bool(true), Type.Bool, loc)
    }
    LoweredAst.Expression.Apply(select, List(admins, blocking), selectRetTpe, Type.Impure, loc)
  }

  /**
    * Construct a sequence of MatchRules corresponding to the given SelectChannelRules
    */
  private def mkChannelCases(rs: List[LoweredAst.SelectChannelRule], channels: List[(Symbol.VarSym, LoweredAst.Expression)], eff: Type, loc: SourceLocation)(implicit flix: Flix): List[LoweredAst.MatchRule] = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    rs.zip(channels).zipWithIndex map {
      case ((LoweredAst.SelectChannelRule(sym, chan, exp), (chSym, _)), i) =>
        val locksSym = mkLetSym("locks", loc)
        val pat = mkTuplePattern(List(LoweredAst.Pattern.Cst(Ast.Constant.Int32(i), Type.Int32, loc), LoweredAst.Pattern.Var(locksSym, locksType, loc)), loc)
        val getTpe = Type.eraseTopAliases(chan.tpe) match {
          case Type.Apply(_, t, _) => t
          case _ => throw InternalCompilerException("Unexpected channel type found.", loc)
        }
        val get = LoweredAst.Expression.Def(Defs.ChannelUnsafeGetAndUnlock, Type.mkImpureUncurriedArrow(List(chan.tpe, locksType), getTpe, loc), loc)
        val getExp = LoweredAst.Expression.Apply(get, List(LoweredAst.Expression.Var(chSym, chan.tpe, loc), LoweredAst.Expression.Var(locksSym, locksType, loc)), getTpe, eff, loc)
        val e = LoweredAst.Expression.Let(sym, Ast.Modifiers.Empty, getExp, exp, exp.tpe, eff, loc)
        LoweredAst.MatchRule(pat, None, e)
    }
  }

  /**
    * Construct additional MatchRule to handle the (optional) default case
    * NB: Does not need to unlock because that is handled inside Concurrent/Channel.selectFrom.
    */
  private def mkSelectDefaultCase(default: Option[LoweredAst.Expression], t: Type, loc: SourceLocation)(implicit flix: Flix): List[LoweredAst.MatchRule] = {
    default match {
      case Some(defaultExp) =>
        val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)
        val pat = mkTuplePattern(List(LoweredAst.Pattern.Cst(Ast.Constant.Int32(-1), Type.Int32, loc), LoweredAst.Pattern.Wild(locksType, loc)), loc)
        val defaultMatch = LoweredAst.MatchRule(pat, None, defaultExp)
        List(defaultMatch)
      case _ =>
        List()
    }
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: LoweredAst.Expression, argTypes: List[Type], resultType: Type): LoweredAst.Expression = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Boxable.lift${argTypes.length}")

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
    val defn = LoweredAst.Expression.Def(sym, liftType, exp0.loc)
    LoweredAst.Expression.Apply(defn, List(exp0), returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    */
  private def liftXb(exp0: LoweredAst.Expression, argTypes: List[Type]): LoweredAst.Expression = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Boxable.lift${argTypes.length}b")

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
    val defn = LoweredAst.Expression.Def(sym, liftType, exp0.loc)
    LoweredAst.Expression.Apply(defn, List(exp0), returnType, Type.Pure, exp0.loc)
  }


  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
    */
  private def liftXY(outVars: List[Symbol.VarSym], exp0: LoweredAst.Expression, argTypes: List[Type], resultType: Type, loc: SourceLocation): LoweredAst.Expression = {
    // Compute the number of bound ("output") and free ("input") variables.
    val numberOfInVars = argTypes.length
    val numberOfOutVars = outVars.length

    // Compute the liftXY symbol.
    // For example, lift3X2 is a function from three arguments to a Vector of pairs.
    val sym = Symbol.mkDefnSym(s"Boxable.lift${numberOfInVars}X${numberOfOutVars}")

    //
    // The liftXY family of functions are of the form: i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)] and
    // returns a function of the form Vector[Boxed] -> Vector[Vector[Boxed]].
    // That is, the function accepts a *curried* function and an uncurried function that takes
    // its input as a boxed Vector and return its output as a vector of vectors.
    //

    // The type of the function argument, i.e. i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)].
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, loc)

    // The type of the returned function, i.e. Vector[Boxed] -> Vector[Vector[Boxed]].
    val returnType = Type.mkPureArrow(Type.mkVector(Types.Boxed, loc), Type.mkVector(Type.mkVector(Types.Boxed, loc), loc), loc)

    // The type of the overall liftXY function, i.e. (i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)]) -> (Vector[Boxed] -> Vector[Vector[Boxed]]).
    val liftType = Type.mkPureArrow(argType, returnType, loc)

    // Construct a call to the liftXY function.
    val defn = LoweredAst.Expression.Def(sym, liftType, loc)
    LoweredAst.Expression.Apply(defn, List(exp0), returnType, Type.Pure, loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkList(exps: List[LoweredAst.Expression], elmType: Type, loc: SourceLocation): LoweredAst.Expression = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, loc)
    }
  }

  /**
    * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkVector(exps: List[LoweredAst.Expression], elmType: Type, loc: SourceLocation): LoweredAst.Expression = {
    LoweredAst.Expression.VectorLit(exps, Type.mkVector(elmType, loc), Type.Pure, loc)
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    */
  private def mkNil(elmType: Type, loc: SourceLocation): LoweredAst.Expression = {
    mkTag(Enums.FList, "Nil", LoweredAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc), Types.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
    */
  private def mkCons(hd: LoweredAst.Expression, tail: LoweredAst.Expression, loc: SourceLocation): LoweredAst.Expression = {
    val tuple = mkTuple(hd :: tail :: Nil, loc)
    mkTag(Enums.FList, "Cons", tuple, tail.tpe, loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation): LoweredAst.Expression = {
    val caseSym = new Symbol.CaseSym(sym, tag, SourceLocation.Unknown)
    LoweredAst.Expression.Tag(Ast.CaseSymUse(caseSym, loc), exp, tpe, Type.Pure, loc)
  }

  /**
    * Returns a pure tuple expression constructed from the given list of expressions `exps`.
    */
  private def mkTuple(exps: List[LoweredAst.Expression], loc: SourceLocation): LoweredAst.Expression = {
    val tpe = Type.mkTuple(exps.map(_.tpe), loc)
    val eff = Type.Pure
    LoweredAst.Expression.Tuple(exps, tpe, eff, loc)
  }

  /**
    * Returns a new `VarSym` for use in a let-binding.
    *
    * This function is called `mkLetSym` to avoid confusion with [[mkVarSym]].
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe`
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Star), loc), tpe, loc)
  }

  /**
    * An expression for a channel variable called `sym`
    */
  private def mkChannelExp(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation): LoweredAst.Expression = {
    LoweredAst.Expression.Var(sym, mkChannelTpe(tpe, loc), loc)
  }

  /**
    * Returns a `GetChannel` expression based on `sym` and `exp`.
    */
  private def mkParWait(exp: LoweredAst.Expression, sym: Symbol.VarSym): LoweredAst.Expression = {
    val loc = exp.loc.asSynthetic
    val chExp = mkChannelExp(sym, exp.tpe, loc)
    mkGetChannel(chExp, exp.tpe, Type.Impure, loc)
  }

  /**
    * Returns a full `par exp` expression.
    */
  private def mkParChannels(exp: LoweredAst.Expression, chanSymsWithExps: List[(Symbol.VarSym, LoweredAst.Expression)]): LoweredAst.Expression = {
    // Make spawn expressions `spawn ch <- exp`.
    val spawns = chanSymsWithExps.foldRight(exp: LoweredAst.Expression) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val e1 = mkChannelExp(sym, e.tpe, loc) // The channel `ch`
        val e2 = mkPutChannel(e1, e, Type.Impure, loc) // The put exp: `ch <- exp0`.
        val e3 = LoweredAst.Expression.Spawn(e2, LoweredAst.Expression.Region(Type.Unit, loc), Type.Unit, Type.Impure, loc) // Spawn the put expression from above i.e. `spawn ch <- exp0`.
        LoweredAst.Expression.Stm(e3, acc, acc.tpe, Type.mkUnion(e3.eff, acc.eff, loc), loc) // Return a statement expression containing the other spawn expressions along with this one.
    }

    // Make let bindings `let ch = chan 1;`.
    chanSymsWithExps.foldRight(spawns: LoweredAst.Expression) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val chan = mkNewChannel(LoweredAst.Expression.Cst(Ast.Constant.Int32(1), Type.Int32, loc), mkChannelTpe(e.tpe, loc), Type.Impure, loc) // The channel exp `chan 1`
        LoweredAst.Expression.Let(sym, Modifiers(List(Ast.Modifier.Synthetic)), chan, acc, acc.tpe, Type.mkUnion(e.eff, acc.eff, loc), loc) // The let-binding `let ch = chan 1`
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
  def mkLetMatch(pat: LoweredAst.Pattern, exp: LoweredAst.Expression, body: LoweredAst.Expression): LoweredAst.Expression = {
    val loc = exp.loc.asSynthetic
    val rule = List(LoweredAst.MatchRule(pat, None, body))
    val eff = Type.mkUnion(exp.eff, body.eff, loc)
    LoweredAst.Expression.Match(exp, rule, body.tpe, eff, loc)
  }

  /**
    * Returns an expression where the pattern variables used in `exp` are
    * bound to [[TypedAst.Expression.GetChannel]] expressions,
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
  def mkBoundParWaits(patSymExps: List[(LoweredAst.Pattern, Symbol.VarSym, LoweredAst.Expression)], exp: LoweredAst.Expression): LoweredAst.Expression =
    patSymExps.map {
      case (p, sym, e) =>
        val loc = e.loc.asSynthetic
        val chExp = mkChannelExp(sym, e.tpe, loc)
        (p, mkGetChannel(chExp, e.tpe, Type.Impure, loc))
    }.foldRight(exp) {
      case ((pat, chan), e) => mkLetMatch(pat, chan, e)
    }

  /**
    * Returns a desugared [[TypedAst.Expression.ParYield]] expression as a nested match-expression.
    */
  private def mkParYield(frags: List[LoweredAst.ParYieldFragment], exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation)(implicit flix: Flix): LoweredAst.Expression = {
    // Partition fragments into complex and simple (vars or csts) exps.
    val (complex, varOrCsts) = frags.partition(f => isSpawnable(f.exp))

    // Only generate channels for n-1 fragments. We use the current thread for the last fragment.
    val (fs, lastComplex) = complex.splitAt(complex.length - 1)

    // Generate symbols for each channel.
    val chanSymsWithPatAndExp = fs.map { case LoweredAst.ParYieldFragment(p, e, l) => (p, mkLetSym("channel", l.asSynthetic), e) }

    // Make `GetChannel` exps for the spawnable exps.
    val waitExps = mkBoundParWaits(chanSymsWithPatAndExp, exp)

    // Make expression that evaluates simple exps and the last fragment before proceeding to wait for channels.
    val desugaredYieldExp = mkParYieldCurrentThread(varOrCsts ::: lastComplex, waitExps)

    // Generate channels and spawn exps.
    val chanSymsWithExp = chanSymsWithPatAndExp.map { case (_, s, e) => (s, e) }
    val blockExp = mkParChannels(desugaredYieldExp, chanSymsWithExp)

    // Wrap everything in a purity cast,
    LoweredAst.Expression.Cast(blockExp, None, Some(Type.Pure), tpe, eff, loc.asSynthetic)
  }

  /**
    * Returns the expression of a `ParYield` expression that should be evaluated in the current thread.
    */
  private def mkParYieldCurrentThread(exps: List[LoweredAst.ParYieldFragment], waitExps: LoweredAst.Expression): LoweredAst.Expression = {
    exps.foldRight(waitExps) {
      case (exp, acc) => mkLetMatch(exp.pat, exp.exp, acc)
    }
  }

  /**
    * Returns `true` if the ParYield fragment should be spawned in a thread. Wrapper for `isVarOrCst`.
    */
  private def isSpawnable(exp: LoweredAst.Expression): Boolean = !isVarOrCst(exp)

  /**
    * Returns `true` if `exp0` is either a literal or a variable.
    */
  private def isVarOrCst(exp0: LoweredAst.Expression): Boolean = exp0 match {
    case LoweredAst.Expression.Var(_, _, _) => true
    case LoweredAst.Expression.Cst(_: Ast.Constant, _, _) => true
    case _ => false
  }

  /**
    * Returns a tuple expression that is evaluated in parallel.
    *
    * {{{
    *   par (exp0, exp1, exp2)
    * }}}
    *
    * is translated to
    *
    * {{{
    *   let ch0 = chan 1;
    *   let ch1 = chan 1;
    *   let ch2 = chan 1;
    *   spawn ch0 <- exp0;
    *   spawn ch1 <- exp1;
    *   spawn ch2 <- exp2;
    *   (<- ch0, <- ch1, <- ch2)
    * }}}
    */
  private def mkParTuple(exp: LoweredAst.Expression.Tuple)(implicit flix: Flix): LoweredAst.Expression = {
    val LoweredAst.Expression.Tuple(elms, tpe, eff, loc) = exp

    // Partition elements into complex and simple (vars or csts) exps.
    // We remember the index so we can sort the expression into the correct
    // position of the tuple.
    val (complex, varOrCsts) = elms.zipWithIndex.partition(e => isSpawnable(e._1))

    // Only generate channels for n-1 elements. We use the current thread for the last element.
    val (es, last) = complex.splitAt(complex.length - 1)

    // Generate symbols for each channel.
    val chanSymsWithExps = es.map { case (e, i) => (mkLetSym("channel", e.loc.asSynthetic), e, i) }

    // Make GetChannel exps for the spawned expressions.
    val waitExps = chanSymsWithExps.map { case (s, e, i) => (mkParWait(e, s), i) }

    val lastVarExpWithSym = last.map {
      case (e, i) =>
        val sym = Symbol.freshVarSym("last", Ast.BoundBy.Let, e.loc)
        (LoweredAst.Expression.Var(sym, e.tpe, e.loc.asSynthetic), i, e, sym)
    }

    val lastVarExp = lastVarExpWithSym.map { case (v, i, _, _) => (v, i) }

    // Sort to original ordering and map to exps
    val parElmExps = (waitExps ::: lastVarExp ::: varOrCsts).sortBy(_._2).map(_._1)

    // Make new tuple
    val parTuple = LoweredAst.Expression.Tuple(parElmExps, tpe, eff, loc.asSynthetic)

    // Make let-exp that evaluates last under `lastVarExp` and prepend to the tuple.
    val lastLetExp = lastVarExpWithSym.map {
      case (_, _, e, s) =>
        val l = e.loc.asSynthetic
        val mods = Ast.Modifiers.Empty
        val t = parTuple.tpe
        val p = Type.mkUnion(e.eff, parTuple.eff, l)
        LoweredAst.Expression.Let(s, mods, e, parTuple, t, p, l)
    }

    // If there was no lastVarExp, then just return `parTuple`.
    val finalExp = lastLetExp match {
      case Nil => parTuple
      case e :: _ => e
    }

    // Finally, spawn channels.
    mkParChannels(finalExp, chanSymsWithExps.map { case (s, e, _) => (s, e) })
  }

  /**
    * Applies the given expression `exp` to the `debug` function.
    */
  private def mkApplyDebug(exp1: TypedAst.Expression, exp2: TypedAst.Expression, loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Expression = {
    //
    // Note that we mark the call as impure (even though it may have been typed as pure!)
    //
    val tpe = Type.mkImpureUncurriedArrow(exp1.tpe :: exp2.tpe :: Nil, exp2.tpe, loc)
    val innerExp = TypedAst.Expression.Def(Defs.DebugWithPrefix, tpe, loc)
    TypedAst.Expression.Apply(innerExp, exp1 :: exp2 :: Nil, exp2.tpe, Type.Impure, loc)
  }

  /**
    * Returns a TypedAst.Pattern representing a tuple of patterns.
    */
  def mkTuplePattern(patterns: List[LoweredAst.Pattern], loc: SourceLocation): LoweredAst.Pattern = {
    LoweredAst.Pattern.Tuple(patterns, Type.mkTuple(patterns.map(_.tpe), loc), loc)
  }

  /**
    * Return a list of quantified variables in the given expression `exp0`.
    *
    * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
    * but not in the constraint params `cparams0` of the constraint.
    */
  private def quantifiedVars(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expression): List[(Symbol.VarSym, Type)] = {
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
    cparams0.exists(p => p.sym == sym)

  /**
    * Applies the given substitution `subst` to the given expression `exp0`.
    */
  private def substExp(exp0: LoweredAst.Expression, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.Expression = exp0 match {
    case LoweredAst.Expression.Cst(_, _, _) => exp0

    case LoweredAst.Expression.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.Expression.Var(s, tpe, loc)

    case LoweredAst.Expression.Def(_, _, _) => exp0

    case LoweredAst.Expression.Sig(_, _, _) => exp0

    case LoweredAst.Expression.Hole(_, _, _) => exp0

    case LoweredAst.Expression.Lambda(fparam, exp, tpe, loc) =>
      val p = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      LoweredAst.Expression.Lambda(p, e, tpe, loc)

    case LoweredAst.Expression.Apply(exp, exps, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expression.Apply(e, es, tpe, eff, loc)

    case LoweredAst.Expression.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expression.ApplyAtomic(op, es, tpe, eff, loc)

    case LoweredAst.Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.Let(s, mod, e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.LetRec(s, mod, e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.Region(tpe, loc) =>
      LoweredAst.Expression.Region(tpe, loc)

    case LoweredAst.Expression.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e = substExp(exp, subst)
      LoweredAst.Expression.Scope(s, regionVar, e, tpe, eff, loc)

    case LoweredAst.Expression.ScopeExit(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.ScopeExit(e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      LoweredAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case LoweredAst.Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.Stm(e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.Discard(exp, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Discard(e, eff, loc)

    case LoweredAst.Expression.Match(_, _, _, _, _) => ??? // TODO

    case LoweredAst.Expression.TypeMatch(_, _, _, _, _) => ??? // TODO

    case LoweredAst.Expression.RelationalChoose(exps, rules, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      val rs = rules map {
        case LoweredAst.RelationalChooseRule(pat, exp) =>
          // TODO: Substitute in patterns?
          LoweredAst.RelationalChooseRule(pat, substExp(exp, subst))
      }
      LoweredAst.Expression.RelationalChoose(es, rs, tpe, eff, loc)

    case LoweredAst.Expression.Tag(sym, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Tag(sym, e, tpe, eff, loc)

    case LoweredAst.Expression.Tuple(elms, tpe, eff, loc) =>
      val es = elms.map(substExp(_, subst))
      LoweredAst.Expression.Tuple(es, tpe, eff, loc)

    case LoweredAst.Expression.RecordEmpty(_, _) => exp0

    case LoweredAst.Expression.RecordSelect(exp, field, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.RecordSelect(e, field, tpe, eff, loc)

    case LoweredAst.Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      val v = substExp(value, subst)
      val r = substExp(rest, subst)
      LoweredAst.Expression.RecordExtend(field, v, r, tpe, eff, loc)

    case LoweredAst.Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      val r = substExp(rest, subst)
      LoweredAst.Expression.RecordRestrict(field, r, tpe, eff, loc)

    case LoweredAst.Expression.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      val e = substExp(exp, subst)
      LoweredAst.Expression.ArrayLit(es, e, tpe, eff, loc)

    case LoweredAst.Expression.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      LoweredAst.Expression.ArrayNew(e1, e2, e3, tpe, eff, loc)

    case LoweredAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      val b = substExp(base, subst)
      val i = substExp(index, subst)
      LoweredAst.Expression.ArrayLoad(b, i, tpe, eff, loc)

    case LoweredAst.Expression.ArrayLength(base, eff, loc) =>
      val b = substExp(base, subst)
      LoweredAst.Expression.ArrayLength(b, eff, loc)

    case LoweredAst.Expression.ArrayStore(base, index, elm, eff, loc) =>
      val b = substExp(base, subst)
      val i = substExp(index, subst)
      LoweredAst.Expression.ArrayStore(b, i, elm, eff, loc)

    case LoweredAst.Expression.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expression.VectorLit(es, tpe, eff, loc)

    case LoweredAst.Expression.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.VectorLoad(e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.VectorLength(exp, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.VectorLength(e, loc)

    case LoweredAst.Expression.Ref(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.Ref(e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.Deref(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Deref(e, tpe, eff, loc)

    case LoweredAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.Assign(e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.Ascribe(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Ascribe(e, tpe, eff, loc)

    case LoweredAst.Expression.InstanceOf(exp, clazz, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.InstanceOf(e, clazz, loc)

    case LoweredAst.Expression.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case LoweredAst.Expression.TryCatch(_, _, _, _, _) => ??? // TODO

    case LoweredAst.Expression.TryWith(exp, sym, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.HandlerRule(op, fparams, hexp) =>
          val fps = fparams.map(substFormalParam(_, subst))
          val he = substExp(hexp, subst)
          LoweredAst.HandlerRule(op, fps, he)
      }
      LoweredAst.Expression.TryWith(e, sym, rs, tpe, eff, loc)

    case LoweredAst.Expression.Do(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expression.Do(sym, es, tpe, eff, loc)

    case LoweredAst.Expression.Resume(exp, tpe, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Resume(e, tpe, loc)

    case LoweredAst.Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      val as = args.map(substExp(_, subst))
      LoweredAst.Expression.InvokeConstructor(constructor, as, tpe, eff, loc)

    case LoweredAst.Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val as = args.map(substExp(_, subst))
      LoweredAst.Expression.InvokeMethod(method, e, as, tpe, eff, loc)

    case LoweredAst.Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      val as = args.map(substExp(_, subst))
      LoweredAst.Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

    case LoweredAst.Expression.GetField(field, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.GetField(field, e, tpe, eff, loc)

    case LoweredAst.Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.PutField(field, e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.GetStaticField(_, _, _, _) => exp0

    case LoweredAst.Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.PutStaticField(field, e, tpe, eff, loc)

    case LoweredAst.Expression.NewObject(_, _, _, _, _, _) => exp0

    case LoweredAst.Expression.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expression.Spawn(e1, e2, tpe, eff, loc)

    case LoweredAst.Expression.Lazy(exp, tpe, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Lazy(e, tpe, loc)

    case LoweredAst.Expression.Force(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expression.Force(e, tpe, eff, loc)

  }

  /**
    * Applies the given substitution `subst` to the given formal param `fparam0`.
    */
  private def substFormalParam(fparam0: LoweredAst.FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.FormalParam = fparam0 match {
    case LoweredAst.FormalParam(sym, mod, tpe, src, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.FormalParam(s, mod, tpe, src, loc)
  }

}
