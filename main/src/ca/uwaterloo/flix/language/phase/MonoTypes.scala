/*
 * Copyright 2023 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.Ast.CaseSymUse
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, LoweredAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.{Substitution, TypeNormalization}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable

/**
  * This phase does two things:
  * - Specializes polymorphic enums with the specific types that it is used as.
  * - Removes all type aliases in types
  */
object MonoTypes {

  /**
    * Holds the mutable data used throughout monomorphization.
    *
    * The context is shared and must be guarded by locks (synchronized) to ensure thread-safety.
    */
  private class SharedContext() {

    /**
      * A map from a symbol and a ground normalized type to the fresh symbol for
      * the specialized version of that enum.
      *
      * For example, if the enum:
      *
      * -   def MyEnum[a, b]{ case MyCase(a, b) }
      *
      * has been specialized w.r.t. to `Int` and `String` then this map will contain an entry:
      *
      * -   (MyEnum, MyEnum[Int, String]) -> MyEnum$42
      *
      * Like shown here, the type should be pre enum specialization and the enum
      * symbols should be un-specialized, i.e. present in root.enums. To re-iterate:
      *
      * The types in this must must:
      * 1. Be ground, i.e. no type variables
      * 2. Be normalized, i.e.
      * 2a. {a=Int32, b=Int32} and {b=Int32, a=Int32} should not both be present (labels should be sorted)
      * 2b. Pure + Pure and Pure should not both be present (formulas should be fully evaluated)
      * 3. Contain no specialized enums, i.e. should use List[Int32] instead of List$32
      */
    private val enum2enum: mutable.Map[(Symbol.EnumSym, Type), (Symbol.EnumSym, List[Type])] = mutable.Map.empty

    /**
      * A map used to collect specialized definitions.
      */
    private val specializedEnums: mutable.Map[Symbol.EnumSym, LoweredAst.Enum] = mutable.Map.empty

    /**
      * Lookup `sym` specialized to `tpe`.
      *
      * If the enum is already specialized, return its fresh symbol.
      *
      * Otherwise, create a fresh symbol.
      */
    def getOrPut(sym: Symbol.EnumSym, args: List[Type], tpe: Type, nonParametric: Boolean)(implicit flix: Flix): Symbol.EnumSym = synchronized {
      enum2enum.get((sym, tpe)) match {
        case None =>
          val freshSym = if (nonParametric) sym else Symbol.freshEnumSym(sym)
          enum2enum.put((sym, tpe), (freshSym, args))
          freshSym
        case Some((freshSym, _)) => freshSym
      }
    }

    def specializations: Map[(Symbol.EnumSym, Type), (Symbol.EnumSym, List[Type])] =
      enum2enum.toMap

    def clearSpecializations(): Unit = enum2enum.clear()

    /**
      * Returns the specialized enums as a map.
      */
    def toMap: Map[Symbol.EnumSym, LoweredAst.Enum] = synchronized {
      specializedEnums.toMap
    }

    def addSpecializedEnum(e: LoweredAst.Enum): Unit = synchronized {
      specializedEnums.put(e.sym, e)
    }
  }

  /**
    * Performs monomorphization of enums on the given AST `root` and removes alias types.
    */
  def run(root: LoweredAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("MonoTypes") {
    // Goal:
    //   The goal of MonoTypes is to replace polymorphic enums with specialized enums.
    //   For example replacing `List[Int32]` with `List$152` with cases specified to `Int32`.
    // Assumptions:
    // - All typeclass information have been transformed into defs - this
    //   phase only looks at types and expressions in defs.
    // - All the following types have been removed:
    //   - Type variables
    //   - Associated types
    // - In schemas these are unused
    //   - tconstrs
    //   - econstrs


    implicit val r: LoweredAst.Root = root
    implicit val ctx: SharedContext = new SharedContext()

    // phase 1
    //   A) Find all enum types via all types and expressions in defs
    //   B) Build a mappping (enum2enum) from enum types to new specialized names (`List[Int32]` |-> `List$152`)
    //   C) Replace enum expressions and enum types with their new name.
    val defs = ParOps.parMapValues(root.defs)(visitDef)

    // phase 2
    //   A) These new specializations (in enum2enum) might lead to more specializations when looking
    //      at the enum definitions. `MyEnum[Char]` might have a case which mentions
    //      `List[MyEnum[Char]]` which could be a new specialization of `List`.

    // phase 3
    //   A) Look through all the specializations (in enum2enum) and generate the new enums.

    // phase 2 & 3 combined
    while (ctx.specializations.nonEmpty) {
      ParOps.parMap(ctx.specializations) {
        case ((oldSym, tpe), (specializedSym, specializedArgs)) =>
          createSpecialization(oldSym, specializedArgs, specializedSym, tpe.loc)
      }
      ctx.clearSpecializations()
    }

    root.copy(defs = defs, enums = ctx.toMap)
  }

  /**
    * Returns a [[LoweredAst.Def]] with specialized enums and without aliases in its types.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitDef(defn: LoweredAst.Def)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): LoweredAst.Def = defn match {
    case LoweredAst.Def(sym, spec, exp) =>
      val s = visitSpec(spec)
      val e = visitExp(exp)
      LoweredAst.Def(sym, s, e)
  }

  /**
    * Returns a [[LoweredAst.Spec]] with specialized enums and without aliases in its types.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitSpec(spec: LoweredAst.Spec)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): LoweredAst.Spec = spec match {
    case LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, loc) =>
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      val rt = visitType(retTpe)
      val p = visitType(eff)
      LoweredAst.Spec(doc, ann, mod, tparams, fs, ds, rt, p, tconstrs, loc)
  }

  /**
    * Returns an expression with specialized enums and without aliases in its types.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitExp(exp: LoweredAst.Expr)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): LoweredAst.Expr = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Cst(cst, t, loc)

    case Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Var(sym, t, loc)

    case Expr.Def(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Def(sym, t, loc)

    case Expr.Sig(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Sig(sym, t, loc)

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val fs = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expr.Lambda(fs, e, t, loc)

    case Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Apply(e, es, t, p, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val op1 = op match {
        case AtomicOp.Tag(sym) =>
          //
          // Specialize the enum
          //
          val freshCaseSym = specializeCaseSymUse(CaseSymUse(sym, sym.loc), tpe.typeArguments, tpe.loc)
          AtomicOp.Tag(freshCaseSym.sym)
        case _ => op
      }
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.ApplyAtomic(op1, es, t, p, loc)

    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Let(sym, mod, e1, e2, t, p, loc)

    case Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.LetRec(sym, mod, e1, e2, t, p, loc)

    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      // The region variable has been rendered redundant by Monomorph.
      // It has replaced the region with pure/impure and the variable could
      // conceptually be removed.
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Scope(sym, regionVar, e, t, p, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.IfThenElse(e1, e2, e3, t, p, loc)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Stm(e1, e2, t, p, loc)

    case Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      val p = visitType(eff)
      Expr.Discard(e, p, loc)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.MatchRule(pat, guard, exp) =>
          val p = visitPat(pat)
          val g = guard.map(visitExp)
          val e = visitExp(exp)
          LoweredAst.MatchRule(p, g, e)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Match(e, rs, t, p, loc)

    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, exp) =>
          val t = visitType(tpe)
          val re = visitExp(exp)
          LoweredAst.TypeMatchRule(sym, t, re)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.TypeMatch(e, rs, t, p, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.VectorLit(es, t, p, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.VectorLoad(e1, e2, t, p, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Expr.VectorLength(e, loc)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Ascribe(e, t, p, loc)

    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(visitType)
      val dp = declaredEff.map(visitType)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Cast(e, dt, dp, t, p, loc)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.CatchRule(sym, clazz, exp) =>
          val re = visitExp(exp)
          LoweredAst.CatchRule(sym, clazz, re)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.TryCatch(e, rs, t, p, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.HandlerRule(op, fparams, exp) =>
          val fs = fparams.map(visitFormalParam)
          val he = visitExp(exp)
          LoweredAst.HandlerRule(op, fs, he)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.TryWith(e, effUse, rs, t, p, loc)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Do(op, es, t, p, loc)

    case Expr.Resume(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expr.Resume(e, t, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe)
      val p = visitType(eff)
      val ms = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val fs = fparams.map(visitFormalParam)
          val me = visitExp(exp)
          val mt = visitType(retTpe)
          val mp = visitType(eff)
          LoweredAst.JvmMethod(ident, fs, me, mt, mp, loc)
      }
      Expr.NewObject(name, clazz, t, p, ms, loc)
  }

  /**
    * Returns a pattern with specialized enums in its type and no aliases.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitPat(pat: LoweredAst.Pattern)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): LoweredAst.Pattern = pat match {
    case Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Wild(t, loc)

    case Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Var(sym, t, loc)

    case Pattern.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Cst(cst, t, loc)

    case Pattern.Tag(sym, tagPat, tpe, loc) =>
      //
      // Specialize the enum
      //
      val freshCaseSym = specializeCaseSymUse(sym, tpe.typeArguments, tpe.loc)
      val tp = visitPat(tagPat)
      val t = visitType(tpe)
      Pattern.Tag(freshCaseSym, tp, t, loc)

    case Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      Pattern.Tuple(es, t, loc)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map {
        case Pattern.Record.RecordLabelPattern(label, tpe1, pat1, loc1) =>
          Pattern.Record.RecordLabelPattern(label, visitType(tpe1), visitPat(pat1), loc1)
      }
      val p = visitPat(pat)
      val t = visitType(tpe)
      Pattern.Record(ps, p, t, loc)

    case Pattern.RecordEmpty(tpe, loc) =>
      Pattern.RecordEmpty(visitType(tpe), loc)
  }

  /**
    * Returns a type with specialized enums and no aliases.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitType(tpe: Type)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): Type = {
    def visitInner(tpe0: Type): Type = tpe0.baseType match {
      case Type.Cst(TypeConstructor.Enum(sym, _), loc) => // enum
        // args cannot be visited yet, because specialization works on
        // non-specialized enums.
        val args = tpe0.typeArguments
        val freshSym = specializeEnum(sym, args, loc)
        Type.mkEnum(freshSym, Nil, loc)
      case _ => tpe0 match { // non-enum
        case Type.Cst(tc, loc) => Type.Cst(tc, loc)
        case Type.Apply(tpe1, tpe2, loc) => Type.Apply(visitInner(tpe1), visitInner(tpe2), loc)
        case Type.Var(sym, loc) => throw InternalCompilerException(s"Unexpected type var: '$sym'", loc)
        case Type.Alias(cst, _, _, loc) => throw InternalCompilerException(s"Unexpected type alias: '${cst.sym}'", loc)
        case Type.AssocType(cst, _, _, loc) => throw InternalCompilerException(s"Unexpected associated type: '${cst.sym}'", loc)
      }
    }
    // It is important that eraseAliases happens BEFORE enum specialization
    visitInner(eraseAliases(tpe))
  }

  /**
    * Returns a formal param with specialized enums in its type and no aliases.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitFormalParam(p: LoweredAst.FormalParam)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): LoweredAst.FormalParam = p match {
    case LoweredAst.FormalParam(sym, mod, tpe, src, loc) =>
      val t = visitType(tpe)
      LoweredAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Returns a scheme with specialized enums in its base and no aliases.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def visitScheme(sc: Scheme)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      // Since the types are expected to be specialized, all except base should be "unused"/empty
      val b = visitType(base)
      Scheme(quantifiers, tconstrs, econstrs, b)
  }

  /**
    * Specializes the given enum in respect to the given type arguments and the
    * enums it itself uses.
    *
    * The type arguments:
    * - must be ground
    * - must not contain specialized enums
    * - may have aliases
    * - may be un-normalized
    */
  private def specializeEnum(sym: Symbol.EnumSym, args0: List[Type], loc: SourceLocation)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): Symbol.EnumSym = {
    // type arguments
    val args = args0.map(eraseAliases).map(TypeNormalization.normalizeType)

    // assemble enum type (e.g. `List[Int32]`)
    val tpe = Type.mkEnum(sym, args, loc)

    // reuse specialization if possible
    ctx.getOrPut(sym, args, tpe, args.isEmpty)
  }

  /**
    * Creates the specialized enum `specializedSym` based on the `oldSym[args...]` enum.
    * The new enum is put into `ctx.specializedEnums`.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def createSpecialization(oldSym: Symbol.EnumSym, args: List[Type], specializedSym: Symbol.EnumSym, loc: SourceLocation)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): Unit = {
    // do the specialization
    // The enum is parametric on its type parameters, so we must instantiate
    // e.g. for List[a] and List[Int32] we substitute [a -> Int32]
    val decl = root.enums(oldSym)
    val subst = Substitution(decl.tparams.map(_.sym).zip(args).toMap)
    val cases = decl.cases.foldLeft(Map.empty[Symbol.CaseSym, LoweredAst.Case]) {
      case (macc, (_, caze)) => macc + specializeCase(specializedSym, caze, subst)
    }
    val specializedEnum = decl.copy(sym = specializedSym, tparams = Nil, cases = cases, tpe = Type.mkEnum(specializedSym, Nil, loc))
    ctx.addSpecializedEnum(specializedEnum)
  }

  /**
    * Specialize the given case `caze` belonging to the given enum `enumSym`.
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def specializeCase(enumSym: Symbol.EnumSym, caze: LoweredAst.Case, subst: Substitution)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): (Symbol.CaseSym, LoweredAst.Case) = caze match {
    case LoweredAst.Case(caseSym, caseTpe, caseSc, caseLoc) =>
      val freshCaseSym = new Symbol.CaseSym(enumSym, caseSym.name, caseSym.loc)
      val newCaseSc = Scheme(caseSc.quantifiers, caseSc.tconstrs, caseSc.econstrs, subst(caseSc.base))
      val caze = LoweredAst.Case(freshCaseSym, visitType(subst(caseTpe)), visitScheme(newCaseSc), caseLoc)
      (freshCaseSym, caze)
  }

  /**
    * Specializes the symbol and the enum it uses via [[specializeEnum]].
    *
    * `ctx.enums2enums` is filled with the enums that need specialization.
    */
  private def specializeCaseSymUse(sym: CaseSymUse, args: List[Type], loc: SourceLocation)(implicit ctx: SharedContext, root: LoweredAst.Root, flix: Flix): CaseSymUse = {
    val freshEnumSym = specializeEnum(sym.sym.enumSym, args, loc)
    Ast.CaseSymUse(new Symbol.CaseSym(freshEnumSym, sym.sym.name, sym.sym.loc), sym.loc)
  }

}
