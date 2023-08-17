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
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.MapOps

import scala.collection.mutable

/**
  * This phase does two things:
  * - Specializes polymorphic enums with the specific types that it is used as.
  * - Removes all type aliases in types
  */
object MonomorphEnums {

  /**
    * Holds the mutable data used throughout monomorphization.
    */
  private class Context() {

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
    val enum2enum: mutable.Map[(Symbol.EnumSym, Type), Symbol.EnumSym] = mutable.Map.empty

    /**
      * A map used to collect specialized definitions.
      */
    val specializedEnums: mutable.Map[Symbol.EnumSym, LoweredAst.Enum] = mutable.Map.empty
  }

  /**
    * Performs monomorphization of enums on the given AST `root` and removes alias types.
    */
  def run(root: LoweredAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("MonomorphEnums") {
    // Assumptions:
    // - All typeclass information have been transformed into defs - this
    //   phase only looks at types and expressions in defs.
    // - All the following types have been removed:
    //   - Type variables
    //   - Associated types
    // - In schemas these are unused
    //   - tconstrs
    //   - econstrs

    // monomorphization works by finding ground enum types in expressions and
    // types.
    // When such an enum is found, its symbol is bound in `ctx.enum2enum` and
    // the is specialized and put into `ctx.specializedEnums`. This process
    // might be recursive which is why the symbol is put into enum2enum before
    // the work is actually done.

    implicit val r: LoweredAst.Root = root

    implicit val ctx: Context = new Context()

    val defs = MapOps.mapValues(root.defs)(visitDef)
    val enums = ctx.specializedEnums.toMap
    root.copy(defs = defs, enums = enums)
  }

  /**
    * Returns a [[LoweredAst.Def]] with specialized enums and without aliases in its types.
    */
  private def visitDef(defn: LoweredAst.Def)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Def = defn match {
    case LoweredAst.Def(sym, spec, impl) =>
      val s = visitSpec(spec)
      val i = visitImpl(impl)
      LoweredAst.Def(sym, s, i)
  }

  /**
    * Returns a [[LoweredAst.Spec]] with specialized enums and without aliases in its types.
    */
  private def visitSpec(spec: LoweredAst.Spec)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Spec = spec match {
    case LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, loc) =>
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      val rt = visitType(retTpe)
      val p = visitType(eff)
      LoweredAst.Spec(doc, ann, mod, tparams, fs, ds, rt, p, tconstrs, loc)
  }

  /**
    * Returns a [[LoweredAst.Impl]] with specialized enums and without aliases in its types.
    */
  private def visitImpl(impl: LoweredAst.Impl)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Impl = impl match {
    case LoweredAst.Impl(exp, inferredScheme) =>
      val e = visitExp(exp)
      val is = visitScheme(inferredScheme)
      LoweredAst.Impl(e, is)
  }

  /**
    * Returns an expression with specialized enums and without aliases in its types
    */
  private def visitExp(exp: LoweredAst.Expr)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Expr = exp match {
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
    case Expr.RelationalChoose(_, _, _, _, loc) =>
      throw InternalCompilerException(s"Code generation for relational choice is no longer supported", loc)
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
    */
  private def visitPat(pat: LoweredAst.Pattern)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Pattern = pat match {
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
        case Pattern.Record.RecordFieldPattern(field, tpe1, pat1, loc1) =>
          Pattern.Record.RecordFieldPattern(field, visitType(tpe1), visitPat(pat1), loc1)
      }
      val p = visitPat(pat)
      val t = visitType(tpe)
      Pattern.Record(ps, p, t, loc)
    case Pattern.RecordEmpty(tpe, loc) =>
      Pattern.RecordEmpty(visitType(tpe), loc)
  }

  /**
    * Returns a type with specialized enums and no aliases.
    */
  private def visitType(tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Type = {
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
    */
  private def visitFormalParam(p: LoweredAst.FormalParam)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.FormalParam = p match {
    case LoweredAst.FormalParam(sym, mod, tpe, src, loc) =>
      val t = visitType(tpe)
      LoweredAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Returns a scheme with specialized enums in its base and no aliases.
    */
  private def visitScheme(sc: Scheme)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Scheme = sc match {
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
  private def specializeEnum(sym: Symbol.EnumSym, args0: List[Type], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.EnumSym = {
    val args = args0.map(eraseAliases).map(TypeNormalization.normalizeType)
    // assemble enum type (e.g. `List[Int32]`)
    val tpe = Type.mkEnum(sym, args, loc)
    // reuse specialization if possible
    ctx.enum2enum.get((sym, tpe)) match {
      case Some(freshSym) =>
        // specialization was already done
        freshSym
      case None =>
        // reuse the existing symbol if the enum is non-parametric
        val freshSym = if (args.isEmpty) sym else Symbol.freshEnumSym(sym)
        // insert the symbol for reuse
        // (inserted before the work is done to avoid infinite recursion)
        ctx.enum2enum.put((sym, tpe), freshSym)

        // do the specialization
        // The enum is parametric on its type parameters, so we must instantiate
        // e.g. for List[a] and List[Int32] we substitute [a -> Int32]
        val e = root.enums(sym)
        val subst = Substitution(e.tparams.map(_.sym).zip(args).toMap)
        val cases = e.cases.map {
          case (caseSym, LoweredAst.Case(_, caseTpe, caseSc, caseLoc)) =>
            val freshCaseSym = new Symbol.CaseSym(freshSym, caseSym.name, caseSym.loc)
            val newCaseSc = Scheme(caseSc.quantifiers, caseSc.tconstrs, caseSc.econstrs, subst(caseSc.base))
            val caze = LoweredAst.Case(freshCaseSym, visitType(subst(caseTpe)), visitScheme(newCaseSc), caseLoc)
            (freshCaseSym, caze)
        }
        val freshEnum = LoweredAst.Enum(
          e.doc,
          e.ann,
          e.mod,
          freshSym,
          Nil,
          e.derives,
          cases,
          Type.mkEnum(freshSym, Nil, loc),
          e.loc
        )
        ctx.specializedEnums.put(freshSym, freshEnum)
        freshSym
    }
  }

  /**
    * Specializes the symbol and the enum it uses via [[specializeEnum]].
    */
  private def specializeCaseSymUse(sym: CaseSymUse, args: List[Type], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): CaseSymUse = {
    val freshEnumSym = specializeEnum(sym.sym.enumSym, args, loc)
    Ast.CaseSymUse(new Symbol.CaseSym(freshEnumSym, sym.sym.name, sym.sym.loc), sym.loc)
  }

}
