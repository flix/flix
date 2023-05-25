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
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expression, Pattern}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.{Ast, LoweredAst, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.InternalCompilerException

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
      *   2a. {a=Int32, b=Int32} and {b=Int32, a=Int32} should not both be present (labels should be sorted)
      *   2b. Pure + Pure and Pure should not both be present (formulas should be fully evaluated)
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

    val defs = for ((sym, defn) <- root.defs) yield {
      val spec0 = defn.spec
      val spec = LoweredAst.Spec(
        spec0.doc,
        spec0.ann,
        spec0.mod,
        spec0.tparams,
        spec0.fparams.map(visitFormalParam),
        visitScheme(spec0.declaredScheme),
        visitType(spec0.retTpe),
        visitType(spec0.pur),
        spec0.tconstrs, spec0.loc
      )
      val impl = LoweredAst.Impl(visitExp(defn.impl.exp), visitScheme(defn.impl.inferredScheme))
      (sym, LoweredAst.Def(sym, spec, impl))
    }

    root.copy(defs = defs, enums = ctx.specializedEnums.toMap)
  }

  /**
    * Returns an expression with specialized enums and without aliases in its types
    */
  private def visitExp(exp: LoweredAst.Expression)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Expression = exp match {
    case Expression.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Cst(cst, t, loc)
    case Expression.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Var(sym, t, loc)
    case Expression.Def(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Def(sym, t, loc)
    case Expression.Sig(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Sig(sym, t, loc)
    case Expression.Hole(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expression.Hole(sym, t, loc)
    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val fs = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Lambda(fs, e, t, loc)
    case Expression.Apply(exp, exps, tpe, pur, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Apply(e, es, t, p, loc)
    case Expression.Unary(sop, exp, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Unary(sop, e, t, p, loc)
    case Expression.Binary(sop, exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Binary(sop, e1, e2, t, p, loc)
    case Expression.Let(sym, mod, exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Let(sym, mod, e1, e2, t, p, loc)
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.LetRec(sym, mod, e1, e2, t, p, loc)
    case Expression.Region(tpe, loc) =>
      Expression.Region(visitType(tpe), loc)
    case Expression.Scope(sym, regionVar, exp, tpe, pur, loc) =>
      // The region variable has been rendered redundant by Monomorph.
      // It has replaced the region with pure/impure and the variable could
      // conceptually be removed.
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Scope(sym, regionVar, e, t, p, loc)
    case Expression.ScopeExit(exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.ScopeExit(e1, e2, t, p, loc)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.IfThenElse(e1, e2, e3, t, p, loc)
    case Expression.Stm(exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Stm(e1, e2, t, p, loc)
    case Expression.Discard(exp, pur, loc) =>
      val e = visitExp(exp)
      val p = visitType(pur)
      Expression.Discard(e, p, loc)
    case Expression.Match(exp, rules, tpe, pur, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.MatchRule(pat, guard, exp) =>
          val p = visitPat(pat)
          val g = guard.map(visitExp)
          val e = visitExp(exp)
          LoweredAst.MatchRule(p, g, e)
      }
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Match(e, rs, t, p, loc)
    case Expression.TypeMatch(exp, rules, tpe, pur, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, exp) =>
          val t = visitType(tpe)
          val re = visitExp(exp)
          LoweredAst.TypeMatchRule(sym, t, re)
      }
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.TypeMatch(e, rs, t, p, loc)
    case Expression.RelationalChoose(_, _, _, _, loc) =>
      throw InternalCompilerException(s"Code generation for relational choice is no longer supported", loc)
    case Expression.Tag(sym, exp, tpe, pur, loc) =>
      //
      // Specialize the enum
      //
      val freshCaseSym = specializeCaseSymUse(sym, tpe.typeArguments, tpe.loc)
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Tag(freshCaseSym, e, t, p, loc)
    case Expression.Tuple(elms, tpe, pur, loc) =>
      val es = elms.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Tuple(es, t, p, loc)
    case Expression.RecordEmpty(tpe, loc) =>
      val t = visitType(tpe)
      Expression.RecordEmpty(t, loc)
    case Expression.RecordSelect(exp, field, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.RecordSelect(e, field, t, p, loc)
    case Expression.RecordExtend(field, value, rest, tpe, pur, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.RecordExtend(field, v, r, t, p, loc)
    case Expression.RecordRestrict(field, rest, tpe, pur, loc) =>
      val r = visitExp(rest)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.RecordRestrict(field, r, t, p, loc)
    case Expression.ArrayLit(exps, exp, tpe, pur, loc) =>
      val es = exps.map(visitExp)
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.ArrayLit(es, e, t, p, loc)
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.ArrayNew(e1, e2, e3, t, p, loc)
    case Expression.ArrayLoad(base, index, tpe, pur, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.ArrayLoad(b, i, t, p, loc)
    case Expression.ArrayLength(base, pur, loc) =>
      val b = visitExp(base)
      val p = visitType(pur)
      Expression.ArrayLength(b, p, loc)
    case Expression.ArrayStore(base, index, elm, pur, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val e = visitExp(elm)
      val p = visitType(pur)
      Expression.ArrayStore(b, i, e, p, loc)
    case Expression.VectorLit(exps, tpe, pur, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.VectorLit(es, t, p, loc)
    case Expression.VectorLoad(exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.VectorLoad(e1, e2, t, p, loc)
    case Expression.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Expression.VectorLength(e, loc)
    case Expression.Ref(exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Ref(e1, e2, t, p, loc)
    case Expression.Deref(exp, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Deref(e, t, p, loc)
    case Expression.Assign(exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Assign(e1, e2, t, p, loc)
    case Expression.Ascribe(exp, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Ascribe(e, t, p, loc)
    case Expression.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      Expression.InstanceOf(e, clazz, loc)
    case Expression.Cast(exp, declaredType, declaredPur, tpe, pur, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(visitType)
      val dp = declaredPur.map(visitType)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Cast(e, dt, dp, t, p, loc)
    case Expression.Without(exp, effUse, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Without(e, effUse, t, p, loc)
    case Expression.TryCatch(exp, rules, tpe, pur, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.CatchRule(sym, clazz, exp) =>
          val re = visitExp(exp)
          LoweredAst.CatchRule(sym, clazz, re)
      }
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.TryCatch(e, rs, t, p, loc)
    case Expression.TryWith(exp, effUse, rules, tpe, pur, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.HandlerRule(op, fparams, exp) =>
          val fs = fparams.map(visitFormalParam)
          val he = visitExp(exp)
          LoweredAst.HandlerRule(op, fs, he)
      }
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.TryWith(e, effUse, rs, t, p, loc)
    case Expression.Do(op, exps, pur, loc) =>
      val es = exps.map(visitExp)
      val p = visitType(pur)
      Expression.Do(op, es, p, loc)
    case Expression.Resume(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Resume(e, t, loc)
    case Expression.InvokeConstructor(constructor, args, tpe, pur, loc) =>
      val as = args.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.InvokeConstructor(constructor, as, t, p, loc)
    case Expression.InvokeMethod(method, exp, args, tpe, pur, loc) =>
      val e = visitExp(exp)
      val as = args.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.InvokeMethod(method, e, as, t, p, loc)
    case Expression.InvokeStaticMethod(method, args, tpe, pur, loc) =>
      val as = args.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.InvokeStaticMethod(method, as, t, p, loc)
    case Expression.GetField(field, exp, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.GetField(field, e, t, p, loc)
    case Expression.PutField(field, exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.PutField(field, e1, e2, t, p, loc)
    case Expression.GetStaticField(field, tpe, pur, loc) =>
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.GetStaticField(field, t, p, loc)
    case Expression.PutStaticField(field, exp, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.PutStaticField(field, e, t, p, loc)
    case Expression.NewObject(name, clazz, tpe, pur, methods, loc) =>
      val t = visitType(tpe)
      val p = visitType(pur)
      val ms = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, pur, loc) =>
          val fs = fparams.map(visitFormalParam)
          val me = visitExp(exp)
          val mt = visitType(retTpe)
          val mp = visitType(pur)
          LoweredAst.JvmMethod(ident, fs, me, mt, mp, loc)
      }
      Expression.NewObject(name, clazz, t, p, ms, loc)
    case Expression.Spawn(exp1, exp2, tpe, pur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Spawn(e1, e2, t, p, loc)
    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expression.Lazy(e, t, loc)
    case Expression.Force(exp, tpe, pur, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(pur)
      Expression.Force(e, t, p, loc)
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
  private def visitFormalParam(p: LoweredAst.FormalParam)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.FormalParam = {
    val LoweredAst.FormalParam(sym, mod, tpe, src, loc) = p
    val t = visitType(tpe)
    LoweredAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Returns a scheme with specialized enums in its base and no aliases.
    */
  private def visitScheme(sc: Scheme)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Scheme = {
    val Scheme(quantifiers, tconstrs, econstrs, base) = sc
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
    val args = args0.map(eraseAliases).map(normalizeType)
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

  /**
    * Converts a type into an equivalent type in normalized form, which will be
    * the same for all other equivalent types.
    *
    * Returns a type where
    * - Formulas in types have been fully evaluated (and ordered in the case of sets)
    * - Types involving rows have been sorted alphabetically (respecting duplicate label ordering)
    * - The assumptions still hold
    *
    * Assumes that
    * - `tpe` is ground (no type variables)
    * - `tpe` has no aliases
    * - `tpe` has no associated types
    */
  private def normalizeType(tpe: Type): Type = tpe match {
    case Type.Var(sym, loc) =>
      throw InternalCompilerException(s"Unexpected type var '$sym'", loc)
    case Type.Cst(_, _) =>
      tpe
    case Type.Apply(tpe1, tpe2, applyLoc) =>
      val t1 = normalizeType(tpe1)
      val t2 = normalizeType(tpe2)
      t1 match {
        // Simplify effect set equations.
        case Type.Cst(TypeConstructor.Complement, _) => t2 match {
          case Type.Pure => Type.EffUniv
          case Type.EffUniv => Type.Pure
          case _ => throw InternalCompilerException(s"Unexpected non-simple effect $tpe", applyLoc)
        }
        case Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _) =>
          (x, t2) match {
            case (Type.Pure, Type.Pure) => Type.Pure
            case (Type.Pure, Type.EffUniv) => Type.EffUniv
            case (Type.EffUniv, Type.Pure) => Type.EffUniv
            case (Type.EffUniv, Type.EffUniv) => Type.EffUniv
            case _ => throw InternalCompilerException(s"Unexpected non-simple effect $tpe", applyLoc)
          }
        case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _) =>
          (x, t2) match {
            case (Type.Pure, Type.Pure) => Type.Pure
            case (Type.Pure, Type.EffUniv) => Type.Pure
            case (Type.EffUniv, Type.Pure) => Type.Pure
            case (Type.EffUniv, Type.EffUniv) => Type.EffUniv
            case _ => throw InternalCompilerException(s"Unexpected non-simple effect $tpe", applyLoc)
          }

        // Simplify boolean equations.
        case Type.Cst(TypeConstructor.Not, _) |
             Type.Apply(Type.Cst(TypeConstructor.And, _), _, _) |
             Type.Apply(Type.Cst(TypeConstructor.Or, _), _, _) =>
          throw InternalCompilerException(s"Unexpected Not/And/Or in formula $tpe", applyLoc)

        // Simplify set expressions
        case Type.Cst(TypeConstructor.CaseComplement(enumSym), _) => t2 match {
          case Type.Cst(TypeConstructor.CaseSet(syms, _), loc) =>
            Type.Cst(TypeConstructor.CaseSet(enumSym.universe.diff(syms), enumSym), loc)
          case _ => throw InternalCompilerException(s"Unexpected non-simple case set formula $tpe", applyLoc)
        }
        case Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(enumSym), _), x, loc) =>
          (x, t2) match {
            case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) =>
              Type.Cst(TypeConstructor.CaseSet(syms1.intersect(syms2), enumSym), loc)
            case _ => throw InternalCompilerException(s"Unexpected non-simple case set formula $tpe", applyLoc)
          }
        case Type.Apply(Type.Cst(TypeConstructor.CaseUnion(enumSym), _), x, loc) =>
          (x, t2) match {
            case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) =>
              Type.Cst(TypeConstructor.CaseSet(syms1.union(syms2), enumSym), loc)
            case _ => throw InternalCompilerException(s"Unexpected non-simple case set formula $tpe", applyLoc)
          }

        // Sort record row fields
        case Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field), _), fieldType, _) =>
          insertRecordField(field, fieldType, t2, applyLoc)

        // Sort schema row fields
        case Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), predType, _) =>
          insertSchemaPred(pred, predType, t2, applyLoc)

        // Else just apply
        case x => Type.Apply(x, t2, applyLoc)
      }
    case Type.Alias(cst, _, _, loc) =>
      throw InternalCompilerException(s"Unexpected type alias: '${cst.sym}'", loc)
    case Type.AssocType(cst, _, _, loc) =>
      throw InternalCompilerException(s"Unexpected associated type: '${cst.sym}'", loc)
  }

  /**
    * Inserts the given field into `rest` in its ordered position, assuming that
    * `rest` is already ordered. This, together with [[normalizeType]]
    * effectively implements insertion sort.
    */
  private def insertRecordField(field: Name.Field, fieldType: Type, rest: Type, loc: SourceLocation): Type = rest match {
    // empty rest, create the singleton record row
    case Type.Cst(TypeConstructor.RecordRowEmpty, emptyLoc) =>
      Type.mkRecordRowExtend(field, fieldType, Type.mkRecordRowEmpty(emptyLoc), loc)
    // the current field should be before the next field and since
    // - we insert from the left, one by one
    // - rest is ordered
    // we can return the current field with the rest
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), _), _, _), _, _) if field.name <= field1.name =>
      Type.mkRecordRowExtend(field, fieldType, rest, loc)
    // The current field should be after the next field, so we swap and continue recursively
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), field1Loc), field1Type, field1TypeLoc), rest1, rest1Loc) =>
      val tail = insertRecordField(field, fieldType, rest1, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), field1Loc), field1Type, field1TypeLoc), tail, rest1Loc)
    case other => throw InternalCompilerException(s"Unexpected record rest: '$other'", rest.loc)
  }

  /**
    * Inserts the given predicate into `rest` in its ordered position, assuming that
    * `rest` is already ordered. This, together with [[normalizeType]]
    * effectively implements insertion sort.
    */
  private def insertSchemaPred(pred: Name.Pred, predType: Type, rest: Type, loc: SourceLocation): Type = rest match {
    // empty rest, create the singleton schema row
    case Type.Cst(TypeConstructor.SchemaRowEmpty, _) =>
      Type.mkSchemaRowExtend(pred, predType, rest, loc)
    // the current pred should be before the next pred and since
    // - we insert from the left, one by one
    // - rest is ordered
    // we can return the current pred with the rest
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred1), _), _, _), _, _) if pred.name <= pred1.name =>
      Type.mkSchemaRowExtend(pred, predType, rest, loc)
    // The current pred should be after the next pred, so we swap and continue recursively
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred1), pred1Loc), pred1Type, pred1TypeLoc), rest1, rest1Loc) =>
      val rest2 = insertSchemaPred(pred, predType, rest1, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred1), pred1Loc), pred1Type, pred1TypeLoc), rest2, rest1Loc)
    case other =>
      throw InternalCompilerException(s"Unexpected record rest: '$other'", rest.loc)
  }

}
