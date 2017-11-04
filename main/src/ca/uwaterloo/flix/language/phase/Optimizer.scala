/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol, Type}
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.{InternalCompilerException, Optimization, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt._

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of dead branches (e.g. if (true) e1 else e2).
  * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
  * - Elimination of single-case enums.
  */
object Optimizer extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    /**
      * Performs intra-procedural optimization on the given expression `exp0` and substitution map `env0`.
      */
    def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = exp0 match {
      // 
      // Literal Expressions.
      //
      case Expression.Unit => exp0
      case Expression.True => exp0
      case Expression.False => exp0
      case Expression.Char(lit) => exp0
      case Expression.Float32(lit) => exp0
      case Expression.Float64(lit) => exp0
      case Expression.Int8(lit) => exp0
      case Expression.Int16(lit) => exp0
      case Expression.Int32(lit) => exp0
      case Expression.Int64(lit) => exp0
      case Expression.BigInt(lit) => exp0
      case Expression.Str(lit) => exp0

      //
      // Variable Expressions.
      //
      case Expression.Var(sym, tpe, loc) =>
        // Lookup to see if the variable should be replaced by a copy.
        env0.get(sym) match {
          case None => Expression.Var(sym, adjustType(tpe), loc)
          case Some(srcSym) => Expression.Var(srcSym, adjustType(tpe), loc)
        }

      //
      // Def Expressions.
      //
      case Expression.Def(sym, tpe, loc) => Expression.Def(sym, adjustType(tpe), loc)

      //
      // Closure Expressions.
      //
      case Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars map {
          case FreeVar(s, varType) => FreeVar(env0.getOrElse(s, s), adjustType(varType))
        }
        Expression.Closure(sym, fvs, adjustType(tpe), loc)

      //
      // ApplyClo Expressions.
      //
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        val as = args map (visitExp(_, env0))
        Expression.ApplyClo(e, as, adjustType(tpe), loc)

      //
      // ApplyDef Expressions.
      //
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyDef(sym, as, adjustType(tpe), loc)

      //
      // ApplyCloTail Expressions.
      //
      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        val as = args map (visitExp(_, env0))
        Expression.ApplyCloTail(e, as, adjustType(tpe), loc)

      //
      // ApplyDefTail Expressions.
      //
      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyDefTail(sym, as, adjustType(tpe), loc)

      //
      // ApplySelfTail Expressions.
      //
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        val fs = formals map adjustFormalParam
        val as = actuals map (visitExp(_, env0))
        Expression.ApplySelfTail(sym, fs, as, adjustType(tpe), loc)

      //
      // ApplyHook Expressions.
      //
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyHook(hook, as, adjustType(tpe), loc)

      //
      // Unary Expressions.
      //
      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Unary(sop, op, e, adjustType(tpe), loc)

      //
      // Binary Expressions.
      //
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Binary(sop, op, e1, e2, adjustType(tpe), loc)

      //
      // If-then-else Expressions.
      //
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Eliminate dead branches, if possible.
        val cond = visitExp(exp1, env0)
        val consequent = visitExp(exp2, env0)
        val alternative = visitExp(exp3, env0)
        cond match {
          case Expression.True => consequent
          case Expression.False => alternative
          case _ => Expression.IfThenElse(cond, consequent, alternative, adjustType(tpe), loc)
        }

      //
      // Block Expressions.
      //
      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp, env0)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br, env0)
        }
        Expression.Branch(e, bs, adjustType(tpe), loc)

      //
      // Jump Expressions.
      //
      case Expression.JumpTo(sym, tpe, loc) =>
        Expression.JumpTo(sym, adjustType(tpe), loc)

      //
      // Let Expressions.
      //
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        // Visit the value expression.
        val e1 = visitExp(exp1, env0)

        // Check for copy propagation: let x = y; e ~~> e[x -> y]
        e1 match {
          case Expression.Var(srcSym, _, _) =>
            // The srcSym might itself originate from some other symbol.
            val originalSym = env0.getOrElse(srcSym, srcSym)
            visitExp(exp2, env0 + (sym -> originalSym))
          case _ =>
            val e2 = visitExp(exp2, env0)
            Expression.Let(sym, e1, e2, adjustType(tpe), loc)
        }

      //
      // LetRec Expressions.
      //
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.LetRec(sym, e1, e2, adjustType(tpe), loc)

      //
      // Is Expressions.
      //
      case Expression.Is(sym, tag, exp, loc) =>
        //
        // Check if this is a single-case enum subject to elimination.
        //
        if (isSingleCaseEnum(sym)) {
          if (flix.options.optimizations contains Optimization.SingleCaseEnums) {
            // A single-case enum only has one tag.
            return Expression.True
          }
        }

        // Otherwise keep the is.
        val e = visitExp(exp, env0)
        Expression.Is(sym, tag, e, loc)

      //
      // Tag Expressions.
      //
      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        //
        // Check if this is a single-case enum subject to elimination.
        //
        if (isSingleCaseEnum(sym)) {
          if (flix.options.optimizations contains Optimization.SingleCaseEnums) {
            return visitExp(exp, env0)
          }
        }

        // Otherwise keep the tag.
        val e = visitExp(exp, env0)
        Expression.Tag(sym, tag, e, adjustType(tpe), loc)

      //
      // Check if this is a single-case enum subject to elimination.
      //
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        //
        // Check if NewType optimization is enabled and if this is a single-case enum.
        //
        if (isSingleCaseEnum(sym)) {
          if (flix.options.optimizations contains Optimization.SingleCaseEnums) {
            return visitExp(exp, env0)
          }
        }

        // Otherwise keep the untag.
        val e = visitExp(exp, env0)
        Expression.Untag(sym, tag, e, adjustType(tpe), loc)

      //
      // Index Expressions.
      //
      case Expression.Index(base, offset, tpe, loc) =>
        val b = visitExp(base, env0)
        Expression.Index(b, offset, adjustType(tpe), loc)

      //
      // Tuple Expressions.
      //
      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map (visitExp(_, env0))
        Expression.Tuple(es, adjustType(tpe), loc)

      //
      // Reference Expressions.
      //
      case Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Ref(e, adjustType(tpe), loc)

      //
      // Dereference Expressions.
      //
      case Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Deref(e, adjustType(tpe), loc)

      //
      // Assign Expressions.
      //
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Assign(e1, e2, adjustType(tpe), loc)

      //
      // Existential Expressions.
      //
      case Expression.Existential(fparam, exp, loc) =>
        val p = adjustFormalParam(fparam)
        val e = visitExp(exp, env0)
        Expression.Existential(p, e, loc)

      //
      // Universal Expressions.
      //
      case Expression.Universal(fparam, exp, loc) =>
        val p = adjustFormalParam(fparam)
        val e = visitExp(exp, env0)
        Expression.Universal(p, e, loc)

      //
      // Native Constructor.
      //
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.NativeConstructor(constructor, as, adjustType(tpe), loc)

      //
      // Native Field.
      //
      case Expression.NativeField(field, tpe, loc) => Expression.NativeField(field, adjustType(tpe), loc)

      //
      // Native Method.
      //
      case Expression.NativeMethod(method, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.NativeMethod(method, as, adjustType(tpe), loc)

      //
      // Error Expressions.
      //
      case Expression.UserError(tpe, loc) => Expression.UserError(adjustType(tpe), loc)
      case Expression.MatchError(tpe, loc) => Expression.MatchError(adjustType(tpe), loc)
      case Expression.SwitchError(tpe, loc) => Expression.SwitchError(adjustType(tpe), loc)

      //
      // Unexpected Expressions.
      //
      case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Hook(hook, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }

    /**
      * Performs intra-procedural optimization on the terms of the given head predicate `p0`.
      */
    def visitHeadPred(p0: Predicate.Head): Predicate.Head = p0 match {
      case Predicate.Head.True(loc) => p0
      case Predicate.Head.False(loc) => p0
      case Predicate.Head.Atom(sym, terms, loc) => Predicate.Head.Atom(sym, terms map visitHeadTerm, loc)
    }

    /**
      * Performs intra-procedural optimization on the terms of the given body predicate `p0`.
      */
    def visitBodyPred(p0: Predicate.Body): Predicate.Body = p0 match {
      case Predicate.Body.Atom(sym, polarity, terms, loc) => Predicate.Body.Atom(sym, polarity, terms map visitBodyTerm, loc)
      case Predicate.Body.Filter(sym, terms, loc) => Predicate.Body.Filter(sym, terms map visitBodyTerm, loc)
      case Predicate.Body.Loop(sym, term, loc) => Predicate.Body.Loop(sym, visitHeadTerm(term), loc)
    }

    /**
      * Performs intra-procedural optimization on the given head term `t0`.
      */
    def visitHeadTerm(h0: Term.Head): Term.Head = h0 match {
      case Term.Head.Var(sym, tpe, loc) => Term.Head.Var(sym, adjustType(tpe), loc)
      case Term.Head.Lit(lit, tpe, loc) => Term.Head.Lit(visitExp(lit, Map.empty), adjustType(tpe), loc)
      case Term.Head.App(sym, args, tpe, loc) => Term.Head.App(sym, args, adjustType(tpe), loc)
    }

    /**
      * Performs intra-procedural optimization on the given body term `t0`.
      */
    def visitBodyTerm(b0: Term.Body): Term.Body = b0 match {
      case Term.Body.Wild(tpe, loc) => Term.Body.Wild(adjustType(tpe), loc)
      case Term.Body.Var(sym, tpe, loc) => Term.Body.Var(sym, adjustType(tpe), loc)
      case Term.Body.Lit(exp, tpe, loc) => Term.Body.Lit(visitExp(exp, Map.empty), adjustType(tpe), loc)
      case Term.Body.Pat(pat, tpe, loc) => Term.Body.Pat(adjustPat(pat), adjustType(tpe), loc)
    }

    /**
      * Returns `true` if the enum associated with the given symbol `sym` is a single-case enum.
      */
    def isSingleCaseEnum(sym: Symbol.EnumSym): Boolean = {
      // An enum is single-cased if it has a single case and is non-polymorphic.
      val isSingleCased = root.enums(sym).cases.size == 1
      val isPolymorphic = root.enums(sym).tpe.typeArguments.nonEmpty
      isSingleCased && !isPolymorphic
    }

    /**
      * Returns the inner type of the given single-case enum associated with the symbol `sym`.
      */
    def getSingleCaseType(sym: Symbol.EnumSym): Type = {
      // Retrieve the enum declaration.
      val enum = root.enums(sym)

      // Retrieve the case.
      enum.cases.head match {
        case (_, SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)) => tagType
      }
    }

    /**
      * Adjusts the type of `p0` to remove any references to a single-case enum.
      */
    def adjustConstraintParam(p0: SimplifiedAst.ConstraintParam): ConstraintParam = p0 match {
      case ConstraintParam.HeadParam(sym, tpe, loc) => ConstraintParam.HeadParam(sym, adjustType(tpe), loc)
      case ConstraintParam.RuleParam(sym, tpe, loc) => ConstraintParam.RuleParam(sym, adjustType(tpe), loc)
    }

    /**
      * Adjusts the type of `p0` to remove any references to a single-case enum.
      */
    def adjustFormalParam(p0: SimplifiedAst.FormalParam): FormalParam = p0 match {
      case FormalParam(sym, mod, tpe, loc) => FormalParam(sym, mod, adjustType(tpe), loc)
    }

    /**
      * Adjusts the type of `p0` to remove any references to a single-case enum.
      */
    def adjustPat(p0: Pattern): Pattern = p0 match {
      case Pattern.Wild(tpe, loc) => Pattern.Wild(adjustType(tpe), loc)
      case Pattern.Var(sym, tpe, loc) => Pattern.Var(sym, adjustType(tpe), loc)
      case Pattern.Unit(loc) => p0
      case Pattern.True(loc) => p0
      case Pattern.False(loc) => p0
      case Pattern.Char(lit, loc) => p0
      case Pattern.Float32(lit, loc) => p0
      case Pattern.Float64(lit, loc) => p0
      case Pattern.Int8(lit, loc) => p0
      case Pattern.Int16(lit, loc) => p0
      case Pattern.Int32(lit, loc) => p0
      case Pattern.Int64(lit, loc) => p0
      case Pattern.BigInt(lit, loc) => p0
      case Pattern.Str(lit, loc) => p0
      case Pattern.Tag(sym, tag, pat, tpe, loc) =>
        //
        // Check if this is a single-case enum subject to elimination.
        //
        if (isSingleCaseEnum(sym)) {
          if (flix.options.optimizations contains Optimization.SingleCaseEnums) {
            return adjustPat(pat)
          }
        }
        val p = adjustPat(pat)
        Pattern.Tag(sym, tag, p, adjustType(tpe), loc)

      case Pattern.Tuple(elms, tpe, loc) =>
        val es = elms map adjustPat
        Pattern.Tuple(es, adjustType(tpe), loc)
    }

    /**
      * Adjusts the given `tpe0` to remove any references to a single-case enum.
      *
      * NB: Returns the same type if the single-case enum elimination optimization is not enabled.
      */
    def adjustType(tpe0: Type): Type = {
      // Returns the type if single-case elimination is disabled.
      if (!(flix.options.optimizations contains Optimization.SingleCaseEnums)) {
        return tpe0
      }

      // Adjust the type.
      tpe0 match {
        // Check if the enum is single-case.
        case Type.Enum(sym, kind) if isSingleCaseEnum(sym) => adjustType(getSingleCaseType(sym))
        case Type.Apply(tpe1, tpe2) => tpe1 match {
          // NB: This case is necessary if the single-case enum is polymorphic.
          case Type.Enum(sym, kind) if isSingleCaseEnum(sym) => adjustType(getSingleCaseType(sym))
          case _ => Type.Apply(adjustType(tpe1), adjustType(tpe2))
        }
        case _ => tpe0
      }
    }

    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) =>
        val ps = defn.fparams.map(adjustFormalParam)
        val exp = visitExp(defn.exp, Map.empty)
        val tpe = adjustType(defn.tpe)
        sym -> defn.copy(fparams = ps, exp = exp, tpe = tpe)
    }

    // Visit every enum in the program.
    val enums = root.enums.map {
      case (sym, enum) =>
        val cases = enum.cases map {
          case (tag, caze) => tag -> caze.copy(tpe = adjustType(caze.tpe))
        }
        sym -> enum.copy(cases = cases)
    }

    // Visit every lattice in the program.
    val lattices = root.lattices.map {
      case (tpe, Lattice(t, bot0, top0, equ0, leq0, lub0, glb0, loc)) =>
        val bot = visitExp(bot0, Map.empty)
        val top = visitExp(top0, Map.empty)
        val equ = visitExp(equ0, Map.empty)
        val leq = visitExp(leq0, Map.empty)
        val lub = visitExp(lub0, Map.empty)
        val glb = visitExp(glb0, Map.empty)
        // TODO: We need to compile the lattice abstraction away and instead equip the attributes with references to the definition symbols.
        adjustType(tpe) -> SimplifiedAst.Lattice(adjustType(tpe), bot, top, equ, leq, lub, glb, loc)
    }

    // Visit every stratum in the program.
    val strata = root.strata.map {
      case Stratum(constraints) =>
        val cs = constraints map {
          case Constraint(cparams, head, body) =>
            Constraint(cparams map adjustConstraintParam, visitHeadPred(head), body map visitBodyPred)
        }
        Stratum(cs)
    }

    // Visit every property in the program.
    val properties = root.properties.map {
      case property =>
        val exp = visitExp(property.exp, Map.empty)
        property.copy(exp = exp)
    }

    // TODO: Unsoundness due to use of types as keys.
    // We need to compile the specialOps away and instead equip each attribute with the definition symbols of the equality operators.
    // Similarly for toString, although that might be more tricky.

    // Reassemble the ast root.
    val result = root.copy(
      defs = defs,
      enums = enums,
      lattices = lattices,
      strata = strata,
      properties = properties
    )

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Simplified.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
    }

    result.toSuccess
  }

}
