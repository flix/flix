/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.Purity._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

/**
  * A phase that simplifies the LoweredAst by elimination of pattern matching and other rewritings.
  */
object Simplifier {

  type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Def]

  def run(root: LoweredAst.Root)(implicit flix: Flix): SimplifiedAst.Root = flix.phase("Simplifier") {
    //
    // A mutable map to contain fresh top-level definitions.
    //
    val toplevel: TopLevel = mutable.Map.empty

    /**
      * Translates the given definition `def0` to the SimplifiedAst.
      */
    def visitDef(defn: LoweredAst.Def): SimplifiedAst.Def = defn match {
      case LoweredAst.Def(sym, spec, impl) =>
        val fs = spec.fparams.map(visitFormalParam)
        val exp = visitExp(impl.exp)
        val funType = impl.inferredScheme.base
        val retType = funType.arrowResultType
        val eff = funType.arrowEffectType
        SimplifiedAst.Def(spec.ann, spec.mod, sym, fs, exp, retType, eff, sym.loc)
    }

    /**
      * Translates the given expression `exp0` to the SimplifiedAst.
      */
    def visitExp(exp0: LoweredAst.Expr): SimplifiedAst.Expr = exp0 match {
      case LoweredAst.Expr.Var(sym, tpe, loc) => SimplifiedAst.Expr.Var(sym, tpe, loc)

      case LoweredAst.Expr.Def(sym, tpe, loc) => SimplifiedAst.Expr.Def(sym, tpe, loc)

      case LoweredAst.Expr.Cst(cst, tpe, loc) => SimplifiedAst.Expr.Cst(cst, tpe, loc)

      case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp)
        SimplifiedAst.Expr.Lambda(List(p), e, tpe, loc)

      case LoweredAst.Expr.Apply(exp, exps, tpe, eff, loc) =>
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        SimplifiedAst.Expr.Apply(e, es, tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps map visitExp
        val purity = simplifyEffect(eff)
        op match {
          case AtomicOp.Binary(SemanticOp.StringOp.Concat) =>
            // Translate to InvokeMethod exp
            val strClass = Class.forName("java.lang.String")
            val method = strClass.getMethod("concat", strClass)
            SimplifiedAst.Expr.ApplyAtomic(AtomicOp.InvokeMethod(method), es, tpe, purity, loc)

          case AtomicOp.ArrayLit | AtomicOp.ArrayNew =>
            // The region expression is dropped (head of exps / es)
            val es1 = es.tail
            SimplifiedAst.Expr.ApplyAtomic(op, es1, tpe, purity, loc)

          case AtomicOp.Ref =>
            // The region expression is dropped (tail of exps / es)
            val es1 = List(es.head)
            SimplifiedAst.Expr.ApplyAtomic(op, es1, tpe, purity, loc)

          case AtomicOp.Spawn =>
            // Wrap the expression in a closure: () -> tpe \ eff
            val List(e1, e2) = es
            val lambdaTyp = Type.mkArrowWithEffect(Type.Unit, eff, e1.tpe, loc)
            val fp = SimplifiedAst.FormalParam(Symbol.freshVarSym("_spawn", BoundBy.FormalParam, loc), Ast.Modifiers.Empty, Type.mkUnit(loc), loc)
            val lambdaExp = SimplifiedAst.Expr.Lambda(List(fp), e1, lambdaTyp, loc)
            SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(lambdaExp, e2), tpe, Purity.Impure, loc)

          case AtomicOp.Lazy =>
            // Wrap the expression in a closure: () -> tpe \ Pure
            val e = es.head
            val lambdaTyp = Type.mkArrowWithEffect(Type.Unit, Type.Pure, e.tpe, loc)
            val fp = SimplifiedAst.FormalParam(Symbol.freshVarSym("_lazy", BoundBy.FormalParam, loc), Ast.Modifiers.Empty, Type.mkUnit(loc), loc)
            val lambdaExp = SimplifiedAst.Expr.Lambda(List(fp), e, lambdaTyp, loc)
            SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Lazy, List(lambdaExp), tpe, Purity.Pure, loc)

          case AtomicOp.HoleError(_) =>
            // Simplify purity to impure, must be done after Monomorph
            SimplifiedAst.Expr.ApplyAtomic(op, es, tpe, Purity.Impure, loc)

          case _ => SimplifiedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)
        }

      case LoweredAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc) =>
        SimplifiedAst.Expr.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.Stm(e1, e2, tpe, eff, loc) =>
        val sym = Symbol.freshVarSym("_", BoundBy.Let, loc)
        SimplifiedAst.Expr.Let(sym, visitExp(e1), visitExp(e2), tpe, simplifyEffect(eff), loc)

      case d@LoweredAst.Expr.Discard(exp, eff, loc) =>
        val sym = Symbol.freshVarSym("_", BoundBy.Let, loc)
        SimplifiedAst.Expr.Let(sym, visitExp(exp), SimplifiedAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc), d.tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.Let(sym, mod, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expr.Let(sym, visitExp(e1), visitExp(e2), tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.LetRec(sym, mod, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expr.LetRec(sym, visitExp(e1), visitExp(e2), tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
        SimplifiedAst.Expr.Scope(sym, visitExp(exp), tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.Match(exp0, rules, tpe, eff, loc) =>
        patternMatchWithLabels(exp0, rules, tpe, loc)

      case LoweredAst.Expr.RelationalChoose(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Code generation for relational choice is no longer supported", loc)

      case LoweredAst.Expr.VectorLit(exps, tpe, _, loc) =>
        // Note: We simplify Vectors to Arrays.
        val es = exps.map(visitExp)
        SimplifiedAst.Expr.ApplyAtomic(AtomicOp.ArrayLit, es, tpe, Purity.Pure, loc)

      case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, _, loc) =>
        // Note: We simplify Vectors to Arrays.
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expr.ApplyAtomic(AtomicOp.ArrayLoad, List(e1, e2), tpe, Purity.Pure, loc)

      case LoweredAst.Expr.VectorLength(exp, loc) =>
        // Note: We simplify Vectors to Arrays.
        val e = visitExp(exp)
        val purity = e.purity
        SimplifiedAst.Expr.ApplyAtomic(AtomicOp.ArrayLength, List(e), Type.Int32, purity, loc)

      case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) => visitExp(exp)

      case LoweredAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Cast, List(e), tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case LoweredAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expr.TryCatch(e, rs, tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case LoweredAst.HandlerRule(sym, fparams, body) =>
            val fps = fparams.map(visitFormalParam)
            val b = visitExp(body)
            SimplifiedAst.HandlerRule(sym, fps, b)
        }
        SimplifiedAst.Expr.TryWith(e, effUse, rs, tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        SimplifiedAst.Expr.Do(op, es, tpe, simplifyEffect(eff), loc)

      case LoweredAst.Expr.Resume(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expr.Resume(e, tpe, loc)

      case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
        val methods = methods0 map visitJvmMethod
        SimplifiedAst.Expr.NewObject(name, clazz, tpe, simplifyEffect(eff), methods, loc)

      case LoweredAst.Expr.Sig(_, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)

      case LoweredAst.Expr.TypeMatch(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)
    }

    /**
      * Translates the given formal param `p` to the SimplifiedAst.
      */
    def visitFormalParam(p: LoweredAst.FormalParam): SimplifiedAst.FormalParam =
      SimplifiedAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

    /**
      * Translates the given JvmMethod `method` to the SimplifiedAst
      */
    def visitJvmMethod(method: LoweredAst.JvmMethod): SimplifiedAst.JvmMethod = method match {
      case LoweredAst.JvmMethod(ident, fparams0, exp0, retTpe, eff, loc) =>
        val fparams = fparams0 map visitFormalParam
        val exp = visitExp(exp0)
        SimplifiedAst.JvmMethod(ident, fparams, exp, retTpe, simplifyEffect(eff), loc)
    }

    /**
      * Returns the given pattern `pat0` as an expression.
      */
    def pat2exp(pat0: LoweredAst.Pattern): SimplifiedAst.Expr = pat0 match {
      case LoweredAst.Pattern.Cst(cst, tpe, loc) => SimplifiedAst.Expr.Cst(cst, tpe, loc)
      case LoweredAst.Pattern.Tag(Ast.CaseSymUse(sym, _), p, tpe, loc) =>
        val e = pat2exp(p)
        SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Tag(sym), List(e), tpe, e.purity, loc)
      case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
        val es = elms.map(pat2exp)
        val purity = combineAll(es.map(_.purity))
        SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Tuple, es, tpe, purity, loc)
      case _ => throw InternalCompilerException(s"Unexpected non-literal pattern $pat0.", pat0.loc)
    }

    /**
      * Returns `true` if the given pattern `pat0` is a literal.
      */
    def isPatLiteral(pat0: LoweredAst.Pattern): Boolean = pat0 match {
      case LoweredAst.Pattern.Cst(_, _, _) => true
      case _ => false
    }

    /**
      * Returns an expression that compares the two given expressions `e1` and `e2` for equality.
      */
    def mkEqual(e1: SimplifiedAst.Expr, e2: SimplifiedAst.Expr, loc: SourceLocation): SimplifiedAst.Expr = {
      /*
       * Special Case 1: Unit
       * Special Case 2: String - must be desugared to String.equals
       */
      (e1.tpe.typeConstructor, e2.tpe.typeConstructor) match {
        case (Some(TypeConstructor.Unit), Some(TypeConstructor.Unit)) =>
          // Unit is always equal to itself.
          return SimplifiedAst.Expr.Cst(Ast.Constant.Bool(true), Type.Bool, loc)

        case (Some(TypeConstructor.Str), _) =>
          val strClass = Class.forName("java.lang.String")
          val objClass = Class.forName("java.lang.Object")
          val method = strClass.getMethod("equals", objClass)
          val op = AtomicOp.InvokeMethod(method)
          return SimplifiedAst.Expr.ApplyAtomic(op, List(e1, e2), Type.Bool, combine(e1.purity, e2.purity), loc)

        case _ => // fallthrough
      }

      /*
       * Compute the semantic operator.
       */
      val sop = e1.tpe.typeConstructor match {
        case Some(TypeConstructor.Bool) => SemanticOp.BoolOp.Eq
        case Some(TypeConstructor.Char) => SemanticOp.CharOp.Eq
        case Some(TypeConstructor.Float32) => SemanticOp.Float32Op.Eq
        case Some(TypeConstructor.Float64) => SemanticOp.Float64Op.Eq
        case Some(TypeConstructor.Int8) => SemanticOp.Int8Op.Eq
        case Some(TypeConstructor.Int16) => SemanticOp.Int16Op.Eq
        case Some(TypeConstructor.Int32) => SemanticOp.Int32Op.Eq
        case Some(TypeConstructor.Int64) => SemanticOp.Int64Op.Eq
        case t => throw InternalCompilerException(s"Unexpected type: '$t'.", e1.loc)
      }
      val purity = combine(e1.purity, e2.purity)
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), Type.Bool, purity, loc)
    }

    /**
      * Eliminates pattern matching by translations to labels and jumps.
      */
    def patternMatchWithLabels(exp0: LoweredAst.Expr, rules: List[LoweredAst.MatchRule], tpe: Type, loc: SourceLocation): SimplifiedAst.Expr = {
      //
      // Given the code:
      //
      // match x {
      //   case PATTERN_1 => BODY_1
      //   case PATTERN_2 => BODY_2
      //   ...
      //   case PATTERN_N => BODY_N
      // }
      //
      // The structure of the generated code is as follows:
      //
      // let matchVar = x ;
      //
      //   branch {
      //     jumpto label$1
      //
      //     label$1:
      //       ...
      //     label$2:
      //       ...
      //     default:
      //       MatchError
      //   }
      //

      // Generate a fresh variable to hold the result of the match expression.
      val matchVar = Symbol.freshVarSym("matchVar" + Flix.Delimiter, BoundBy.Let, loc)

      // Translate the match expression.
      val matchExp = visitExp(exp0)

      // Generate a fresh label for the default fall through case.
      val defaultLab = Symbol.freshLabel("default")

      // Generate a label for each rule.
      val ruleLabels = rules.map(_ => Symbol.freshLabel("case"))

      // Construct a map from each label to the label of the next case.
      // The default label is the next label of the last case.
      val nextLabel = (ruleLabels zip (ruleLabels.drop(1) ::: defaultLab :: Nil)).toMap

      //TODO Intermediate solution (which is correct, but imprecise): Compute the purity of every match rule in rules
      val jumpPurity = combineAll(rules.map(r => simplifyEffect(r.exp.eff)))

      // Create a branch for each rule.
      val branches = (ruleLabels zip rules) map {
        // Process each (label, rule) pair.
        case (field, LoweredAst.MatchRule(pat, guard, body)) =>
          // Retrieve the label of the next rule.
          // If this rule is the last, the next label is the default label.
          val next = nextLabel(field)

          // Success case: evaluate the match body.
          val success = visitExp(body)

          // Failure case: Jump to the next label.
          val failure = SimplifiedAst.Expr.JumpTo(next, tpe, jumpPurity, loc)

          // Return the branch with its label.
          field -> patternMatchList(List(pat), List(matchVar), guard.getOrElse(LoweredAst.Expr.Cst(Ast.Constant.Bool(true), Type.Bool, SourceLocation.Unknown)), success, failure
          )
      }
      // Construct the error branch.
      val errorExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.MatchError, List.empty, tpe, Purity.Impure, loc)
      val errorBranch = defaultLab -> errorExp

      // The initial expression simply jumps to the first label.
      val entry = SimplifiedAst.Expr.JumpTo(ruleLabels.head, tpe, jumpPurity, loc)

      // The purity of the branch
      val branchPurity = combineAll(branches.map { case (_, exp) => exp.purity })

      // Assemble all the branches together.
      val branch = SimplifiedAst.Expr.Branch(entry, branches.toMap + errorBranch, tpe, branchPurity, loc)

      // The purity of the match exp
      val matchPurity = combine(matchExp.purity, branch.purity)

      // Wrap the branches inside a let-binding for the match variable.
      SimplifiedAst.Expr.Let(matchVar, matchExp, branch, tpe, matchPurity, loc)
    }

    /**
      * Returns an expression that matches the given list of patterns `xs` against the given list of variables `ys`.
      *
      * Checks the `guard` when all patterns have been matched.
      *
      * Evaluates `succ` on success and `fail` otherwise.
      */
    def patternMatchList(xs: List[LoweredAst.Pattern], ys: List[Symbol.VarSym], guard: LoweredAst.Expr, succ: SimplifiedAst.Expr, fail: SimplifiedAst.Expr): SimplifiedAst.Expr =
      ((xs, ys): @unchecked) match {
        /**
          * There are no more patterns and variables to match.
          *
          * The pattern was match successfully. Test the guard.
          */
        case (Nil, Nil) =>
          val g = visitExp(guard)
          SimplifiedAst.Expr.IfThenElse(g, succ, fail, succ.tpe, g.purity, g.loc)

        /**
          * Matching a wildcard is guaranteed to succeed.
          *
          * We proceed by recursion on the remaining patterns and variables.
          */
        case (LoweredAst.Pattern.Wild(tpe, loc) :: ps, v :: vs) =>
          patternMatchList(ps, vs, guard, succ, fail)

        /**
          * Matching a variable is guaranteed to succeed.
          *
          * We proceed by constructing a let-binding that binds the value
          * of the match variable `ident` to the variable `v`.
          * The body of the let-binding is computed by recursion on the
          * remaining patterns and variables.
          */
        case (LoweredAst.Pattern.Var(sym, tpe, loc) :: ps, v :: vs) =>
          val exp = patternMatchList(ps, vs, guard, succ, fail)
          SimplifiedAst.Expr.Let(sym, SimplifiedAst.Expr.Var(v, tpe, loc), exp, succ.tpe, exp.purity, loc)

        /**
          * Matching a literal may succeed or fail.
          *
          * We generate a binary expression testing whether the literal `lit`
          * matches the variable `v` and then we generate an if-then-else
          * expression where the consequent expression is determined by
          * recursion on the remaining patterns and variables and the
          * alternative expression is `fail`.
          */
        case (lit :: ps, v :: vs) if isPatLiteral(lit) =>
          val exp = patternMatchList(ps, vs, guard, succ, fail)
          val cond = mkEqual(pat2exp(lit), SimplifiedAst.Expr.Var(v, lit.tpe, lit.loc), lit.loc)
          val purity = combine(cond.purity, exp.purity, fail.purity)
          SimplifiedAst.Expr.IfThenElse(cond, exp, fail, succ.tpe, purity, lit.loc)

        /**
          * Matching a tag may succeed or fail.
          *
          * We generate a binary expression testing whether the tag name `tag`
          * matches the tag extracted from the variable `v` and then we generate
          * an if-then-else expression where the consequent expression is determined
          * by recursion on the remaining patterns and variables together with the
          * nested pattern of the tag added in front and a new fresh variable holding
          * the value of the tag.
          */
        case (LoweredAst.Pattern.Tag(Ast.CaseSymUse(sym, _), pat, tpe, loc) :: ps, v :: vs) =>
          val varExp = SimplifiedAst.Expr.Var(v, tpe, loc)
          val cond = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Is(sym), List(varExp), Type.Bool, Pure, loc)
          val freshVar = Symbol.freshVarSym("innerTag" + Flix.Delimiter, BoundBy.Let, loc)
          val inner = patternMatchList(pat :: ps, freshVar :: vs, guard, succ, fail)
          val purity1 = inner.purity
          val untagExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Untag(sym), List(varExp), pat.tpe, purity1, loc)
          val consequent = SimplifiedAst.Expr.Let(freshVar, untagExp, inner, succ.tpe, purity1, loc)
          val purity2 = combine(cond.purity, consequent.purity, fail.purity)
          SimplifiedAst.Expr.IfThenElse(cond, consequent, fail, succ.tpe, purity2, loc)

        /**
          * Matching a tuple may succeed or fail.
          *
          * We generate a fresh variable and let-binding for each component of the
          * tuple and then we recurse on the nested patterns and freshly generated
          * variables.
          */
        case (LoweredAst.Pattern.Tuple(elms, tpe, loc) :: ps, v :: vs) =>
          val freshVars = elms.map(_ => Symbol.freshVarSym("innerElm" + Flix.Delimiter, BoundBy.Let, loc))
          val zero = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
          elms.zip(freshVars).zipWithIndex.foldRight(zero) {
            case (((pat, name), idx), exp) =>
              val varExp = SimplifiedAst.Expr.Var(v, tpe, loc)
              val indexExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Index(idx), List(varExp), pat.tpe, Pure, loc)
              SimplifiedAst.Expr.Let(name, indexExp, exp, succ.tpe, exp.purity, loc)
          }

        /**
          * Matching a record may succeed or fail.
          *
          * We generate a fresh variable and let-binding for each component of the
          * record (field or extension) and then we recurse on the nested patterns
          * and freshly generated variables.
          */
        case (LoweredAst.Pattern.Record(pats, pat, tpe, loc) :: ps, v :: vs) =>
          val freshVars = pats.map(_ => Symbol.freshVarSym("innerField" + Flix.Delimiter, BoundBy.Let, loc))
          val fieldPats = pats.map(_.pat)
          val zero = patternMatchList(fieldPats ::: ps, freshVars ::: vs, guard, succ, fail)
          val one = pats.zip(freshVars).foldRight(zero) {
            case ((LoweredAst.Pattern.Record.RecordFieldPattern(field, _, pat, loc1), name), exp) =>
              val varExp = SimplifiedAst.Expr.Var(v, tpe, loc)
              val recordSelectExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.RecordSelect(field), List(varExp), pat.tpe, Pure, loc1)
              SimplifiedAst.Expr.Let(name, recordSelectExp, exp, succ.tpe, exp.purity, loc1)
          }
          pat match {
            case Some(LoweredAst.Pattern.Var(sym, _, varLoc)) =>
              // Extension is { ... | sym } so we generate a let-binding `let sym = matchVar`
              SimplifiedAst.Expr.Let(sym, SimplifiedAst.Expr.Var(v, tpe, loc), one, succ.tpe, one.purity, varLoc)
            case _ =>
              // Extension is either wild or non-existent
              one
          }

        case p => throw InternalCompilerException(s"Unsupported pattern '$p'.", xs.head.loc)
      }

    //
    // Main computation.
    //
    val defns = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map {
      case (k, LoweredAst.Enum(_, ann, mod, sym, _, _, cases0, enumType, loc)) =>
        val cases = cases0 map {
          case (tag, LoweredAst.Case(caseSym, tagType, _, tagLoc)) => tag -> SimplifiedAst.Case(caseSym, tagType, tagLoc)
        }
        k -> SimplifiedAst.Enum(ann, mod, sym, cases, enumType, loc)
    }

    SimplifiedAst.Root(defns ++ toplevel, enums, root.entryPoint, root.sources)
  }

  /**
    * Returns a copy of the given expression `exp0` where every variable symbol has been replaced according to the given substitution `m`.
    */
  def substitute(exp0: SimplifiedAst.Expr, m: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.Expr = {

    def visitExp(e: SimplifiedAst.Expr): SimplifiedAst.Expr = e match {
      case SimplifiedAst.Expr.Cst(_, _, _) => e

      case SimplifiedAst.Expr.Var(sym, tpe, loc) => m.get(sym) match {
        case None => SimplifiedAst.Expr.Var(sym, tpe, loc)
        case Some(replacement) => SimplifiedAst.Expr.Var(replacement, tpe, loc)
      }

      case SimplifiedAst.Expr.Def(sym, tpe, loc) => e

      case SimplifiedAst.Expr.Lambda(fparams, body, tpe, loc) =>
        SimplifiedAst.Expr.Lambda(fparams, visitExp(body), tpe, loc)

      case SimplifiedAst.Expr.Apply(exp, args, tpe, purity, loc) =>
        SimplifiedAst.Expr.Apply(visitExp(exp), args.map(visitExp), tpe, purity, loc)

      case SimplifiedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps map visitExp
        SimplifiedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case SimplifiedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        SimplifiedAst.Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, purity, loc)

      case SimplifiedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> br
        }
        SimplifiedAst.Expr.Branch(e, bs, tpe, purity, loc)

      case SimplifiedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        SimplifiedAst.Expr.JumpTo(sym, tpe, purity, loc)

      case SimplifiedAst.Expr.Let(sym, exp1, exp2, purity, tpe, loc) =>
        SimplifiedAst.Expr.Let(sym, visitExp(exp1), visitExp(exp2), purity, tpe, loc)

      case SimplifiedAst.Expr.LetRec(sym, exp1, exp2, tpe, purity, loc) =>
        SimplifiedAst.Expr.LetRec(sym, visitExp(exp1), visitExp(exp2), tpe, purity, loc)

      case SimplifiedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
        SimplifiedAst.Expr.Scope(sym, visitExp(exp), tpe, purity, loc)

      case SimplifiedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

      case SimplifiedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.HandlerRule(sym, fparams, body) =>
            val b = visitExp(body)
            SimplifiedAst.HandlerRule(sym, fparams, b)
        }
        SimplifiedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

      case SimplifiedAst.Expr.Do(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExp)
        SimplifiedAst.Expr.Do(op, es, tpe, purity, loc)

      case SimplifiedAst.Expr.Resume(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expr.Resume(e, tpe, loc)

      case SimplifiedAst.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
        val methods = methods0 map visitJvmMethod
        SimplifiedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

      case SimplifiedAst.Expr.LambdaClosure(_, _, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expr.ApplyClo(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expr.ApplyDef(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
    }

    def visitJvmMethod(method: SimplifiedAst.JvmMethod) = method match {
      case SimplifiedAst.JvmMethod(ident, fparams, exp, tpe, purity, loc) =>
        SimplifiedAst.JvmMethod(ident, fparams, visitExp(exp), tpe, purity, loc)
    }

    visitExp(exp0)
  }

  /**
    * Returns the purity (or impurity) of an expression.
    */
  private def simplifyEffect(eff: Type): Purity = {
    if (eff == Type.Pure)
      Pure
    else
      Impure
  }

  /**
    * Combines purities `p1` and `p2`
    * A combined purity is only pure if both `p1` and `p2` are pure, otherwise it is always impure.
    */
  def combine(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Pure, Pure) => Pure
    case _ => Impure
  }

  /**
    * Combine `p1`, `p2` and `p3`
    * A combined purity is only pure if `p1`, `p2` and `p3` are pure, otherwise it is always impure.
    */
  def combine(p1: Purity, p2: Purity, p3: Purity): Purity = combine(p1, combine(p2, p3))

  /**
    * Combine `purities`
    * A combined purity is only pure if every purity in `purities` are pure, otherwise it is always impure.
    */
  def combineAll(purities: List[Purity]): Purity = purities.foldLeft[Purity](Pure) {
    case (acc, purity) => combine(acc, purity)
  }
}
