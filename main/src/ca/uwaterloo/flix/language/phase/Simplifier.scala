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
    def visitDef(def0: LoweredAst.Def): SimplifiedAst.Def = {
      val fs = def0.spec.fparams.map(visitFormalParam)
      val exp = visitExp(def0.impl.exp)
      SimplifiedAst.Def(def0.spec.ann, def0.spec.mod, def0.sym, fs, exp, def0.impl.inferredScheme.base, def0.sym.loc)
    }

    /**
      * Translates the given expression `exp0` to the SimplifiedAst.
      */
    def visitExp(exp0: LoweredAst.Expression): SimplifiedAst.Expression = exp0 match {
      case LoweredAst.Expression.Var(sym, tpe, loc) => SimplifiedAst.Expression.Var(sym, tpe, loc)

      case LoweredAst.Expression.Def(sym, tpe, loc) => SimplifiedAst.Expression.Def(sym, tpe, loc)

      case LoweredAst.Expression.Hole(sym, tpe, loc) => SimplifiedAst.Expression.HoleError(sym, tpe, loc)

      case LoweredAst.Expression.Cst(cst, tpe, loc) => SimplifiedAst.Expression.Cst(cst, tpe, loc)

      case LoweredAst.Expression.Lambda(fparam, exp, tpe, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Lambda(List(p), e, tpe, loc)

      case LoweredAst.Expression.Apply(exp, exps, tpe, pur, loc) =>
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        SimplifiedAst.Expression.Apply(e, es, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Unary(sop, e, tpe, pur, loc) =>
        SimplifiedAst.Expression.Unary(sop, visitExp(e), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Binary(sop, e1, e2, tpe, pur, loc) =>
        SimplifiedAst.Expression.Binary(sop, visitExp(e1), visitExp(e2), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.IfThenElse(e1, e2, e3, tpe, pur, loc) =>
        SimplifiedAst.Expression.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Stm(e1, e2, tpe, pur, loc) =>
        val sym = Symbol.freshVarSym("_", BoundBy.Let, loc)
        SimplifiedAst.Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, simplifyPurity(pur), loc)

      case d@LoweredAst.Expression.Discard(exp, pur, loc) =>
        val sym = Symbol.freshVarSym("_", BoundBy.Let, loc)
        SimplifiedAst.Expression.Let(sym, visitExp(exp), SimplifiedAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, loc), d.tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Let(sym, mod, e1, e2, tpe, pur, loc) =>
        SimplifiedAst.Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.LetRec(sym, mod, e1, e2, tpe, pur, loc) =>
        SimplifiedAst.Expression.LetRec(sym, visitExp(e1), visitExp(e2), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Region(tpe, loc) =>
        SimplifiedAst.Expression.Region(tpe, loc)

      case LoweredAst.Expression.Scope(sym, regionVar, exp, tpe, pur, loc) =>
        SimplifiedAst.Expression.Scope(sym, visitExp(exp), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.ScopeExit(exp1, exp2, tpe, pur, loc) =>
        SimplifiedAst.Expression.ScopeExit(visitExp(exp1), visitExp(exp2), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Match(exp0, rules, tpe, pur, loc) =>
        patternMatchWithLabels(exp0, rules, tpe, loc)

      case LoweredAst.Expression.RelationalChoose(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Code generation for relational choice is no longer supported", loc)

      case LoweredAst.Expression.Tag(Ast.CaseSymUse(sym, _), e, tpe, pur, loc) =>
        SimplifiedAst.Expression.Tag(sym, visitExp(e), tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.Tuple(elms, tpe, pur, loc) =>
        SimplifiedAst.Expression.Tuple(elms map visitExp, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.RecordEmpty(tpe, loc) =>
        SimplifiedAst.Expression.RecordEmpty(tpe, loc)

      case LoweredAst.Expression.RecordSelect(base, field, tpe, pur, loc) =>
        val b = visitExp(base)
        SimplifiedAst.Expression.RecordSelect(b, field, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.RecordExtend(field, value, rest, tpe, pur, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordExtend(field, v, r, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.RecordRestrict(field, rest, tpe, pur, loc) =>
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordRestrict(field, r, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.ArrayLit(exps, _, tpe, pur, loc) =>
        // Note: The region expression is erased.
        val es = exps.map(visitExp)
        SimplifiedAst.Expression.ArrayLit(es, tpe, loc)

      case LoweredAst.Expression.ArrayNew(_, exp2, exp3, tpe, pur, loc) =>
        // Note: The region expression is erased.
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        SimplifiedAst.Expression.ArrayNew(e2, e3, tpe, loc)

      case LoweredAst.Expression.ArrayLoad(base, index, tpe, pur, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        SimplifiedAst.Expression.ArrayLoad(b, i, tpe, loc)

      case LoweredAst.Expression.ArrayStore(base, index, elm, _, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        SimplifiedAst.Expression.ArrayStore(b, i, e, Type.Unit, loc)

      case LoweredAst.Expression.ArrayLength(base, _, loc) =>
        val b = visitExp(base)
        val purity = b.purity
        SimplifiedAst.Expression.ArrayLength(b, Type.Int32, purity, loc)

      case LoweredAst.Expression.VectorLit(exps, tpe, pur, loc) =>
        // Note: We simplify Vectors to Arrays.
        val es = exps.map(visitExp)
        SimplifiedAst.Expression.ArrayLit(es, tpe, loc)

      case LoweredAst.Expression.VectorLoad(exp1, exp2, tpe, pur, loc) =>
        // Note: We simplify Vectors to Arrays.
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.ArrayLoad(e1, e2, tpe, loc)

      case LoweredAst.Expression.VectorLength(exp, loc) =>
        // Note: We simplify Vectors to Arrays.
        val e = visitExp(exp)
        val pur = e.purity
        SimplifiedAst.Expression.ArrayLength(e, Type.Int32, pur, loc)

      case LoweredAst.Expression.Ref(exp, _, tpe, pur, loc) =>
        // Note: The region expression is erased.
        val e = visitExp(exp)
        SimplifiedAst.Expression.Ref(e, tpe, loc)

      case LoweredAst.Expression.Deref(exp, tpe, pur, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Deref(e, tpe, loc)

      case LoweredAst.Expression.Assign(exp1, exp2, tpe, pur, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.Assign(e1, e2, tpe, loc)

      case LoweredAst.Expression.Ascribe(exp, tpe, pur, loc) => visitExp(exp)

      case LoweredAst.Expression.InstanceOf(exp, clazz, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.InstanceOf(e, clazz, loc)

      case LoweredAst.Expression.Cast(exp, _, _, tpe, pur, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Cast(e, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.TryCatch(exp, rules, tpe, pur, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case LoweredAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expression.TryCatch(e, rs, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.InvokeConstructor(constructor, args, tpe, pur, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeConstructor(constructor, as, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.InvokeMethod(method, exp, args, tpe, pur, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeMethod(method, e, as, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.InvokeStaticMethod(method, args, tpe, pur, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeStaticMethod(method, as, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.GetField(field, exp, tpe, pur, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetField(field, e, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.PutField(field, exp1, exp2, tpe, pur, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutField(field, e1, e2, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.GetStaticField(field, tpe, pur, loc) =>
        SimplifiedAst.Expression.GetStaticField(field, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.PutStaticField(field, exp, tpe, pur, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.PutStaticField(field, e, tpe, simplifyPurity(pur), loc)

      case LoweredAst.Expression.NewObject(name, clazz, tpe, pur, methods0, loc) =>
        val methods = methods0 map visitJvmMethod
        SimplifiedAst.Expression.NewObject(name, clazz, tpe, simplifyPurity(pur), methods, loc)

      case LoweredAst.Expression.Spawn(exp1, exp2, tpe, pur, loc) =>
        // Wrap the expression in a closure: () -> tpe \ eff
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val lambdaTyp = Type.mkArrowWithEffect(Type.Unit, pur, e1.tpe, loc)
        val lambdaExp = SimplifiedAst.Expression.Lambda(List(), e1, lambdaTyp, loc)
        SimplifiedAst.Expression.Spawn(lambdaExp, e2, tpe, loc)

      case LoweredAst.Expression.Lazy(exp, tpe, loc) =>
        // Wrap the expression in a closure: () -> tpe \ Pure
        val e = visitExp(exp)
        val lambdaTyp = Type.mkArrowWithEffect(Type.Unit, Type.Pure, e.tpe, loc)
        val lambdaExp = SimplifiedAst.Expression.Lambda(List(), e, lambdaTyp, loc)
        SimplifiedAst.Expression.Lazy(lambdaExp, tpe, loc)

      case LoweredAst.Expression.Force(exp, tpe, pur, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Force(e, tpe, loc)

      case LoweredAst.Expression.Sig(_, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)

      case LoweredAst.Expression.Without(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)

      case LoweredAst.Expression.TryWith(_, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)

      case LoweredAst.Expression.Do(_, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)

      case LoweredAst.Expression.Resume(_, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.", loc)

      case LoweredAst.Expression.TypeMatch(_, _, _, _, loc) =>
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
      case LoweredAst.JvmMethod(ident, fparams0, exp0, retTpe, pur, loc) =>
        val fparams = fparams0 map visitFormalParam
        val exp = visitExp(exp0)
        SimplifiedAst.JvmMethod(ident, fparams, exp, retTpe, simplifyPurity(pur), loc)
    }

    /**
      * Returns the given pattern `pat0` as an expression.
      */
    def pat2exp(pat0: LoweredAst.Pattern): SimplifiedAst.Expression = pat0 match {
      case LoweredAst.Pattern.Cst(cst, tpe, loc) => SimplifiedAst.Expression.Cst(cst, tpe, loc)
      case LoweredAst.Pattern.Tag(Ast.CaseSymUse(sym, _), p, tpe, loc) =>
        val e = pat2exp(p)
        SimplifiedAst.Expression.Tag(sym, e, tpe, e.purity, loc)
      case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
        val es = elms.map(pat2exp)
        val purity = combineAll(es.map(_.purity))
        SimplifiedAst.Expression.Tuple(es, tpe, purity, loc)
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
    def mkEqual(e1: SimplifiedAst.Expression, e2: SimplifiedAst.Expression, loc: SourceLocation): SimplifiedAst.Expression = {
      /*
       * Special Case 1: Unit
       */
      (e1.tpe.typeConstructor, e2.tpe.typeConstructor) match {
        case (Some(TypeConstructor.Unit), Some(TypeConstructor.Unit)) =>
          // Unit is always equal to itself.
          return SimplifiedAst.Expression.Cst(Ast.Constant.Bool(true), Type.Bool, loc)
        case _ => // fallthrough
      }

      /*
       * Compute the semantic operator.
       */
      val sop = e1.tpe.typeConstructor match {
        case Some(TypeConstructor.Bool) => SemanticOperator.BoolOp.Eq
        case Some(TypeConstructor.Char) => SemanticOperator.CharOp.Eq
        case Some(TypeConstructor.Float32) => SemanticOperator.Float32Op.Eq
        case Some(TypeConstructor.Float64) => SemanticOperator.Float64Op.Eq
        case Some(TypeConstructor.BigDecimal) => SemanticOperator.BigDecimalOp.Eq
        case Some(TypeConstructor.Int8) => SemanticOperator.Int8Op.Eq
        case Some(TypeConstructor.Int16) => SemanticOperator.Int16Op.Eq
        case Some(TypeConstructor.Int32) => SemanticOperator.Int32Op.Eq
        case Some(TypeConstructor.Int64) => SemanticOperator.Int64Op.Eq
        case Some(TypeConstructor.Str) => SemanticOperator.StringOp.Eq
        case t => throw InternalCompilerException(s"Unexpected type: '$t'.", e1.loc)
      }
      val purity = combine(e1.purity, e2.purity)
      SimplifiedAst.Expression.Binary(sop, e1, e2, Type.Bool, purity, loc)
    }

    /**
      * Eliminates pattern matching by translations to labels and jumps.
      */
    def patternMatchWithLabels(exp0: LoweredAst.Expression, rules: List[LoweredAst.MatchRule], tpe: Type, loc: SourceLocation): SimplifiedAst.Expression = {
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
      val jumpPurity = combineAll(rules.map(r => simplifyPurity(r.exp.pur)))

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
          val failure = SimplifiedAst.Expression.JumpTo(next, tpe, jumpPurity, loc)

          // Return the branch with its label.
          field -> patternMatchList(List(pat), List(matchVar), guard.getOrElse(LoweredAst.Expression.Cst(Ast.Constant.Bool(true), Type.Bool, SourceLocation.Unknown)), success, failure
          )
      }
      // Construct the error branch.
      val errorBranch = defaultLab -> SimplifiedAst.Expression.MatchError(tpe, loc)

      // The initial expression simply jumps to the first label.
      val entry = SimplifiedAst.Expression.JumpTo(ruleLabels.head, tpe, jumpPurity, loc)

      // The purity of the branch
      val branchPurity = combineAll(branches.map { case (_, exp) => exp.purity })

      // Assemble all the branches together.
      val branch = SimplifiedAst.Expression.Branch(entry, branches.toMap + errorBranch, tpe, branchPurity, loc)

      // The purity of the match exp
      val matchPurity = combine(matchExp.purity, branch.purity)

      // Wrap the branches inside a let-binding for the match variable.
      SimplifiedAst.Expression.Let(matchVar, matchExp, branch, tpe, matchPurity, loc)
    }

    /**
      * Returns an expression that matches the given list of patterns `xs` against the given list of variables `ys`.
      *
      * Checks the `guard` when all patterns have been matched.
      *
      * Evaluates `succ` on success and `fail` otherwise.
      */
    def patternMatchList(xs: List[LoweredAst.Pattern], ys: List[Symbol.VarSym], guard: LoweredAst.Expression, succ: SimplifiedAst.Expression, fail: SimplifiedAst.Expression): SimplifiedAst.Expression =
      ((xs, ys): @unchecked) match {
        /**
          * There are no more patterns and variables to match.
          *
          * The pattern was match successfully. Test the guard.
          */
        case (Nil, Nil) =>
          val g = visitExp(guard)
          SimplifiedAst.Expression.IfThenElse(g, succ, fail, succ.tpe, g.purity, g.loc)

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
          SimplifiedAst.Expression.Let(sym, SimplifiedAst.Expression.Var(v, tpe, loc), exp, succ.tpe, exp.purity, loc)

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
          val cond = mkEqual(pat2exp(lit), SimplifiedAst.Expression.Var(v, lit.tpe, lit.loc), lit.loc)
          val purity = combine(cond.purity, exp.purity, fail.purity)
          SimplifiedAst.Expression.IfThenElse(cond, exp, fail, succ.tpe, purity, lit.loc)

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
          val cond = SimplifiedAst.Expression.Is(sym, SimplifiedAst.Expression.Var(v, tpe, loc), Pure, loc)
          val freshVar = Symbol.freshVarSym("innerTag" + Flix.Delimiter, BoundBy.Let, loc)
          val inner = patternMatchList(pat :: ps, freshVar :: vs, guard, succ, fail)
          val purity1 = inner.purity
          val consequent = SimplifiedAst.Expression.Let(freshVar, SimplifiedAst.Expression.Untag(sym, SimplifiedAst.Expression.Var(v, tpe, loc), pat.tpe, purity1, loc), inner, succ.tpe, purity1, loc)
          val purity2 = combine(cond.purity, consequent.purity, fail.purity)
          SimplifiedAst.Expression.IfThenElse(cond, consequent, fail, succ.tpe, purity2, loc)

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
              SimplifiedAst.Expression.Let(name, SimplifiedAst.Expression.Index(SimplifiedAst.Expression.Var(v, tpe, loc), idx, pat.tpe, Pure, loc), exp, succ.tpe, exp.purity, loc)
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
  def substitute(exp0: SimplifiedAst.Expression, m: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.Expression = {

    def visitExp(e: SimplifiedAst.Expression): SimplifiedAst.Expression = e match {
      case SimplifiedAst.Expression.Cst(_, _, _) => e

      case SimplifiedAst.Expression.Var(sym, tpe, loc) => m.get(sym) match {
        case None => SimplifiedAst.Expression.Var(sym, tpe, loc)
        case Some(replacement) => SimplifiedAst.Expression.Var(replacement, tpe, loc)
      }

      case SimplifiedAst.Expression.Def(sym, tpe, loc) => e

      case SimplifiedAst.Expression.Lambda(fparams, body, tpe, loc) =>
        SimplifiedAst.Expression.Lambda(fparams, visitExp(body), tpe, loc)

      case SimplifiedAst.Expression.Apply(exp, args, tpe, purity, loc) =>
        SimplifiedAst.Expression.Apply(visitExp(exp), args.map(visitExp), tpe, purity, loc)

      case SimplifiedAst.Expression.Unary(sop, exp, tpe, purity, loc) =>
        SimplifiedAst.Expression.Unary(sop, visitExp(exp), tpe, purity, loc)

      case SimplifiedAst.Expression.Binary(sop, exp1, exp2, tpe, purity, loc) =>
        SimplifiedAst.Expression.Binary(sop, visitExp(exp1), visitExp(exp2), tpe, purity, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        SimplifiedAst.Expression.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, purity, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> br
        }
        SimplifiedAst.Expression.Branch(e, bs, tpe, purity, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, purity, loc) =>
        SimplifiedAst.Expression.JumpTo(sym, tpe, purity, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, purity, tpe, loc) =>
        SimplifiedAst.Expression.Let(sym, visitExp(exp1), visitExp(exp2), purity, tpe, loc)

      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, purity, loc) =>
        SimplifiedAst.Expression.LetRec(sym, visitExp(exp1), visitExp(exp2), tpe, purity, loc)

      case SimplifiedAst.Expression.Region(tpe, loc) =>
        SimplifiedAst.Expression.Region(tpe, loc)

      case SimplifiedAst.Expression.Scope(sym, exp, tpe, purity, loc) =>
        SimplifiedAst.Expression.Scope(sym, visitExp(exp), tpe, purity, loc)

      case SimplifiedAst.Expression.ScopeExit(exp1, exp2, tpe, purity, loc) =>
        SimplifiedAst.Expression.ScopeExit(visitExp(exp1), visitExp(exp2), tpe, purity, loc)

      case SimplifiedAst.Expression.Is(sym, exp, purity, loc) =>
        SimplifiedAst.Expression.Is(sym, visitExp(exp), purity, loc)

      case SimplifiedAst.Expression.Tag(sym, exp, tpe, purity, loc) =>
        SimplifiedAst.Expression.Tag(sym, visitExp(exp), tpe, purity, loc)

      case SimplifiedAst.Expression.Untag(sym, exp, tpe, purity, loc) =>
        SimplifiedAst.Expression.Untag(sym, visitExp(exp), tpe, purity, loc)

      case SimplifiedAst.Expression.Index(exp, offset, tpe, purity, loc) =>
        SimplifiedAst.Expression.Index(visitExp(exp), offset, tpe, purity, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, purity, loc) =>
        SimplifiedAst.Expression.Tuple(elms.map(visitExp), tpe, purity, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        SimplifiedAst.Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, field, tpe, purity, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.RecordSelect(e, field, tpe, purity, loc)

      case SimplifiedAst.Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordExtend(field, v, r, tpe, purity, loc)

      case SimplifiedAst.Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordRestrict(field, r, tpe, purity, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLit(elms.map(visitExp), tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        SimplifiedAst.Expression.ArrayNew(visitExp(elm), visitExp(len), tpe, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLoad(visitExp(base), visitExp(index), tpe, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        SimplifiedAst.Expression.ArrayStore(visitExp(base), visitExp(index), visitExp(elm), tpe, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe, _, loc) =>
        val b = visitExp(base)
        val purity = b.purity
        SimplifiedAst.Expression.ArrayLength(b, tpe, purity, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Ref(visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Deref(visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Assign(visitExp(exp1), visitExp(exp2), tpe, loc)

      case SimplifiedAst.Expression.InstanceOf(exp, clazz, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.InstanceOf(e, clazz, loc)

      case SimplifiedAst.Expression.Cast(exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Cast(e, tpe, purity, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expression.TryCatch(e, rs, tpe, purity, loc)

      case SimplifiedAst.Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeConstructor(constructor, as, tpe, purity, loc)

      case SimplifiedAst.Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeMethod(method, e, as, tpe, purity, loc)

      case SimplifiedAst.Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeStaticMethod(method, as, tpe, purity, loc)

      case SimplifiedAst.Expression.GetField(field, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetField(field, e, tpe, purity, loc)

      case SimplifiedAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutField(field, e1, e2, tpe, purity, loc)

      case SimplifiedAst.Expression.GetStaticField(field, tpe, purity, loc) =>
        exp0

      case SimplifiedAst.Expression.PutStaticField(field, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.PutStaticField(field, e, tpe, purity, loc)

      case SimplifiedAst.Expression.NewObject(name, clazz, tpe, purity, methods0, loc) =>
        val methods = methods0 map visitJvmMethod
        SimplifiedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc)

      case SimplifiedAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.Spawn(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Lazy(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Lazy(e, tpe, loc)

      case SimplifiedAst.Expression.Force(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Force(e, tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, loc) => e

      case SimplifiedAst.Expression.MatchError(tpe, loc) => e

      case SimplifiedAst.Expression.Closure(_, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expression.LambdaClosure(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expression.ApplyClo(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expression.ApplyDef(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
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
  private def simplifyPurity(eff: Type): Purity = {
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
