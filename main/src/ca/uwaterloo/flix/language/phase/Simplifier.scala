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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

/**
  * A phase that simplifies the TypedAst by elimination of pattern matching and other rewritings.
  */
object Simplifier extends Phase[TypedAst.Root, SimplifiedAst.Root] {

  type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Def]

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase(this) {
    //
    // A mutable map to contain fresh top-level definitions.
    //
    val toplevel: TopLevel = mutable.Map.empty

    /**
      * Translates the given definition `def0` to the SimplifiedAst.
      */
    def visitDef(def0: TypedAst.Def): SimplifiedAst.Def = {
      val ann = if (def0.spec.ann.isEmpty) Ast.Annotations.Empty else Ast.Annotations(def0.spec.ann.map(a => a.name))
      val fs = def0.spec.fparams.map(visitFormalParam)
      val exp = visitExp(def0.impl.exp)
      SimplifiedAst.Def(ann, def0.spec.mod, def0.sym, fs, exp, def0.impl.inferredScheme.base, def0.spec.loc)
    }

    /**
      * Translates the given expression `exp0` to the SimplifiedAst.
      */
    def visitExp(exp0: TypedAst.Expression): SimplifiedAst.Expression = exp0 match {
      case TypedAst.Expression.Var(sym, tpe, loc) => SimplifiedAst.Expression.Var(sym, tpe, loc)

      case TypedAst.Expression.Def(sym, tpe, loc) => SimplifiedAst.Expression.Def(sym, tpe, loc)

      case TypedAst.Expression.Sig(sym, tpe, loc) => SimplifiedAst.Expression.HoleError(new Symbol.HoleSym(sym.clazz.namespace, sym.name, loc), tpe, loc) // TODO replace with implementation

      case TypedAst.Expression.Hole(sym, tpe, eff, loc) => SimplifiedAst.Expression.HoleError(sym, tpe, loc)

      case TypedAst.Expression.Unit(loc) => SimplifiedAst.Expression.Unit(loc)

      case TypedAst.Expression.Null(tpe, loc) => SimplifiedAst.Expression.Null(tpe, loc)

      case TypedAst.Expression.True(loc) => SimplifiedAst.Expression.True(loc)

      case TypedAst.Expression.False(loc) => SimplifiedAst.Expression.False(loc)

      case TypedAst.Expression.Char(lit, loc) => SimplifiedAst.Expression.Char(lit, loc)

      case TypedAst.Expression.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit, loc)

      case TypedAst.Expression.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit, loc)

      case TypedAst.Expression.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit, loc)

      case TypedAst.Expression.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit, loc)

      case TypedAst.Expression.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit, loc)

      case TypedAst.Expression.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit, loc)

      case TypedAst.Expression.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit, loc)

      case TypedAst.Expression.Str(lit, loc) => SimplifiedAst.Expression.Str(lit, loc)

      case TypedAst.Expression.Lambda(fparam, exp, tpe, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Lambda(List(p), e, tpe, loc)

      case TypedAst.Expression.Apply(exp, exps, tpe, eff, loc) =>
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        SimplifiedAst.Expression.Apply(e, es, tpe, loc)

      case TypedAst.Expression.Unary(sop, e, tpe, eff, loc) =>
        // TODO: Remove when we have the new backend
        val op = SemanticOperatorOps.toUnaryOp(sop)
        SimplifiedAst.Expression.Unary(sop, op, visitExp(e), tpe, loc)

      case TypedAst.Expression.Binary(sop, e1, e2, tpe, eff, loc) =>
        // TODO: Remove when we have the new backend
        val op = SemanticOperatorOps.toBinaryOp(sop)

        SimplifiedAst.Expression.Binary(sop, op, visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc) =>
        SimplifiedAst.Expression.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), tpe, loc)

      case TypedAst.Expression.Stm(e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.Let(Symbol.freshVarSym("_", loc), visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.Let(sym, mod, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.LetRegion(_, exp, _, _, _) =>
        visitExp(exp)

      case TypedAst.Expression.Match(exp0, rules, tpe, eff, loc) =>
        patternMatchWithLabels(exp0, rules, tpe, loc)

      case TypedAst.Expression.Choose(exps, rules, tpe, eff, loc) =>
        simplifyChoose(exps, rules, tpe, loc)

      case TypedAst.Expression.Tag(sym, tag, e, tpe, eff, loc) =>
        SimplifiedAst.Expression.Tag(sym, tag, visitExp(e), tpe, loc)

      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) =>
        SimplifiedAst.Expression.Tuple(elms map visitExp, tpe, loc)

      case TypedAst.Expression.RecordEmpty(tpe, loc) =>
        SimplifiedAst.Expression.RecordEmpty(tpe, loc)

      case TypedAst.Expression.RecordSelect(base, field, tpe, eff, loc) =>
        val b = visitExp(base)
        SimplifiedAst.Expression.RecordSelect(b, field, tpe, loc)

      case TypedAst.Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordExtend(field, v, r, tpe, loc)

      case TypedAst.Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordRestrict(field, r, tpe, loc)

      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) =>
        SimplifiedAst.Expression.ArrayLit(elms map visitExp, tpe, loc)

      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        val e = visitExp(elm)
        val ln = visitExp(len)
        SimplifiedAst.Expression.ArrayNew(e, ln, tpe, loc)

      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        SimplifiedAst.Expression.ArrayLoad(b, i, tpe, loc)

      case TypedAst.Expression.ArrayStore(base, index, elm, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        SimplifiedAst.Expression.ArrayStore(b, i, e, Type.Unit, loc)

      case TypedAst.Expression.ArrayLength(base, eff, loc) =>
        val b = visitExp(base)
        SimplifiedAst.Expression.ArrayLength(b, Type.Int32, loc)

      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val b = visitExp(base)
        val i1 = visitExp(beginIndex)
        val i2 = visitExp(endIndex)
        SimplifiedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

      case TypedAst.Expression.Ref(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Ref(e, tpe, loc)

      case TypedAst.Expression.Deref(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Deref(e, tpe, loc)

      case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.Assign(e1, e2, tpe, loc)

      case TypedAst.Expression.Existential(fparam, exp, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.mod, fparam.tpe, fparam.loc)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Existential(p, e, loc)

      case TypedAst.Expression.Universal(fparam, exp, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.mod, fparam.tpe, fparam.loc)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Universal(p, e, loc)

      case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)

      case TypedAst.Expression.Cast(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Cast(e, tpe, loc)

      case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case TypedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expression.TryCatch(e, rs, tpe, loc)

      case TypedAst.Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeConstructor(constructor, as, tpe, loc)

      case TypedAst.Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeMethod(method, e, as, tpe, loc)

      case TypedAst.Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeStaticMethod(method, as, tpe, loc)

      case TypedAst.Expression.GetField(field, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetField(field, e, tpe, loc)

      case TypedAst.Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutField(field, e1, e2, tpe, loc)

      case TypedAst.Expression.GetStaticField(field, tpe, eff, loc) =>
        SimplifiedAst.Expression.GetStaticField(field, tpe, loc)

      case TypedAst.Expression.PutStaticField(field, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.PutStaticField(field, e, tpe, loc)

      case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.NewChannel(e, tpe, loc)

      case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetChannel(e, tpe, loc)

      case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutChannel(e1, e2, tpe, loc)

      case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        val rs = rules map {
          case TypedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SimplifiedAst.SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp)

        SimplifiedAst.Expression.SelectChannel(rs, d, tpe, loc)

      case TypedAst.Expression.Spawn(exp, tpe, eff, loc) =>
        // Wrap the expression in a closure: () -> tpe & eff
        val e = visitExp(exp)
        val lambdaTyp = Type.mkArrowWithEffect(Type.Unit, eff, e.tpe)
        val lambdaExp = SimplifiedAst.Expression.Lambda(List(), e, lambdaTyp, loc)
        SimplifiedAst.Expression.Spawn(lambdaExp, lambdaTyp, loc)

      case TypedAst.Expression.Lazy(exp, tpe, loc) =>
        // Wrap the expression in a closure: () -> tpe & Pure
        val e = visitExp(exp)
        val lambdaTyp = Type.mkArrowWithEffect(Type.Unit, Type.Pure, e.tpe)
        val lambdaExp = SimplifiedAst.Expression.Lambda(List(), e, lambdaTyp, loc)
        SimplifiedAst.Expression.Lazy(lambdaExp, tpe, loc)

      case TypedAst.Expression.Force(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Force(e, tpe, loc)

      case TypedAst.Expression.Default(_, _) => throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.Wild(_, _) => throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.FixpointConstraintSet(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.FixpointMerge(_, _, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.FixpointSolve(_, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.FixpointFilter(_, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.FixpointProjectIn(_, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.FixpointProjectOut(_, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case TypedAst.Expression.MatchEff(_, _, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: $exp0.")

    }

    /**
      * Translates the given formal param `p` to the SimplifiedAst.
      */
    def visitFormalParam(p: TypedAst.FormalParam): SimplifiedAst.FormalParam =
      SimplifiedAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

    /**
      * Returns the given pattern `pat0` as an expression.
      */
    def pat2exp(pat0: TypedAst.Pattern): SimplifiedAst.Expression = pat0 match {
      case TypedAst.Pattern.Unit(loc) => SimplifiedAst.Expression.Unit(loc)
      case TypedAst.Pattern.True(loc) => SimplifiedAst.Expression.True(loc)
      case TypedAst.Pattern.False(loc) => SimplifiedAst.Expression.False(loc)
      case TypedAst.Pattern.Char(lit, loc) => SimplifiedAst.Expression.Char(lit, loc)
      case TypedAst.Pattern.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit, loc)
      case TypedAst.Pattern.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit, loc)
      case TypedAst.Pattern.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit, loc)
      case TypedAst.Pattern.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit, loc)
      case TypedAst.Pattern.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit, loc)
      case TypedAst.Pattern.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit, loc)
      case TypedAst.Pattern.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit, loc)
      case TypedAst.Pattern.Str(lit, loc) => SimplifiedAst.Expression.Str(lit, loc)
      case TypedAst.Pattern.Tag(sym, tag, p, tpe, loc) => SimplifiedAst.Expression.Tag(sym, tag, pat2exp(p), tpe, loc)
      case TypedAst.Pattern.Tuple(elms, tpe, loc) => SimplifiedAst.Expression.Tuple(elms map pat2exp, tpe, loc)
      case _ => throw InternalCompilerException(s"Unexpected non-literal pattern $pat0.")
    }

    /**
      * Returns `true` if the given pattern `pat0` is a literal.
      */
    def isPatLiteral(pat0: TypedAst.Pattern): Boolean = pat0 match {
      case TypedAst.Pattern.Unit(loc) => true
      case TypedAst.Pattern.True(loc) => true
      case TypedAst.Pattern.False(loc) => true
      case TypedAst.Pattern.Char(lit, loc) => true
      case TypedAst.Pattern.Float32(lit, loc) => true
      case TypedAst.Pattern.Float64(lit, loc) => true
      case TypedAst.Pattern.Int8(lit, loc) => true
      case TypedAst.Pattern.Int16(lit, loc) => true
      case TypedAst.Pattern.Int32(lit, loc) => true
      case TypedAst.Pattern.Int64(lit, loc) => true
      case TypedAst.Pattern.BigInt(lit, loc) => true
      case TypedAst.Pattern.Str(lit, loc) => true
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
          return SimplifiedAst.Expression.True(loc)
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
        case Some(TypeConstructor.Int8) => SemanticOperator.Int8Op.Eq
        case Some(TypeConstructor.Int16) => SemanticOperator.Int16Op.Eq
        case Some(TypeConstructor.Int32) => SemanticOperator.Int32Op.Eq
        case Some(TypeConstructor.Int64) => SemanticOperator.Int64Op.Eq
        case Some(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Eq
        case Some(TypeConstructor.Str) => SemanticOperator.StringOp.Eq
        case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
      }

      SimplifiedAst.Expression.Binary(sop, BinaryOperator.Equal, e1, e2, Type.Bool, loc)
    }

    /**
      * Returns an expression that adds e2 to e1
      */
    def mkAdd(e1: SimplifiedAst.Expression, e2: SimplifiedAst.Expression, loc: SourceLocation): SimplifiedAst.Expression = {
      val add = SemanticOperator.Int32Op.Add
      val tpe = Type.Int32
      SimplifiedAst.Expression.Binary(add, BinaryOperator.Plus, e1, e2, tpe, loc)
    }

    /**
      * Returns an expression that subtracts e2 from e1
      */
    def mkSub(e1: SimplifiedAst.Expression, e2: SimplifiedAst.Expression, loc: SourceLocation): SimplifiedAst.Expression = {
      val sub = SemanticOperator.Int32Op.Sub
      val tpe = Type.Int32
      SimplifiedAst.Expression.Binary(sub, BinaryOperator.Minus, e1, e2, tpe, loc)
    }

    /**
      * Eliminates pattern matching by translations to labels and jumps.
      */
    def patternMatchWithLabels(exp0: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, loc: SourceLocation): SimplifiedAst.Expression = {
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
      val matchVar = Symbol.freshVarSym("matchVar", loc)

      // Translate the match expression.
      val matchExp = visitExp(exp0)

      // Generate a fresh label for the default fall through case.
      val defaultLab = Symbol.freshLabel("default")

      // Generate a label for each rule.
      val ruleLabels = rules.map(_ => Symbol.freshLabel("case"))

      // Construct a map from each label to the label of the next case.
      // The default label is the next label of the last case.
      val nextLabel = (ruleLabels zip (ruleLabels.drop(1) ::: defaultLab :: Nil)).toMap

      // Create a branch for each rule.
      val branches = (ruleLabels zip rules) map {
        // Process each (label, rule) pair.
        case (field, TypedAst.MatchRule(pat, guard, body)) =>
          // Retrieve the label of the next rule.
          // If this rule is the last, the next label is the default label.
          val next = nextLabel(field)

          // Success case: evaluate the match body.
          val success = visitExp(body)

          // Failure case: Jump to the next label.
          val failure = SimplifiedAst.Expression.JumpTo(next, tpe, loc)

          // Return the branch with its label.
          field -> patternMatchList(List(pat), List(matchVar), guard, success, failure
          )
      }

      // Construct the error branch.
      val errorBranch = defaultLab -> SimplifiedAst.Expression.MatchError(tpe, loc)

      // The initial expression simply jumps to the first label.
      val entry = SimplifiedAst.Expression.JumpTo(ruleLabels.head, tpe, loc)

      // Assemble all the branches together.
      val branch = SimplifiedAst.Expression.Branch(entry, branches.toMap + errorBranch, tpe, loc)

      // Wrap the branches inside a let-binding for the match variable.
      SimplifiedAst.Expression.Let(matchVar, matchExp, branch, tpe, loc)
    }

    /**
      * Returns an expression that matches the given list of patterns `xs` against the given list of variables `ys`.
      *
      * Checks the `guard` when all patterns have been matched.
      *
      * Evaluates `succ` on success and `fail` otherwise.
      */
    def patternMatchList(xs: List[TypedAst.Pattern], ys: List[Symbol.VarSym], guard: TypedAst.Expression, succ: SimplifiedAst.Expression, fail: SimplifiedAst.Expression): SimplifiedAst.Expression =
      ((xs, ys): @unchecked) match {
        /**
          * There are no more patterns and variables to match.
          *
          * The pattern was match successfully. Test the guard.
          */
        case (Nil, Nil) =>
          val g = visitExp(guard)
          SimplifiedAst.Expression.IfThenElse(g, succ, fail, succ.tpe, g.loc)

        /**
          * Matching a wildcard is guaranteed to succeed.
          *
          * We proceed by recursion on the remaining patterns and variables.
          */
        case (TypedAst.Pattern.Wild(tpe, loc) :: ps, v :: vs) =>
          patternMatchList(ps, vs, guard, succ, fail)

        /**
          * Matching a variable is guaranteed to succeed.
          *
          * We proceed by constructing a let-binding that binds the value
          * of the match variable `ident` to the variable `v`.
          * The body of the let-binding is computed by recursion on the
          * remaining patterns and variables.
          */
        case (TypedAst.Pattern.Var(sym, tpe, loc) :: ps, v :: vs) =>
          val exp = patternMatchList(ps, vs, guard, succ, fail)
          SimplifiedAst.Expression.Let(sym, SimplifiedAst.Expression.Var(v, tpe, loc), exp, succ.tpe, loc)

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
          SimplifiedAst.Expression.IfThenElse(cond, exp, fail, succ.tpe, lit.loc)

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
        case (TypedAst.Pattern.Tag(sym, tag, pat, tpe, loc) :: ps, v :: vs) =>
          val cond = SimplifiedAst.Expression.Is(sym, tag, SimplifiedAst.Expression.Var(v, tpe, loc), loc)
          val freshVar = Symbol.freshVarSym("innerTag", loc)
          val inner = patternMatchList(pat :: ps, freshVar :: vs, guard, succ, fail)
          val consequent = SimplifiedAst.Expression.Let(freshVar, SimplifiedAst.Expression.Untag(sym, tag, SimplifiedAst.Expression.Var(v, tpe, loc), pat.tpe, loc), inner, succ.tpe, loc)
          SimplifiedAst.Expression.IfThenElse(cond, consequent, fail, succ.tpe, loc)

        /**
          * Matching a tuple may succeed or fail.
          *
          * We generate a fresh variable and let-binding for each component of the
          * tuple and then we recurse on the nested patterns and freshly generated
          * variables.
          */
        case (TypedAst.Pattern.Tuple(elms, tpe, loc) :: ps, v :: vs) =>
          val freshVars = elms.map(_ => Symbol.freshVarSym("innerElm", loc))
          val zero = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
          elms.zip(freshVars).zipWithIndex.foldRight(zero) {
            case (((pat, name), idx), exp) =>
              SimplifiedAst.Expression.Let(name, SimplifiedAst.Expression.Index(SimplifiedAst.Expression.Var(v, tpe, loc), idx, pat.tpe, loc), exp, succ.tpe, loc)
          }

        /**
          * Matching an array may succeed or fail
          *
          * We generate an if clause checking array length for each pattern, and then
          * generate a fresh variable and let-binding for each variable in the
          * array, which we load with ArrayLoad.
          * We then recurse over the freshly generated variables as the true case of the if clause
          */
        case (TypedAst.Pattern.Array(elms, tpe, loc) :: ps, v :: vs) =>
          val patternCheck = {
            val freshVars = elms.map(_ => Symbol.freshVarSym("arrayElm", loc))
            val zero = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
            elms.zip(freshVars).zipWithIndex.foldRight(zero) {
              case (((pat, name), idx), exp) =>
                val base = SimplifiedAst.Expression.Var(v, tpe, loc)
                val index = SimplifiedAst.Expression.Int32(idx, loc)
                SimplifiedAst.Expression.Let(name,
                  SimplifiedAst.Expression.ArrayLoad(base, index, pat.tpe, loc)
                  , exp, succ.tpe, loc)
            }
          }
          val actualArrayLengthExp = SimplifiedAst.Expression.ArrayLength(SimplifiedAst.Expression.Var(v, tpe, loc), Type.Int32, loc)
          val expectedArrayLengthExp = SimplifiedAst.Expression.Int32(elms.length, loc)
          val lengthCheck = mkEqual(actualArrayLengthExp, expectedArrayLengthExp, loc)
          SimplifiedAst.Expression.IfThenElse(lengthCheck, patternCheck, fail, succ.tpe, loc)

        /**
          * Matching an array with a TailSpread may succeed or fail
          *
          * We generate an if clause checking that array length is at least the length of
          * each pattern, and generate a fresh variable and let-binding for each variable
          * in the array, which we load with ArrayLoad.
          * We then check whether the Spread is a variable, creating a let-binding for the
          * appropriate ArraySlice to the spread, if it is.
          * We then recurse over the freshly generated variables as the true case of the previously
          * created if clause
          */
        case (TypedAst.Pattern.ArrayTailSpread(elms, sym, tpe, loc) :: ps, v :: vs) =>
          val actualArrayLengthExp = SimplifiedAst.Expression.ArrayLength(SimplifiedAst.Expression.Var(v, tpe, loc), Type.Int32, loc)
          val expectedArrayLengthExp = SimplifiedAst.Expression.Int32(elms.length, loc)
          val patternCheck = {
            val freshVars = elms.map(_ => Symbol.freshVarSym("arrayElm", loc))
            val inner = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
            val zero = sym.text match {
              case "_" => inner
              case _ => SimplifiedAst.Expression.Let(sym,
                SimplifiedAst.Expression.ArraySlice(
                  SimplifiedAst.Expression.Var(v, tpe, loc),
                  expectedArrayLengthExp,
                  actualArrayLengthExp, tpe, loc),
                inner, tpe, loc)
            }
            elms.zip(freshVars).zipWithIndex.foldRight(zero) {
              case (((pat, name), idx), exp) =>
                val base = SimplifiedAst.Expression.Var(v, tpe, loc)
                val index = SimplifiedAst.Expression.Int32(idx, loc)
                SimplifiedAst.Expression.Let(name,
                  SimplifiedAst.Expression.ArrayLoad(base, index, pat.tpe, loc)
                  , exp, succ.tpe, loc)
            }
          }
          val op = SemanticOperator.Int32Op.Ge
          val lengthCheck = SimplifiedAst.Expression.Binary(op, BinaryOperator.GreaterEqual, actualArrayLengthExp, expectedArrayLengthExp, Type.Bool, loc)
          SimplifiedAst.Expression.IfThenElse(lengthCheck, patternCheck, fail, succ.tpe, loc)

        /**
          * Matching an array with a HeadSpread may succeed or fail
          *
          * We generate an if clause checking that array length is at least the length of
          * each pattern, and generate a fresh variable and let-binding for each variable
          * in the array, which we load with ArrayLoad.
          * We then check whether the Spread is a variable, creating a let-binding for the
          * appropriate ArraySlice to the spread, if it is.
          * We then recurse over the freshly generated variables as the true case of the previously
          * created if clause
          */
        case (TypedAst.Pattern.ArrayHeadSpread(sym, elms, tpe, loc) :: ps, v :: vs) =>
          val actualArrayLengthExp = SimplifiedAst.Expression.ArrayLength(SimplifiedAst.Expression.Var(v, tpe, loc), Type.Int32, loc)
          val expectedArrayLengthExp = SimplifiedAst.Expression.Int32(elms.length, loc)
          val offset = mkSub(actualArrayLengthExp, expectedArrayLengthExp, loc)
          val patternCheck = {
            val freshVars = elms.map(_ => Symbol.freshVarSym("arrayElm", loc))
            val inner = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
            val zero = sym.text match {
              case "_" => inner
              case _ => SimplifiedAst.Expression.Let(sym,
                SimplifiedAst.Expression.ArraySlice(
                  SimplifiedAst.Expression.Var(v, tpe, loc),
                  SimplifiedAst.Expression.Int32(0, loc),
                  expectedArrayLengthExp, tpe, loc),
                inner, tpe, loc)
            }
            elms.zip(freshVars).zipWithIndex.foldRight(zero) {
              case (((pat, name), idx), exp) =>
                val base = SimplifiedAst.Expression.Var(v, tpe, loc)
                val index = mkAdd(SimplifiedAst.Expression.Int32(idx, loc), offset, loc)
                SimplifiedAst.Expression.Let(name,
                  SimplifiedAst.Expression.ArrayLoad(base, index, pat.tpe, loc)
                  , exp, succ.tpe, loc)
            }
          }
          val ge = SemanticOperator.Int32Op.Ge
          val lengthCheck = SimplifiedAst.Expression.Binary(ge, BinaryOperator.GreaterEqual, actualArrayLengthExp, expectedArrayLengthExp, Type.Bool, loc)
          SimplifiedAst.Expression.IfThenElse(lengthCheck, patternCheck, fail, succ.tpe, loc)


        case p => throw InternalCompilerException(s"Unsupported pattern '$p'.")
      }

    /**
      * Eliminates the choose construct by translations to if-then-else expressions.
      */
    def simplifyChoose(exps0: List[TypedAst.Expression], rules0: List[TypedAst.ChoiceRule], tpe: Type, loc: SourceLocation)(implicit flix: Flix): SimplifiedAst.Expression = {
      //
      // Given the code:
      //
      // choose (x, y, ...) {
      //   case PATTERN_1 => BODY_1
      //   case PATTERN_2 => BODY_2
      //   ...
      //   case PATTERN_N => BODY_N
      // }
      //
      // The structure of the generated code is as follows:
      //
      // let matchVar1 = x;
      // let matchVar2 = y;
      // ...
      //
      // if (matchVar_i is Present(x_i) && ... matchVar_i is Present(x_j))
      //   let x_i = untag matchVar_i;
      //   ...
      //   let x_j = untag matchVar_j;
      //   BODY_1
      // else
      //   if (matchVar_i is Present(x_i) && ... matchVar_j is Present(x_j)) =>
      //   let x_i = untag matchVar_i;
      //   ...
      //   let x_j = untag matchVar_j;
      //     BODY_2
      // ...
      //   else
      //     MatchError
      //

      //
      // The symbol of the Choice data type.
      //
      val sym = Symbol.mkEnumSym("Choice")

      //
      // Translate the match expressions.
      //
      val exps = exps0.map(visitExp)

      //
      // Introduce a fresh variable for each match expression.
      //
      val freshMatchVars = exps.map(_ => Symbol.freshVarSym("matchVar", loc))

      //
      // The default unmatched error expression.
      //
      val unmatchedExp = SimplifiedAst.Expression.MatchError(tpe, loc)

      //
      // All the if-then-else branches.
      //
      val branches = rules0.foldRight(unmatchedExp: SimplifiedAst.Expression) {
        case (TypedAst.ChoiceRule(pat, body), acc) =>
          val init = SimplifiedAst.Expression.True(loc): SimplifiedAst.Expression
          val condExp = freshMatchVars.zip(pat).zip(exps).foldRight(init) {
            case (((freshMatchVar, TypedAst.ChoicePattern.Wild(_)), matchExp), acc) => acc
            case (((freshMatchVar, TypedAst.ChoicePattern.Absent(_)), matchExp), acc) =>
              val varExp = SimplifiedAst.Expression.Var(freshMatchVar, matchExp.tpe, loc)
              val tag = Name.Tag("Absent", loc)
              val isAbsent = SimplifiedAst.Expression.Is(sym, tag, varExp, loc)
              SimplifiedAst.Expression.Binary(SemanticOperator.BoolOp.And, BinaryOperator.LogicalAnd, isAbsent, acc, Type.Bool, loc)
            case (((freshMatchVar, TypedAst.ChoicePattern.Present(matchVar, _, _)), matchExp), acc) =>
              val varExp = SimplifiedAst.Expression.Var(freshMatchVar, matchExp.tpe, loc)
              val tag = Name.Tag("Present", loc)
              val isPresent = SimplifiedAst.Expression.Is(sym, tag, varExp, loc)
              SimplifiedAst.Expression.Binary(SemanticOperator.BoolOp.And, BinaryOperator.LogicalAnd, isPresent, acc, Type.Bool, loc)
          }
          val bodyExp = visitExp(body)
          val thenExp = freshMatchVars.zip(pat).zip(exps).foldRight(bodyExp) {
            case (((freshMatchVar, TypedAst.ChoicePattern.Wild(_)), matchExp), acc) => acc
            case (((freshMatchVar, TypedAst.ChoicePattern.Absent(_)), matchExp), acc) => acc
            case (((freshMatchVar, TypedAst.ChoicePattern.Present(matchVar, tpe, _)), matchExp), acc) =>
              val varExp = SimplifiedAst.Expression.Var(freshMatchVar, matchExp.tpe, loc)
              val tag = Name.Tag("Present", loc)
              val untagExp = SimplifiedAst.Expression.Untag(sym, tag, varExp, tpe, loc)
              SimplifiedAst.Expression.Let(matchVar, untagExp, acc, acc.tpe, loc)
          }
          val elseExp = acc
          SimplifiedAst.Expression.IfThenElse(condExp, thenExp, elseExp, tpe, loc)
      }

      //
      // Let bind the match variables.
      //
      freshMatchVars.zip(exps).foldRight(branches: SimplifiedAst.Expression) {
        case ((sym, matchExp), acc) => SimplifiedAst.Expression.Let(sym, matchExp, acc, tpe, loc)
      }
    }

    //
    // Main computation.
    //
    val defns = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map {
      case (k, TypedAst.Enum(doc, mod, sym, tparams, cases0, enumType, enumSc, loc)) =>
        val cases = cases0 map {
          case (tag, TypedAst.Case(enumSym, tagName, tagType, tagSc, tagLoc)) => tag -> SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)
        }
        k -> SimplifiedAst.Enum(mod, sym, cases, enumType, loc)
    }
    val reachable = root.reachable

    SimplifiedAst.Root(defns ++ toplevel, enums, reachable, root.sources).toSuccess
  }

  /**
    * Returns a copy of the given expression `exp0` where every variable symbol has been replaced according to the given substitution `m`.
    */
  def substitute(exp0: SimplifiedAst.Expression, m: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.Expression = {

    def visitExp(e: SimplifiedAst.Expression): SimplifiedAst.Expression = e match {
      case SimplifiedAst.Expression.Unit(_) => e

      case SimplifiedAst.Expression.Null(_, _) => e

      case SimplifiedAst.Expression.True(_) => e

      case SimplifiedAst.Expression.False(_) => e

      case SimplifiedAst.Expression.Char(_, _) => e

      case SimplifiedAst.Expression.Float32(_, _) => e

      case SimplifiedAst.Expression.Float64(_, _) => e

      case SimplifiedAst.Expression.Int8(_, _) => e

      case SimplifiedAst.Expression.Int16(_, _) => e

      case SimplifiedAst.Expression.Int32(_, _) => e

      case SimplifiedAst.Expression.Int64(_, _) => e

      case SimplifiedAst.Expression.BigInt(_, _) => e

      case SimplifiedAst.Expression.Str(_, _) => e

      case SimplifiedAst.Expression.Var(sym, tpe, loc) => m.get(sym) match {
        case None => SimplifiedAst.Expression.Var(sym, tpe, loc)
        case Some(replacement) => SimplifiedAst.Expression.Var(replacement, tpe, loc)
      }

      case SimplifiedAst.Expression.Def(sym, tpe, loc) => e

      case SimplifiedAst.Expression.Lambda(fparams, body, tpe, loc) =>
        SimplifiedAst.Expression.Lambda(fparams, visitExp(body), tpe, loc)

      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        SimplifiedAst.Expression.Apply(visitExp(exp), args.map(visitExp), tpe, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        SimplifiedAst.Expression.Unary(sop, op, visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Binary(sop, op, visitExp(exp1), visitExp(exp2), tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        SimplifiedAst.Expression.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> br
        }
        SimplifiedAst.Expression.Branch(e, bs, tpe, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        SimplifiedAst.Expression.JumpTo(sym, tpe, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Let(sym, visitExp(exp1), visitExp(exp2), tpe, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        SimplifiedAst.Expression.Is(sym, tag, visitExp(exp), loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        SimplifiedAst.Expression.Tag(enum, tag, visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        SimplifiedAst.Expression.Untag(sym, tag, visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Index(exp, offset, tpe, loc) =>
        SimplifiedAst.Expression.Index(visitExp(exp), offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        SimplifiedAst.Expression.Tuple(elms.map(visitExp), tpe, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        SimplifiedAst.Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, field, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.RecordSelect(e, field, tpe, loc)

      case SimplifiedAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordExtend(field, v, r, tpe, loc)

      case SimplifiedAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordRestrict(field, r, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLit(elms.map(visitExp), tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        SimplifiedAst.Expression.ArrayNew(visitExp(elm), visitExp(len), tpe, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLoad(visitExp(base), visitExp(index), tpe, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        SimplifiedAst.Expression.ArrayStore(visitExp(base), visitExp(index), visitExp(elm), tpe, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLength(visitExp(base), tpe, loc)

      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        SimplifiedAst.Expression.ArraySlice(visitExp(base), visitExp(startIndex), visitExp(endIndex), tpe, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Ref(visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Deref(visitExp(exp), tpe, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Assign(visitExp(exp1), visitExp(exp2), tpe, loc)

      case SimplifiedAst.Expression.Existential(params, exp, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Existential(params, e, loc)

      case SimplifiedAst.Expression.Universal(params, exp, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Universal(params, e, loc)

      case SimplifiedAst.Expression.Cast(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Cast(e, tpe, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeConstructor(constructor, as, tpe, loc)

      case SimplifiedAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeMethod(method, e, as, tpe, loc)

      case SimplifiedAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args.map(visitExp)
        SimplifiedAst.Expression.InvokeStaticMethod(method, as, tpe, loc)

      case SimplifiedAst.Expression.GetField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetField(field, e, tpe, loc)

      case SimplifiedAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutField(field, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.GetStaticField(field, tpe, loc) =>
        exp0

      case SimplifiedAst.Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.PutStaticField(field, e, tpe, loc)

      case SimplifiedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.NewChannel(e, tpe, loc)

      case SimplifiedAst.Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetChannel(e, tpe, loc)

      case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutChannel(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SimplifiedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SimplifiedAst.SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp)

        SimplifiedAst.Expression.SelectChannel(rs, d, tpe, loc)

      case SimplifiedAst.Expression.Spawn(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Spawn(e, tpe, loc)

      case SimplifiedAst.Expression.Lazy(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Lazy(e, tpe, loc)

      case SimplifiedAst.Expression.Force(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Force(e, tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, loc) => e

      case SimplifiedAst.Expression.MatchError(tpe, loc) => e

      case SimplifiedAst.Expression.Closure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
      case SimplifiedAst.Expression.LambdaClosure(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
      case SimplifiedAst.Expression.ApplyClo(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
      case SimplifiedAst.Expression.ApplyDef(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    }

    visitExp(exp0)
  }

}
