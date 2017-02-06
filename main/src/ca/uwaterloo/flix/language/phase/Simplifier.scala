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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.BinaryOperator.Equal
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * A phase that simplifies a Typed AST by:
  *
  * - Compiles literals to expressions.
  * - Eliminates match expressions.
  * - Numbers every variable.
  */
object Simplifier {

  def simplify(tast: TypedAst.Root)(implicit genSym: GenSym): SimplifiedAst.Root = {
    val t = System.nanoTime()

    val constants = tast.definitions.map { case (k, v) => k -> Definition.simplify(v) }
    val enums = tast.enums.map {
      case (k, TypedAst.Declaration.Enum(doc, sym, cases0, sc, loc)) =>
        val cases = cases0 map {
          case (tag, TypedAst.Case(enum, tagName, tpe)) => tag -> SimplifiedAst.Case(enum, tagName, tpe)
        }
        k -> SimplifiedAst.Definition.Enum(sym, cases, loc)
    }
    val lattices = tast.lattices.map { case (k, v) => k -> Definition.simplify(v) }
    val collections = tast.tables.map { case (k, v) => k -> Table.simplify(v) }
    val indexes = tast.indexes.map { case (k, v) => k -> Definition.simplify(v) }
    val strata = tast.strata.map(simplify)
    val properties = tast.properties.map { p => simplify(p) }
    val time = tast.time

    val e = System.nanoTime() - t
    SimplifiedAst.Root(constants, enums, lattices, collections, indexes, strata, properties, time.copy(simplifier = e))
  }

  object Table {
    def simplify(tast: TypedAst.Table)(implicit genSym: GenSym): SimplifiedAst.Table = tast match {
      case TypedAst.Table.Relation(doc, symbol, attributes, loc) =>
        SimplifiedAst.Table.Relation(symbol, attributes.map(Simplifier.simplify), loc)
      case TypedAst.Table.Lattice(doc, name, keys, value, loc) =>
        SimplifiedAst.Table.Lattice(name, keys.map(Simplifier.simplify), Simplifier.simplify(value), loc)
    }
  }

  object Declarations {

    object Constraints {
      def simplify(tast: TypedAst.Constraint)(implicit genSym: GenSym): SimplifiedAst.Constraint = {
        val head = Predicate.Head.simplify(tast.head)
        val body = tast.body.map(Predicate.Body.simplify)
        val cparams = tast.cparams.map {
          case TypedAst.ConstraintParam.HeadParam(sym, tpe, loc) => SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc)
          case TypedAst.ConstraintParam.RuleParam(sym, tpe, loc) => SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc)
        }

        SimplifiedAst.Constraint(cparams, head, body)
      }
    }

  }

  object Definition {
    def simplify(tast: TypedAst.Declaration.BoundedLattice)(implicit genSym: GenSym): SimplifiedAst.Definition.Lattice = tast match {
      case TypedAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        import Expression.{simplify => s}
        SimplifiedAst.Definition.Lattice(tpe, s(bot), s(top), s(leq), s(lub), s(glb), loc)
    }

    def simplify(tast: TypedAst.Declaration.Definition)(implicit genSym: GenSym): SimplifiedAst.Definition.Constant = {
      val formals = tast.formals.map {
        case TypedAst.FormalParam(sym, tpe, loc) => SimplifiedAst.FormalParam(sym, tpe)
      }
      SimplifiedAst.Definition.Constant(tast.ann, tast.sym, formals, Expression.simplify(tast.exp), isSynthetic = false, tast.tpe, tast.loc)
    }

    def simplify(tast: TypedAst.Declaration.Index)(implicit genSym: GenSym): SimplifiedAst.Definition.Index =
      SimplifiedAst.Definition.Index(tast.sym, tast.indexes, tast.loc)

  }

  object Expression {
    def simplify(tast: TypedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = tast match {
      case TypedAst.Expression.Wild(tpe, loc) => ??? // TODO
      case TypedAst.Expression.Unit(loc) => SimplifiedAst.Expression.Unit
      case TypedAst.Expression.True(loc) => SimplifiedAst.Expression.True
      case TypedAst.Expression.False(loc) => SimplifiedAst.Expression.False
      case TypedAst.Expression.Char(lit, loc) => SimplifiedAst.Expression.Char(lit)
      case TypedAst.Expression.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit)
      case TypedAst.Expression.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit)
      case TypedAst.Expression.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit)
      case TypedAst.Expression.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit)
      case TypedAst.Expression.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit)
      case TypedAst.Expression.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit)
      case TypedAst.Expression.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit)
      case TypedAst.Expression.Str(lit, loc) => SimplifiedAst.Expression.Str(lit)
      case TypedAst.Expression.Var(sym, tpe, loc) => SimplifiedAst.Expression.Var(sym, tpe, loc)
      case TypedAst.Expression.Ref(sym, tpe, loc) => SimplifiedAst.Expression.Ref(sym, tpe, loc)
      case TypedAst.Expression.Hook(hook, tpe, loc) => SimplifiedAst.Expression.Hook(hook, tpe, loc)
      case TypedAst.Expression.Lambda(args, body, tpe, loc) =>
        SimplifiedAst.Expression.Lambda(args map Simplifier.simplify, simplify(body), tpe, loc)
      case TypedAst.Expression.Apply(e, args, tpe, loc) =>
        SimplifiedAst.Expression.Apply(simplify(e), args map simplify, tpe, loc)
      case TypedAst.Expression.Unary(op, e, tpe, loc) =>
        SimplifiedAst.Expression.Unary(op, simplify(e), tpe, loc)
      case TypedAst.Expression.Binary(op, e1, e2, tpe, loc) =>
        SimplifiedAst.Expression.Binary(op, simplify(e1), simplify(e2), tpe, loc)
      case TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc) =>
        SimplifiedAst.Expression.IfThenElse(simplify(e1), simplify(e2), simplify(e3), tpe, loc)
      case TypedAst.Expression.Switch(rules, tpe, loc) =>
        val zero = SimplifiedAst.Expression.SwitchError(tpe, loc)
        rules.foldRight(zero: SimplifiedAst.Expression) {
          case ((e1, e2), acc) =>
            val cond = simplify(e1)
            val body = simplify(e2)
            SimplifiedAst.Expression.IfThenElse(cond, body, acc, tpe, loc)
        }
      case TypedAst.Expression.Let(sym, e1, e2, tpe, loc) =>
        SimplifiedAst.Expression.Let(sym, simplify(e1), simplify(e2), tpe, loc)

      case TypedAst.Expression.Match(exp0, rules, tpe, loc) =>
        import SimplifiedAst.{Expression => SExp}

        /**
          * Given the code:
          *
          * match x with {
          * case PATTERN_1 => BODY_1
          * case PATTERN_2 => BODY_2
          * ...
          * case PATTERN_N => BODY_N
          * }
          *
          * The structure of the generated code is as follows:
          *
          * let v' = x in
          * let fallthrough = fn () = ERROR in
          *
          * let v_n = fn () if (PATTERN_N succeeds) BODY_N else fallthrough() in
          * ...
          * let v_2 = fn () = if (PATTERN_2 succeeds) BODY_2 else v_3() in
          * let v_1 = fn () = if (PATTERN_1 succeeds) BODY_1 else v_2() in
          * v_1()
          */

        /**
          * First, we generate a temporary variable `matchVar` for the match expression `matchExp`
          * to avoid the match expression from being evaluated by every case test.
          *
          * The `matchVar` is used by every case in the pattern match to test the value.
          */
        val matchVar = Symbol.freshVarSym("matchVar")
        val matchExp = simplify(exp0)
        val fallthrough = Symbol.freshVarSym("case")

        /**
          * Second, we generate a fresh variable name for each case, as well as a fresh variable name for the
          * fallthrough case (which matches anything and throws a match error).
          */
        val vars = rules.map(_ => Symbol.freshVarSym("case"))
        val cases = rules

        /**
          * Third, we use recursion to generate the nested let-bindings for all the cases of the pattern match. To make
          * the recursion easier, the initial call reverses `names` and `cases`, since we're building the let-bindings
          * outside-in (from the last case to the first case).
          */
        def recur(names: List[Symbol.VarSym],
                  cases: List[TypedAst.MatchRule],
                  next: Symbol.VarSym): SExp = ((names, cases): @unchecked) match {
          case (Nil, Nil) =>
            // Base case: simply call the function representing the first case, to start the pattern match.
            SExp.Apply(SExp.Var(vars.head, Type.mkArrow(List(), tpe), loc), List(), tpe, loc)
          case (n :: ns, TypedAst.MatchRule(pat, guard, body) :: cs) =>
            // Construct the lambda that represents the current case:
            //   fn() = if `matchVar` matches `pat`, return `body`, else call `next()`
            val lambda = SExp.Lambda(
              args = List(),
              body = Patterns.simplify(
                xs = List(pat),
                ys = List(matchVar),
                guard,
                succ = simplify(body),
                fail = SExp.Apply(SExp.Var(next, Type.mkArrow(List(), tpe), loc), List(), tpe, loc)
              ),
              Type.mkArrow(List(), tpe), loc)

            // Construct the let-expression, binding the lambda to the current case's `name`.
            // Recursively construct the body of the let-expression, on the remaining names and cases.
            // In the recursive call, the `next` name is `n`, the name of the case we just processed.
            SExp.Let(n, lambda, recur(ns, cs, n), tpe, loc)
        }

        val patterns = recur(vars.reverse, cases.reverse, fallthrough)

        /**
          * Fourth, we generate the match error and bind it to the `fallthrough` name. Note that the match error must
          * be wrapped in a function call, to defer its evaluation.
          */
        val error = SExp.Lambda(List(), SExp.MatchError(tpe, loc), Type.mkArrow(List(), tpe), loc)
        val inner = SExp.Let(fallthrough, error, patterns, tpe, loc)

        /**
          * Finally, we generate the outermost let-binding, which binds the `matchExp` to `matchVar`.
          */
        SExp.Let(matchVar, matchExp, inner, tpe, loc)

      case TypedAst.Expression.Tag(sym, tag, e, tpe, loc) =>
        SimplifiedAst.Expression.Tag(sym, tag, simplify(e), tpe, loc)
      case TypedAst.Expression.Tuple(elms, tpe, loc) =>
        SimplifiedAst.Expression.Tuple(elms map simplify, tpe, loc)
      case TypedAst.Expression.Existential(fparam, exp, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.tpe)
        val e = simplify(exp)
        SimplifiedAst.Expression.Existential(p, e, loc)
      case TypedAst.Expression.Universal(fparam, exp, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.tpe)
        val e = simplify(exp)
        SimplifiedAst.Expression.Universal(p, e, loc)
      case TypedAst.Expression.UserError(tpe, loc) =>
        SimplifiedAst.Expression.UserError(tpe, loc)
    }
  }

  object Patterns {

    import SimplifiedAst.{Expression => SExp}
    import TypedAst.Pattern._

    // TODO: Have some one carefully peer-review this. esp. w.r.t. types.

    /**
      * Eliminates pattern matching.
      */
    def simplify(xs: List[TypedAst.Pattern],
                 ys: List[Symbol.VarSym],
                 guard: TypedAst.Expression,
                 succ: SExp,
                 fail: SExp)(implicit genSym: GenSym): SExp = ((xs, ys): @unchecked) match {
      /**
        * There are no more patterns and variables to match.
        *
        * The pattern was match successfully. Test the guard.
        */
      case (Nil, Nil) =>
        val g = Expression.simplify(guard)
        SExp.IfThenElse(g, succ, fail, succ.tpe, g.loc)

      /**
        * Matching a wildcard is guaranteed to succeed.
        *
        * We proceed by recursion on the remaining patterns and variables.
        */
      case (Wild(tpe, loc) :: ps, v :: vs) =>
        simplify(ps, vs, guard, succ, fail)

      /**
        * Matching a variable is guaranteed to succeed.
        *
        * We proceed by constructing a let-binding that binds the value
        * of the match variable `ident` to the variable `v`.
        * The body of the let-binding is computed by recursion on the
        * remaining patterns and variables.
        */
      case (Var(sym, tpe, loc) :: ps, v :: vs) =>
        val exp = simplify(ps, vs, guard, succ, fail)
        SExp.Let(sym, SExp.Var(v, tpe, loc), exp, succ.tpe, loc)

      /**
        * Matching a literal may succeed or fail.
        *
        * We generate a binary expression testing whether the literal `lit`
        * matches the variable `v` and then we generate an if-then-else
        * expression where the consequent expression is determined by
        * recursion on the remaining patterns and variables and the
        * alternative expression is `fail`.
        */
      case (lit :: ps, v :: vs) if isLiteral(lit) =>
        val exp = simplify(ps, vs, guard, succ, fail)
        val cond = SExp.Binary(Equal, pat2exp(lit), SExp.Var(v, lit.tpe, lit.loc), Type.Bool, lit.loc)
        SExp.IfThenElse(cond, exp, fail, succ.tpe, lit.loc)

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
      case (Tag(enum, tag, pat, tpe, loc) :: ps, v :: vs) =>
        val cond = SExp.Is(SExp.Var(v, tpe, loc), tag, loc)
        val freshVar = Symbol.freshVarSym("innerTag")
        val inner = simplify(pat :: ps, freshVar :: vs, guard, succ, fail)
        val consequent = SExp.Let(freshVar, SExp.Untag(tag, SExp.Var(v, tpe, loc), pat.tpe, loc), inner, succ.tpe, loc)
        SExp.IfThenElse(cond, consequent, fail, succ.tpe, loc)

      /**
        * Matching a tuple may succeed or fail.
        *
        * We generate a fresh variable and let-binding for each component of the
        * tuple and then we recurse on the nested patterns and freshly generated
        * variables.
        */
      case (Tuple(elms, tpe, loc) :: ps, v :: vs) =>
        val freshVars = elms.map(_ => Symbol.freshVarSym("innerElm"))
        val zero = simplify(elms ::: ps, freshVars ::: vs, guard, succ, fail)
        elms.zip(freshVars).zipWithIndex.foldRight(zero) {
          case (((pat, name), idx), exp) =>
            SExp.Let(name, SExp.Index(SExp.Var(v, tpe, loc), idx, pat.tpe, loc), exp, succ.tpe, loc)
        }

      case (FSet(elms, rest, tpe, loc) :: ps, v :: vs) => ???

      case (FMap(elms, rest, tpe, loc) :: ps, v :: vs) => ???

      case p => throw InternalCompilerException(s"Unsupported pattern '$p'.")

    }

    /**
      * Translates the given typed pattern `pat0` into a simplified pattern.
      *
      * NB: This function is only used for constraints. Patterns are eliminated for expressions.
      */
    def translate(pat0: TypedAst.Pattern): SimplifiedAst.Pattern = pat0 match {
      case TypedAst.Pattern.Wild(tpe, loc) => SimplifiedAst.Pattern.Wild(tpe, loc)
      case TypedAst.Pattern.Var(sym, tpe, loc) => SimplifiedAst.Pattern.Var(sym, tpe, loc)
      case TypedAst.Pattern.Unit(loc) => SimplifiedAst.Pattern.Unit(loc)
      case TypedAst.Pattern.True(loc) => SimplifiedAst.Pattern.True(loc)
      case TypedAst.Pattern.False(loc) => SimplifiedAst.Pattern.False(loc)
      case TypedAst.Pattern.Char(lit, loc) => SimplifiedAst.Pattern.Char(lit, loc)
      case TypedAst.Pattern.Float32(lit, loc) => SimplifiedAst.Pattern.Float32(lit, loc)
      case TypedAst.Pattern.Float64(lit, loc) => SimplifiedAst.Pattern.Float64(lit, loc)
      case TypedAst.Pattern.Int8(lit, loc) => SimplifiedAst.Pattern.Int8(lit, loc)
      case TypedAst.Pattern.Int16(lit, loc) => SimplifiedAst.Pattern.Int16(lit, loc)
      case TypedAst.Pattern.Int32(lit, loc) => SimplifiedAst.Pattern.Int32(lit, loc)
      case TypedAst.Pattern.Int64(lit, loc) => SimplifiedAst.Pattern.Int64(lit, loc)
      case TypedAst.Pattern.BigInt(lit, loc) => SimplifiedAst.Pattern.BigInt(lit, loc)
      case TypedAst.Pattern.Str(lit, loc) => SimplifiedAst.Pattern.Str(lit, loc)
      case TypedAst.Pattern.Tag(sym, tag, pat, tpe, loc) => SimplifiedAst.Pattern.Tag(sym, tag, translate(pat), tpe, loc)
      case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
        val es = elms map translate
        SimplifiedAst.Pattern.Tuple(es, tpe, loc)
      case TypedAst.Pattern.FSet(elms, rest, tpe, loc) => ??? // TODO: Unsupported
      case TypedAst.Pattern.FMap(elms, rest, tpe, loc) => ??? // TODO: Unsupported
    }

  }

  object Predicate {

    object Head {
      def simplify(tast: TypedAst.Predicate.Head)(implicit genSym: GenSym): SimplifiedAst.Predicate.Head = tast match {
        case TypedAst.Predicate.Head.True(loc) => SimplifiedAst.Predicate.Head.True(loc)

        case TypedAst.Predicate.Head.False(loc) => SimplifiedAst.Predicate.Head.False(loc)

        case TypedAst.Predicate.Head.Positive(sym, terms, loc) =>
          val ts = terms map Term.Head.simplify
          SimplifiedAst.Predicate.Head.Positive(sym, ts, loc)

        case TypedAst.Predicate.Head.Negative(sym, terms, loc) =>
          val ts = terms map Term.Head.simplify
          SimplifiedAst.Predicate.Head.Negative(sym, ts, loc)
      }
    }

    object Body {
      def simplify(tast: TypedAst.Predicate.Body)(implicit genSym: GenSym): SimplifiedAst.Predicate.Body = tast match {
        case TypedAst.Predicate.Body.Positive(sym, terms, loc) =>
          val ts = terms map Term.Body.simplify
          SimplifiedAst.Predicate.Body.Positive(sym, ts, loc)

        case TypedAst.Predicate.Body.Negative(sym, terms, loc) =>
          val ts = terms map Term.Body.simplify
          SimplifiedAst.Predicate.Body.Negative(sym, ts, loc)

        case TypedAst.Predicate.Body.Filter(sym, terms, loc) =>
          SimplifiedAst.Predicate.Body.Filter(sym, terms map Term.Body.simplify, loc)

        case TypedAst.Predicate.Body.Loop(sym, term, loc) =>
          SimplifiedAst.Predicate.Body.Loop(sym, Term.Head.simplify(term), loc)
      }
    }

  }

  object Term {

    object Head {
      /**
        * Simplifies the given head term expression `e`.
        */
      def simplify(e: TypedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Term.Head = e match {
        case TypedAst.Expression.Var(sym, tpe, loc) =>
          SimplifiedAst.Term.Head.Var(sym, tpe, loc)

          // TODO: Must lift every non-literal expression to the top-level.
        case TypedAst.Expression.Apply(TypedAst.Expression.Ref(sym, _, _), args, tpe, loc) =>
          val as = args map {
            case TypedAst.Expression.Var(s, _, _) => s
            case _ => throw InternalCompilerException(s"No longer supported: $e") // TODO
          }
          SimplifiedAst.Term.Head.App(sym, as, tpe, loc)

        case TypedAst.Expression.Apply(TypedAst.Expression.Hook(hook, _, _), args, tpe, loc) =>
          throw InternalCompilerException("No longer supported.") // TODO

        case _ => SimplifiedAst.Term.Head.Lit(Expression.simplify(e), e.tpe, e.loc)
      }
    }


    object Body {
      /**
        * Simplifies the given body term pattern `p`.
        */
      def simplify(p: TypedAst.Pattern)(implicit genSym: GenSym): SimplifiedAst.Term.Body = p match {
        case TypedAst.Pattern.Wild(tpe, loc) => SimplifiedAst.Term.Body.Wild(tpe, loc)
        case TypedAst.Pattern.Var(sym, tpe, loc) => SimplifiedAst.Term.Body.Var(sym, tpe, loc)
        case _ => if (isLiteral(p))
          SimplifiedAst.Term.Body.Lit(pat2exp(p), p.tpe, p.loc)
        else
          SimplifiedAst.Term.Body.Pat(Patterns.translate(p), p.tpe, p.loc)
      }

      /**
        * Simplifies the given body term expression `e`.
        */
      def simplify(e: TypedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Term.Body = e match {
        case TypedAst.Expression.Wild(tpe, loc) => SimplifiedAst.Term.Body.Wild(tpe, loc)
        case TypedAst.Expression.Var(sym, tpe, loc) => SimplifiedAst.Term.Body.Var(sym, tpe, loc)
        case _ => SimplifiedAst.Term.Body.Lit(Expression.simplify(e), e.tpe, e.loc) // TODO: Only certain expressions should be allow here.
      }
    }

  }

  def simplify(tast: TypedAst.Attribute)(implicit genSym: GenSym): SimplifiedAst.Attribute =
    SimplifiedAst.Attribute(tast.name, tast.tpe)

  def simplify(tast: TypedAst.FormalParam)(implicit genSym: GenSym): SimplifiedAst.FormalParam =
    SimplifiedAst.FormalParam(tast.sym, tast.tpe)

  def simplify(tast: TypedAst.Property)(implicit genSym: GenSym): SimplifiedAst.Property =
    SimplifiedAst.Property(tast.law, tast.defn, Expression.simplify(tast.exp))

  def simplify(s: TypedAst.Stratum)(implicit genSym: GenSym): SimplifiedAst.Stratum =
    SimplifiedAst.Stratum(s.constraints.map(Declarations.Constraints.simplify))

  /**
    * Returns `true` if the given pattern `pat0` is a literal.
    */
  def isLiteral(pat0: TypedAst.Pattern): Boolean = pat0 match {
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
    case TypedAst.Pattern.Tag(_, _, p, _, _) => isLiteral(p)
    case TypedAst.Pattern.Tuple(elms, _, _) => elms forall isLiteral
    case _ => false
  }

  /**
    * Returns the given pattern `pat0` as an expression.
    */
  def pat2exp(pat0: TypedAst.Pattern): SimplifiedAst.Expression = pat0 match {
    case TypedAst.Pattern.Unit(loc) => SimplifiedAst.Expression.Unit
    case TypedAst.Pattern.True(loc) => SimplifiedAst.Expression.True
    case TypedAst.Pattern.False(loc) => SimplifiedAst.Expression.False
    case TypedAst.Pattern.Char(lit, loc) => SimplifiedAst.Expression.Char(lit)
    case TypedAst.Pattern.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit)
    case TypedAst.Pattern.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit)
    case TypedAst.Pattern.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit)
    case TypedAst.Pattern.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit)
    case TypedAst.Pattern.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit)
    case TypedAst.Pattern.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit)
    case TypedAst.Pattern.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit)
    case TypedAst.Pattern.Str(lit, loc) => SimplifiedAst.Expression.Str(lit)
    case TypedAst.Pattern.Tag(sym, tag, p, tpe, loc) => SimplifiedAst.Expression.Tag(sym, tag, pat2exp(p), tpe, loc)
    case TypedAst.Pattern.Tuple(elms, tpe, loc) => SimplifiedAst.Expression.Tuple(elms map pat2exp, tpe, loc)
    case _ => throw InternalCompilerException(s"Unexpected non-literal pattern $pat0.")
  }

}
