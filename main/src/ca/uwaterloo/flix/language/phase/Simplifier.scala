package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.BinaryOperator.Equal
import ca.uwaterloo.flix.language.ast._

/**
  * A phase that simplifies a Typed AST by:
  *
  * - Compiles literals to expressions.
  * - Eliminates match expressions.
  * - Numbers every variable.
  */
object Simplifier {

  def simplify(tast: TypedAst.Root)(implicit genSym: GenSym): SimplifiedAst.Root = {
    val constants = tast.constants.mapValues(Definition.simplify)
    val directives = Directives.simplify(tast.directives)
    val lattices = tast.lattices.mapValues(Definition.simplify)
    val collections = tast.collections.mapValues(Collection.simplify)
    val indexes = tast.indexes.mapValues(Definition.simplify)
    val facts = tast.facts.map(Constraint.simplify)
    val rules = tast.rules.map(Constraint.simplify)
    val time = tast.time

    SimplifiedAst.Root(constants, directives, lattices, collections, indexes, facts, rules, time)
  }

  object Collection {
    def simplify(tast: TypedAst.Collection)(implicit genSym: GenSym): SimplifiedAst.Collection = tast match {
      case TypedAst.Collection.Relation(name, attributes, loc) =>
        SimplifiedAst.Collection.Relation(name, attributes.map(Simplifier.simplify), loc)
      case TypedAst.Collection.Lattice(name, keys, values, loc) =>
        SimplifiedAst.Collection.Lattice(name, keys.map(Simplifier.simplify), values.map(Simplifier.simplify), loc)
    }
  }

  object Constraint {
    def simplify(tast: TypedAst.Constraint.Fact)(implicit genSym: GenSym): SimplifiedAst.Constraint.Fact =
      SimplifiedAst.Constraint.Fact(Predicate.Head.simplify(tast.head))

    def simplify(tast: TypedAst.Constraint.Rule)(implicit genSym: GenSym): SimplifiedAst.Constraint.Rule = {
      implicit val genSym = new GenSym
      val head = Predicate.Head.simplify(tast.head)
      val body = tast.body.map(Predicate.Body.simplify)
      SimplifiedAst.Constraint.Rule(head, body)
    }
  }

  object Definition {
    def simplify(tast: TypedAst.Definition.BoundedLattice)(implicit genSym: GenSym): SimplifiedAst.Definition.Lattice = tast match {
      case TypedAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        import Expression.{simplify => s}
        SimplifiedAst.Definition.Lattice(tpe, s(bot), s(top), s(leq), s(lub), s(glb), loc)
    }

    def simplify(tast: TypedAst.Definition.Constant)(implicit genSym: GenSym): SimplifiedAst.Definition.Constant =
      SimplifiedAst.Definition.Constant(tast.name, Expression.simplify(tast.exp), tast.tpe, tast.loc)

    def simplify(tast: TypedAst.Definition.Index)(implicit genSym: GenSym): SimplifiedAst.Definition.Index =
      SimplifiedAst.Definition.Index(tast.name, tast.indexes, tast.loc)
  }

  object Directives {
    def simplify(tast: TypedAst.Directives)(implicit genSym: GenSym): SimplifiedAst.Directives =
      SimplifiedAst.Directives(tast.directives map simplify)

    def simplify(tast: TypedAst.Directive)(implicit genSym: GenSym): SimplifiedAst.Directive = tast match {
      case TypedAst.Directive.AssertFact(fact, loc) => throw new UnsupportedOperationException // TODO: To be removed?
      case TypedAst.Directive.AssertRule(rule, loc) => throw new UnsupportedOperationException // TODO: To be removed?
    }
  }

  object Expression {
    def simplify(tast: TypedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = tast match {
      case TypedAst.Expression.Lit(lit, tpe, loc) => Literal.simplify(lit)
      case TypedAst.Expression.Var(ident, tpe, loc) =>
        // TODO: Variable numbering
        SimplifiedAst.Expression.Var(ident, -1, tpe, loc)
      case TypedAst.Expression.Ref(name, tpe, loc) => SimplifiedAst.Expression.Ref(name, tpe, loc)
      case TypedAst.Expression.Hook(hook, tpe, loc) => SimplifiedAst.Expression.Hook(hook, tpe, loc)
      case TypedAst.Expression.Lambda(annotations, args, body, tpe, loc) =>
        SimplifiedAst.Expression.Lambda(annotations, args map Simplifier.simplify, simplify(body), tpe, loc)
      case TypedAst.Expression.Apply(e, args, tpe, loc) =>
        SimplifiedAst.Expression.Apply3(simplify(e), args map simplify, tpe, loc)
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
      case TypedAst.Expression.Let(ident, e1, e2, tpe, loc) =>
        // TODO: Variable numbering
        SimplifiedAst.Expression.Let(ident, -1, simplify(e1), simplify(e2), tpe, loc)

      case TypedAst.Expression.Match(exp0, rules, tpe, loc) =>
        import SimplifiedAst.{Expression => SExp}

        /**
          * Given the code:
          *
          * ....
          *
          * The structure of the generate code is as follows:
          *
          * let v' = v in
          *   ...
          */

        /**
          * First, we generate a temporary variable `matchVar` for the match expression `matchExp`
          * to avoid the match expression from being evaluated by every case test.
          *
          * The `matchVar` is used by every case in the pattern match to test the value.
          */
        val matchVar = genSym.fresh2()
        val matchExp = simplify(exp0)

        /**
          * Second, we generate a synthetic rule that matches anything and always throws a match error.
          */
        val fallthrough = (TypedAst.Pattern.Wildcard, TypedAst.Expression.Error(tpe, loc)) // TODO: This should be a match error.

        /**
          * Third, we construct all the cases (with the fallthrough) and generate a fresh variable name for each.
          */
        val cases = rules ::: fallthrough :: Nil
        val vars = cases.map(_ => genSym.fresh2())

        /**
          * Fourth, we construct the inner most expression.
          * This is a call to the first lambda variable which triggers the pattern match.
          */
        val Xzero = SExp.Apply3(SExp.Var(vars.head, -1, Type.Lambda(List(), tpe), loc), List(), tpe, loc)

        /**
          * Fifth, we fold over each case and generate all the remaning lambdas.
          * During the fold, we track the name of the current and of the next lambda variable.
          */


        val lambdaVars = rules.map(_ => genSym.fresh2())

        val zero = SExp.Apply3(SExp.Var(lambdaVars.head, -1, /* TODO: Verify type */ exp0.tpe, loc), List(), Type.Lambda(List.empty, tpe), loc)
        val result = (rules zip lambdaVars.sliding(2).toList).foldLeft(zero: SExp) {
          case (exp, ((pat, body), List(currName, nextName))) =>
            val lambdaBody = Pattern.simplify(List(pat), List(matchVar), simplify(body), SExp.Apply3(SExp.Var(nextName, -1, /* TODO: Verify type. */ Type.Lambda(List.empty, body.tpe), loc), List.empty, tpe, loc))
            val lambda = SExp.Lambda(Ast.Annotations(List.empty), List.empty, lambdaBody, Type.Lambda(List.empty, tpe), loc)
            SExp.Let(currName, -1, lambda, exp, tpe, loc)
        }

        val err = SExp.MatchError(tpe, loc)
        val lamBody = Pattern.simplify(List(rules.last._1), List(matchVar), simplify(rules.last._2), err)
        val lam = SExp.Lambda(Ast.Annotations(List.empty), List.empty, lamBody, Type.Lambda(List.empty, tpe), loc)
        val inner = SExp.Let(lambdaVars.last, -1, lam, result, tpe, loc)

        // Finally, we can generate the outer let binding the match value expression.
        SExp.Let(matchVar, -1, matchExp, inner, tpe, loc)

      case TypedAst.Expression.Tag(enum, tag, e, tpe, loc) =>
        SimplifiedAst.Expression.Tag(enum, tag, simplify(e), tpe, loc)
      case TypedAst.Expression.Tuple(elms, tpe, loc) =>
        SimplifiedAst.Expression.Tuple(elms map simplify, tpe, loc)
      case TypedAst.Expression.Set(elms, tpe, loc) =>
        SimplifiedAst.Expression.Set(elms map simplify, tpe, loc)
      case TypedAst.Expression.Error(tpe, loc) =>
        SimplifiedAst.Expression.Error(tpe, loc)
    }
  }

  object Pattern {

    import SimplifiedAst.{Expression => SExp}
    import TypedAst.Pattern._

    // TODO: Have some one carefully peer-review this. esp. w.r.t. types.

    /**
      * Eliminates pattern matching.
      */
    def simplify(xs: List[TypedAst.Pattern],
                 ys: List[Name.Ident],
                 succ: SExp,
                 fail: SExp)(implicit genSym: GenSym): SExp = (xs, ys) match {
      /**
        * There are no more patterns and variables to match.
        *
        * The pattern was match successfully and we simply return the body expression.
        */
      case (Nil, Nil) => succ

      /**
        * Matching a wildcard is guaranteed to succeed.
        *
        * We proceed by recursion on the remaining patterns and variables.
        */
      case (Wildcard(tpe, loc) :: ps, v :: vs) =>
        simplify(ps, vs, succ, fail)

      /**
        * Matching a variable is guaranteed to succeed.
        *
        * We proceed by constructing a let-binding that binds the value
        * of the match variable `ident` to the variable `v`.
        * The body of the let-binding is computed by recursion on the
        * remaining patterns and variables.
        */
      case (Var(ident, tpe, loc) :: ps, v :: vs) =>
        val exp = simplify(ps, vs, succ, fail)
        SExp.Let(ident, -1, SExp.Var(v, -1, tpe, loc), exp, succ.tpe, loc)

      /**
        * Matching a literal may succeed or fail.
        *
        * We generate a binary expression testing whether the literal `lit`
        * matches the variable `v` and then we generate an if-then-else
        * expression where the consequent expression is determined by
        * recursion on the remaining patterns and variables and the
        * alternative expression is `fail`.
        */
      case (Lit(lit, tpe, loc) :: ps, v :: vs) =>
        val exp = simplify(ps, vs, succ, fail)
        val cond = SExp.Binary(Equal, Literal.simplify(lit), SExp.Var(v, -1, tpe, loc), Type.Bool, loc)
        SExp.IfThenElse(cond, exp, fail, succ.tpe, loc)

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
        val cond = SExp.CheckTag(tag, SExp.Var(v, -1, tpe, loc), loc)
        val freshVar = genSym.fresh2()
        val exp = SExp.Let(freshVar, -1, SExp.GetTagValue(SExp.Var(v, -1, tpe, loc), pat.tpe, loc), succ, succ.tpe, loc)
        // TODO: FIX BUG HERE
        //  val consequent = simplify(pat :: ps, freshVar :: vs, exp, fail)
        val consequent = SExp.Let(freshVar, -1, SExp.GetTagValue(SExp.Var(v, -1, tpe, loc), pat.tpe, loc), succ, succ.tpe, loc)
        SExp.IfThenElse(cond, consequent, fail, succ.tpe, loc)

      /**
        * Matching a tuple may succeed or fail.
        *
        * We generate a fresh variable and let-binding for each component of the
        * tuple and then we recurse on the nested patterns and freshly generated
        * variables.
        */
      case (Tuple(elms, tpe, loc) :: ps, v :: vs) =>
        val freshVars = elms.map(_ => genSym.fresh2())
        val zero = simplify(elms ::: ps, freshVars ::: vs, succ, fail)
        (elms zip freshVars zipWithIndex).foldRight(zero) {
          case (((pat, name), idx), exp) =>
            SExp.Let(name, -1, SExp.GetTupleIndex(SExp.Var(v, -1, pat.tpe, loc), idx, pat.tpe, loc), exp, succ.tpe, loc)
        }

    }
  }

  object Literal {
    def simplify(tast: TypedAst.Literal)(implicit genSym: GenSym): SimplifiedAst.Expression = tast match {
      case TypedAst.Literal.Unit(loc) => SimplifiedAst.Expression.Unit
      case TypedAst.Literal.Bool(b, loc) =>
        if (b) SimplifiedAst.Expression.True else SimplifiedAst.Expression.False
      case TypedAst.Literal.Int(i, loc) => SimplifiedAst.Expression.Int32(i)
      case TypedAst.Literal.Str(s, loc) => SimplifiedAst.Expression.Str(s)
      case TypedAst.Literal.Tag(enum, tag, lit, tpe, loc) => SimplifiedAst.Expression.Tag(enum, tag, simplify(lit), tpe, loc)
      case TypedAst.Literal.Tuple(elms, tpe, loc) => SimplifiedAst.Expression.Tuple(elms map simplify, tpe, loc)
      case TypedAst.Literal.Set(elms, tpe, loc) => SimplifiedAst.Expression.Set(elms map simplify, tpe, loc)
    }
  }

  object Predicate {

    object Head {
      def simplify(tast: TypedAst.Predicate.Head)(implicit genSym: GenSym): SimplifiedAst.Predicate.Head = tast match {
        case TypedAst.Predicate.Head.Relation(name, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Head.Relation(name, terms map Term.simplify, tpe, loc)
      }
    }

    object Body {
      def simplify(tast: TypedAst.Predicate.Body)(implicit genSym: GenSym): SimplifiedAst.Predicate.Body = tast match {
        case TypedAst.Predicate.Body.Collection(name, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Body.Collection(name, terms map Term.simplify, tpe, loc)
        case TypedAst.Predicate.Body.ApplyFilter(name, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Body.ApplyFilter(name, terms map Term.simplify, tpe, loc)
        case TypedAst.Predicate.Body.ApplyHookFilter(hook, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Body.ApplyHookFilter(hook, terms map Term.simplify, tpe, loc)
        case TypedAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc) =>
          SimplifiedAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc)
        case TypedAst.Predicate.Body.Loop(ident, term, tpe, loc) =>
          SimplifiedAst.Predicate.Body.Loop(ident, Term.simplify(term), tpe, loc)
      }
    }

  }

  object Term {
    def simplify(tast: TypedAst.Term.Head)(implicit genSym: GenSym): SimplifiedAst.Term.Head = tast match {
      case TypedAst.Term.Head.Var(ident, tpe, loc) => SimplifiedAst.Term.Head.Var(ident, tpe, loc)
      case TypedAst.Term.Head.Lit(lit, tpe, loc) => SimplifiedAst.Term.Head.Exp(Literal.simplify(lit), tpe, loc)
      case TypedAst.Term.Head.Apply(name, args, tpe, loc) => SimplifiedAst.Term.Head.Apply(name, args map simplify, tpe, loc)
      case TypedAst.Term.Head.ApplyHook(hook, args, tpe, loc) => SimplifiedAst.Term.Head.ApplyHook(hook, args map simplify, tpe, loc)
    }

    def simplify(tast: TypedAst.Term.Body)(implicit genSym: GenSym): SimplifiedAst.Term.Body = tast match {
      case TypedAst.Term.Body.Wildcard(tpe, loc) => SimplifiedAst.Term.Body.Wildcard(tpe, loc)
      case TypedAst.Term.Body.Var(ident, tpe, loc) => SimplifiedAst.Term.Body.Var(ident, -1, tpe, loc)
      case TypedAst.Term.Body.Lit(lit, tpe, loc) => SimplifiedAst.Term.Body.Exp(Literal.simplify(lit), tpe, loc)
    }
  }

  def simplify(tast: TypedAst.Attribute): SimplifiedAst.Attribute =
    SimplifiedAst.Attribute(tast.ident, tast.tpe)

  def simplify(tast: TypedAst.FormalArg): SimplifiedAst.FormalArg =
    SimplifiedAst.FormalArg(tast.ident, tast.tpe)
}
