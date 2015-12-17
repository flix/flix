package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.{BinaryOperator, TypedAst}
import ca.uwaterloo.flix.language.backend.ir.SimplifiedAst

object Simplifier {

  // TODO: This phase:
  // - Eliminates switch and match.
  // - Introduce TestTag, extract etc.
  // - Inlines literals into Exp.
  // - Numbers every argument etc.

  type Cont = SimplifiedAst.Expression => SimplifiedAst.Expression

  // TODO: Does this phase require continuations?

  object Literals {

    def simplify(tast: TypedAst.Literal): SimplifiedAst.Expression = ???

  }

  object Expressions {

    def simplify(tast: TypedAst.Expression, k: SimplifiedAst.Expression => SimplifiedAst.Expression): SimplifiedAst.Expression = tast match {
      case TypedAst.Expression.Lit(lit, tpe, loc) => ???

      case TypedAst.Expression.Var(ident, tpe, loc) =>
        k(SimplifiedAst.Expression.Var(ident, tpe, loc))

      case TypedAst.Expression.Ref(name, tpe, loc) =>
        SimplifiedAst.Expression.Ref(name, tpe, loc)

      case TypedAst.Expression.Lambda(annotations, args, body, tpe, loc) => ???

      case TypedAst.Expression.Apply(exp, args, tpe, loc) => ???

      case TypedAst.Expression.Unary(op, te, tpe, loc) =>
        simplify(te, e =>
          k(SimplifiedAst.Expression.Unary(op, e, tpe, loc)))

      case TypedAst.Expression.Binary(op, te1, te2, tpe, loc) =>
        simplify(te1, e1 =>
          simplify(te2, e2 =>
            k(SimplifiedAst.Expression.Binary(op, e1, e2, tpe, loc))))

      case TypedAst.Expression.IfThenElse(te1, te2, te3, tpe, loc) =>
        simplify(te1, e1 =>
          simplify(te2, e2 =>
            simplify(te3, e3 =>
              k(SimplifiedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)))))

      case TypedAst.Expression.Match(te, rules, tpe, loc) =>
        val zero: SimplifiedAst.Expression = ???

        rules.foldRight(zero) {
          case ((pat, body), acc) => Matches.simplify(pat, ???, body, acc, k)
        }


    }

  }

  object Matches {

    def simplify(pat: TypedAst.Pattern, matchExp: TypedAst.Expression, bodyExp: TypedAst.Expression, failExp: SimplifiedAst.Expression, k: Cont): SimplifiedAst.Expression = pat match {
      case TypedAst.Pattern.Wildcard(tpe, loc) => Expressions.simplify(bodyExp, k)
      case TypedAst.Pattern.Var(ident, tpe, loc) =>
        Expressions.simplify(matchExp, e1 =>
          Expressions.simplify(bodyExp, e2 =>
            k(SimplifiedAst.Expression.Let(ident, e1, e2, tpe, loc))))
      case TypedAst.Pattern.Lit(tlit, tpe, loc) =>
        Expressions.simplify(matchExp, me => {
          Expressions.simplify(bodyExp, e2 => {
            val lit = Literals.simplify(tlit)
            val cond = SimplifiedAst.Expression.Binary(BinaryOperator.Equal, ???, me, TypedAst.Type.Bool, loc)
            k(SimplifiedAst.Expression.IfThenElse(cond, e2, failExp, tpe, loc))
          })
        })

    }

  }

}
