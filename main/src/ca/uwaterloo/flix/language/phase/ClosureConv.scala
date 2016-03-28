package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Name, SimplifiedAst, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object ClosureConv {

  object Expressions {

    /**
      * Performs closure conversion on the given expression `e`.
      */
    def convert(e: SimplifiedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = e match {
      case SimplifiedAst.Expression.Unit => e
      case SimplifiedAst.Expression.True => e
      case SimplifiedAst.Expression.False => e
      case SimplifiedAst.Expression.Char(lit) => e
      case SimplifiedAst.Expression.Float32(lit) => e
      case SimplifiedAst.Expression.Float64(lit) => e
      case SimplifiedAst.Expression.Int8(lit) => e
      case SimplifiedAst.Expression.Int16(lit) => e
      case SimplifiedAst.Expression.Int32(lit) => e
      case SimplifiedAst.Expression.Int64(lit) => e
      case SimplifiedAst.Expression.Str(lit) => e
      case SimplifiedAst.Expression.LoadBool(n, o) => e
      case SimplifiedAst.Expression.LoadInt8(b, o) => e
      case SimplifiedAst.Expression.LoadInt16(b, o) => e
      case SimplifiedAst.Expression.LoadInt32(b, o) => e
      case SimplifiedAst.Expression.StoreBool(b, o, v) => e
      case SimplifiedAst.Expression.StoreInt8(b, o, v) => e
      case SimplifiedAst.Expression.StoreInt16(b, o, v) => e
      case SimplifiedAst.Expression.StoreInt32(b, o, v) => e
      case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => e
      case SimplifiedAst.Expression.Ref(name, tpe, loc) => e
      case SimplifiedAst.Expression.Apply(name, args, tpe, loc) => e

      case SimplifiedAst.Expression.Apply3(lambda, args, tpe, loc) =>
        // TODO: Recurse on lambda. Then pattern match: Two cases: (1) Ref -> Apply (2) otherwise -> ApplyClosure
        // Replace Apply by ApplyClosure.
        SimplifiedAst.Expression.ApplyClosure(lambda, args, tpe, loc)

      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        // Generate a fresh variable to hold the environment variable.
        val envVar = genSym.fresh2("env")

        // Find the free variables in the expression.
        // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
        val freeVars = freeVariables(e)

        // Generate a substitution where every free variables is replaced by a reference to a closure variable.
        val m = freeVars.map { case (ident, t) =>
          ident.name -> SimplifiedAst.Expression.ClosureVar(envVar, ident, t, ident.loc)
        }.toMap

        // Apply the substitution to body expression of the lambda.
        val exp = SimplifiedAst.Expression.Lambda(args, substitute(m, body), tpe, loc)

        // Return the closure which consists of the (substituted) lambda expression and environment variable.
        SimplifiedAst.Expression.MkClosure(exp, envVar, freeVars.map(_._1), tpe, loc)

      case SimplifiedAst.Expression.Hook(hook, tpe, loc) => e

      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
        val e1 = convert(exp)
        SimplifiedAst.Expression.Unary(op, e1, tpe, loc)

      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val e1 = convert(exp1)
        val e2 = convert(exp2)
        SimplifiedAst.Expression.Binary(op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = convert(exp1)
        val e2 = convert(exp2)
        val e3 = convert(exp3)
        SimplifiedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        val e1 = convert(exp1)
        val e2 = convert(exp2)
        SimplifiedAst.Expression.Let(ident, offset, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.CheckTag(tag, exp, loc) =>
        val e = convert(exp)
        SimplifiedAst.Expression.CheckTag(tag, e, loc)

      case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
        val e = convert(exp)
        SimplifiedAst.Expression.GetTagValue(tag, e, tpe, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = convert(exp)
        SimplifiedAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.GetTupleIndex(exp, offset, tpe, loc) =>
        val e = convert(exp)
        SimplifiedAst.Expression.GetTupleIndex(e, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms.map(convert)
        SimplifiedAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.CheckNil(exp, loc) =>
        val e = convert(exp)
        SimplifiedAst.Expression.CheckNil(e, loc)

      case SimplifiedAst.Expression.CheckCons(exp, loc) =>
        val e = convert(exp)
        SimplifiedAst.Expression.CheckCons(e, loc)

      case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
        val es = elms.map(convert)
        SimplifiedAst.Expression.FSet(es, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe, loc) => e
      case SimplifiedAst.Expression.MatchError(tpe, loc) => e
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => e

      case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
        throw InternalCompilerException(s"Illegal expression during closure conversion: '$e'.")
      case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) =>
        throw InternalCompilerException(s"Illegal expression during closure conversion: '$e'.")
      case SimplifiedAst.Expression.ApplyClosure(clo, args, tpe, loc) =>
        throw InternalCompilerException(s"Illegal expression during closure conversion: '$e'.")

    }

    /**
      * Replaces variables in the given expression `e` by using the given substitution map `m`.
      *
      * Requires that all variables in any expression in `m` are fresh.
      */
    def substitute(m: Map[String, SimplifiedAst.Expression], e: SimplifiedAst.Expression): SimplifiedAst.Expression = e match {
      case SimplifiedAst.Expression.Unit => e
      case SimplifiedAst.Expression.True => e
      case SimplifiedAst.Expression.False => e
      case SimplifiedAst.Expression.Char(lit) => e
      case SimplifiedAst.Expression.Float32(lit) => e
      case SimplifiedAst.Expression.Float64(lit) => e
      case SimplifiedAst.Expression.Int8(lit) => e
      case SimplifiedAst.Expression.Int16(lit) => e
      case SimplifiedAst.Expression.Int32(lit) => e
      case SimplifiedAst.Expression.Int64(lit) => e
      case SimplifiedAst.Expression.Str(lit) => e
      case SimplifiedAst.Expression.LoadBool(n, o) => e
      case SimplifiedAst.Expression.LoadInt8(b, o) => e
      case SimplifiedAst.Expression.LoadInt16(b, o) => e
      case SimplifiedAst.Expression.LoadInt32(b, o) => e
      case SimplifiedAst.Expression.StoreBool(b, o, v) => e
      case SimplifiedAst.Expression.StoreInt8(b, o, v) => e
      case SimplifiedAst.Expression.StoreInt16(b, o, v) => e
      case SimplifiedAst.Expression.StoreInt32(b, o, v) => e
      case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => m.get(ident.name) match {
        case None => e
        case Some(r) => r
      }

      case SimplifiedAst.Expression.Ref(name, tpe, loc) => e
      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        // TODO: This doesn't seem quite correct.
        // We don't substitute if any of the arguments exists in the substitution map? What about other vars?
        val keys = m.keySet
        val bound = args.exists(a => keys.contains(a.ident.name))
        if (bound) {
          SimplifiedAst.Expression.Lambda(args, body, tpe, loc)
        } else {
          val e = substitute(m, body)
          SimplifiedAst.Expression.Lambda(args, e, tpe, loc)
        }

      case SimplifiedAst.Expression.Hook(hook, tpe, loc) => e
      case SimplifiedAst.Expression.Apply(name, args, tpe, loc) =>
        val es = args.map(substitute(m, _))
        SimplifiedAst.Expression.Apply(name, es, tpe, loc)
      case SimplifiedAst.Expression.Apply3(lambda, args, tpe, loc) =>
        val e = substitute(m, lambda)
        val es = args.map(substitute(m, _))
        SimplifiedAst.Expression.Apply3(e, es, tpe, loc)

      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
        val e1 = substitute(m, exp)
        SimplifiedAst.Expression.Unary(op, e1, tpe, loc)

      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val e1 = substitute(m, exp1)
        val e2 = substitute(m, exp2)
        SimplifiedAst.Expression.Binary(op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = substitute(m, exp1)
        val e2 = substitute(m, exp2)
        val e3 = substitute(m, exp3)
        SimplifiedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        // TODO: This doesn't seem quite correct.
        // We don't substitute if the let-var exists in the substitution map? What about other vars?
        val bound = m.keySet.contains(ident.name)
        if (bound) {
          val e1 = substitute(m, exp1)
          SimplifiedAst.Expression.Let(ident, offset, e1, exp2, tpe, loc)
        } else {
          val e1 = substitute(m, exp1)
          val e2 = substitute(m, exp2)
          SimplifiedAst.Expression.Let(ident, offset, e1, e2, tpe, loc)
        }

      case SimplifiedAst.Expression.CheckTag(tag, exp, loc) =>
        val e = substitute(m, exp)
        SimplifiedAst.Expression.CheckTag(tag, e, loc)

      case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
        val e = substitute(m, exp)
        SimplifiedAst.Expression.GetTagValue(tag, e, tpe, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = substitute(m, exp)
        SimplifiedAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.GetTupleIndex(exp, offset, tpe, loc) =>
        val e = substitute(m, exp)
        SimplifiedAst.Expression.GetTupleIndex(e, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms.map(substitute(m, _))
        SimplifiedAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.CheckNil(exp, loc) =>
        val e = substitute(m, exp)
        SimplifiedAst.Expression.CheckNil(e, loc)

      case SimplifiedAst.Expression.CheckCons(exp, loc) =>
        val e = substitute(m, exp)
        SimplifiedAst.Expression.CheckCons(e, loc)

      case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
        val es = elms.map(substitute(m, _))
        SimplifiedAst.Expression.FSet(es, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe, loc) => e
      case SimplifiedAst.Expression.MatchError(tpe, loc) => e
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => e

      case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e'.")

      case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e'.")

      case SimplifiedAst.Expression.ApplyClosure(clo, args, tpe, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e'.")

    }

    /**
      * Returns the free variables in the given expression `exp`.
      */
    def freeVariables(e: SimplifiedAst.Expression): Set[(Name.Ident, Type)] = e match {
      case SimplifiedAst.Expression.Unit => Set.empty
      case SimplifiedAst.Expression.True => Set.empty
      case SimplifiedAst.Expression.False => Set.empty
      case SimplifiedAst.Expression.Char(lit) => Set.empty
      case SimplifiedAst.Expression.Float32(lit) => Set.empty
      case SimplifiedAst.Expression.Float64(lit) => Set.empty
      case SimplifiedAst.Expression.Int8(lit) => Set.empty
      case SimplifiedAst.Expression.Int16(lit) => Set.empty
      case SimplifiedAst.Expression.Int32(lit) => Set.empty
      case SimplifiedAst.Expression.Int64(lit) => Set.empty
      case SimplifiedAst.Expression.Str(lit) => Set.empty
      case SimplifiedAst.Expression.LoadBool(n, o) => Set.empty
      case SimplifiedAst.Expression.LoadInt8(b, o) => Set.empty
      case SimplifiedAst.Expression.LoadInt16(b, o) => Set.empty
      case SimplifiedAst.Expression.LoadInt32(b, o) => Set.empty
      case SimplifiedAst.Expression.StoreBool(b, o, v) => Set.empty
      case SimplifiedAst.Expression.StoreInt8(b, o, v) => Set.empty
      case SimplifiedAst.Expression.StoreInt16(b, o, v) => Set.empty
      case SimplifiedAst.Expression.StoreInt32(b, o, v) => Set.empty
      case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => Set((ident, tpe))
      case SimplifiedAst.Expression.Ref(name, tpe, loc) => Set.empty
      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        val bound = args.map(_.ident.name)
        freeVariables(body).filterNot { v => bound.contains(v._1.name) }

      case SimplifiedAst.Expression.Hook(hook, tpe, loc) => Set.empty
      case SimplifiedAst.Expression.Apply(name, args, tpe, loc) => args.flatMap(freeVariables).toSet
      case SimplifiedAst.Expression.Apply3(lambda, args, tpe, loc) =>
        freeVariables(lambda) ++ args.flatMap(freeVariables)

      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) => freeVariables(exp)
      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) => freeVariables(exp1) ++ freeVariables(exp2)
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => freeVariables(exp1) ++ freeVariables(exp2) ++ freeVariables(exp3)
      case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        val bound = ident.name
        freeVariables(exp1) ++ freeVariables(exp2).filterNot { v => bound == v._1.name }

      case SimplifiedAst.Expression.CheckTag(tag, exp, loc) => freeVariables(exp)
      case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) => freeVariables(exp)
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) => freeVariables(exp)
      case SimplifiedAst.Expression.GetTupleIndex(base, offset, tpe, loc) => freeVariables(base)
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) => elms.flatMap(freeVariables).toSet
      case SimplifiedAst.Expression.CheckNil(exp, loc) => freeVariables(exp)
      case SimplifiedAst.Expression.CheckCons(exp, loc) => freeVariables(exp)
      case SimplifiedAst.Expression.FSet(elms, tpe, loc) => elms.flatMap(freeVariables).toSet
      case SimplifiedAst.Expression.UserError(tpe, loc) => Set.empty
      case SimplifiedAst.Expression.MatchError(tpe, loc) => Set.empty
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => Set.empty

      case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e'.")

      case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e'.")

      case SimplifiedAst.Expression.ApplyClosure(clo, args, tpe, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e'.")
    }

  }

}
