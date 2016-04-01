package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Name, SimplifiedAst, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object ClosureConv {

  /**
    * Performs closure conversion on the given expression `e`.
    */
  def convert(exp: SimplifiedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = exp match {
    case SimplifiedAst.Expression.Unit => exp
    case SimplifiedAst.Expression.True => exp
    case SimplifiedAst.Expression.False => exp
    case SimplifiedAst.Expression.Char(lit) => exp
    case SimplifiedAst.Expression.Float32(lit) => exp
    case SimplifiedAst.Expression.Float64(lit) => exp
    case SimplifiedAst.Expression.Int8(lit) => exp
    case SimplifiedAst.Expression.Int16(lit) => exp
    case SimplifiedAst.Expression.Int32(lit) => exp
    case SimplifiedAst.Expression.Int64(lit) => exp
    case SimplifiedAst.Expression.Str(lit) => exp
    case SimplifiedAst.Expression.LoadBool(n, o) => exp
    case SimplifiedAst.Expression.LoadInt8(b, o) => exp
    case SimplifiedAst.Expression.LoadInt16(b, o) => exp
    case SimplifiedAst.Expression.LoadInt32(b, o) => exp
    case SimplifiedAst.Expression.StoreBool(b, o, v) => exp
    case SimplifiedAst.Expression.StoreInt8(b, o, v) => exp
    case SimplifiedAst.Expression.StoreInt16(b, o, v) => exp
    case SimplifiedAst.Expression.StoreInt32(b, o, v) => exp
    case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => exp
    case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) => exp

    case e: SimplifiedAst.Expression.Ref =>
      // If we encounter a Ref that has a lambda type (and is not being called in an Apply),
      // i.e. the Ref will evaluate to a lambda, we replace it with a MkClosureRef. Otherwise we leave it alone.
      e.tpe match {
        case t: Type.Lambda => SimplifiedAst.Expression.MkClosureRef(e, genSym.fresh2("env"), Set.empty, t, e.loc)
        case _ => e
      }

    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      // Convert lambdas to closures. This is the main part of the `convert` function.
      // Closure conversion happens as follows:

      // First, we generate a fresh variable name to refer to the closure environment.
      // At run time, the closure environment will be created and then bound to `envVar`.
      val envVar = genSym.fresh2("env")

      // Next, we collect the free variables in the lambda expression.
      // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
      val freeVars = freeVariables(exp)

      // We create a substitution map: every free variable will be replaced by a reference to a closure variable,
      // i.e. a lookup in the closure environment.
      val m = freeVars.map { case (ident, t) =>
        ident.name -> SimplifiedAst.Expression.ClosureVar(envVar, ident, t, ident.loc)
      }.toMap

      // Apply the substitution to the body expression of the lambda.
      // Then we recurse on the result, to apply closure conversion on any lambdas within the body.
      val lambda = SimplifiedAst.Expression.Lambda(args, convert(substitute(m, body)), tpe, loc)

      // At this point, `e` is the original lambda expression, but with all free variables replaced with lookups in the
      // closure environment. Additionally, any lambdas within the body have also been closure converted.

      // We return a MkClosure node, which contains the code/lambda `e` and the closure environment name `envVar`. We
      // also cache `freeVars`. The closure will actually be created at run time, where the closure environment will be
      // created with bindings for the variables, and then passed as an extra argument to the lambda. Thus, all free
      // variables have been eliminated, and in a later phase, we can lift the lambda to a top-level definition.
      SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars.map(_._1), tpe, loc)

    case SimplifiedAst.Expression.Hook(hook, tpe, loc) => exp
    case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion: '$exp'.")
    case SimplifiedAst.Expression.MkClosureRef(ref, envVar, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion: '$exp'.")
    case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion: '$exp'.")

    case SimplifiedAst.Expression.Apply(e, args, tpe, loc) =>
      // We're trying to call some expression `e`. If `e` is a Ref, then it's a top-level function, so we directly call
      // it with ApplyRef. We remove the Ref node and don't recurse on it to avoid creating a closure.
      e match {
        case e: SimplifiedAst.Expression.Ref => SimplifiedAst.Expression.ApplyRef(e.name, args.map(convert), tpe, loc)
        case _ => SimplifiedAst.Expression.Apply(convert(e), args.map(convert), tpe, loc)
      }

    case SimplifiedAst.Expression.Unary(op, e, tpe, loc) =>
      SimplifiedAst.Expression.Unary(op, convert(e), tpe, loc)
    case SimplifiedAst.Expression.Binary(op, e1, e2, tpe, loc) =>
      SimplifiedAst.Expression.Binary(op, convert(e1), convert(e2), tpe, loc)
    case SimplifiedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc) =>
      SimplifiedAst.Expression.IfThenElse(convert(e1), convert(e2), convert(e3), tpe, loc)
    case SimplifiedAst.Expression.Let(ident, offset, e1, e2, tpe, loc) =>
      SimplifiedAst.Expression.Let(ident, offset, convert(e1), convert(e2), tpe, loc)
    case SimplifiedAst.Expression.CheckTag(tag, e, loc) =>
      SimplifiedAst.Expression.CheckTag(tag, convert(e), loc)
    case SimplifiedAst.Expression.GetTagValue(tag, e, tpe, loc) =>
      SimplifiedAst.Expression.GetTagValue(tag, convert(e), tpe, loc)
    case SimplifiedAst.Expression.Tag(enum, tag, e, tpe, loc) =>
      SimplifiedAst.Expression.Tag(enum, tag, convert(e), tpe, loc)
    case SimplifiedAst.Expression.GetTupleIndex(e, offset, tpe, loc) =>
      SimplifiedAst.Expression.GetTupleIndex(convert(e), offset, tpe, loc)
    case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
      SimplifiedAst.Expression.Tuple(elms.map(convert), tpe, loc)
    case SimplifiedAst.Expression.CheckNil(e, loc) =>
      SimplifiedAst.Expression.CheckNil(convert(e), loc)
    case SimplifiedAst.Expression.CheckCons(e, loc) =>
      SimplifiedAst.Expression.CheckCons(convert(e), loc)
    case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
      SimplifiedAst.Expression.FSet(elms.map(convert), tpe, loc)
    case SimplifiedAst.Expression.Existential(params, e, loc) =>
      SimplifiedAst.Expression.Existential(params, convert(e), loc)
    case SimplifiedAst.Expression.Universal(params, e, loc) =>
      SimplifiedAst.Expression.Universal(params, convert(e), loc)
    case SimplifiedAst.Expression.UserError(tpe, loc) => exp
    case SimplifiedAst.Expression.MatchError(tpe, loc) => exp
    case SimplifiedAst.Expression.SwitchError(tpe, loc) => exp
  }

  /**
    * Replaces variables in the given expression `e` by using the given substitution map `m`.
    *
    * Requires that all variables in any expression in `m` are fresh.
    */
  def substitute(m: Map[String, SimplifiedAst.Expression], e: SimplifiedAst.Expression): SimplifiedAst.Expression =
    e match {
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

    case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion (substitution): '$e'.")
    case SimplifiedAst.Expression.Ref(name, tpe, loc) => e
    case SimplifiedAst.Expression.Hook(hook, tpe, loc) => e

    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      // Don't substitute bound variables. We handle this by temporarily modifying the substitution map, removing
      // substitutions that conflict with the bound arguments.
      val bound = args.map(_.ident.name)
      val tempMap = m.filterNot { case (k, _) => bound.contains(k) }
      SimplifiedAst.Expression.Lambda(args, substitute(tempMap, body), tpe, loc)

    case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion (substitution): '$e'.")
    case SimplifiedAst.Expression.MkClosureRef(ref, envVar, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion (substitution): '$e'.")
    case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) =>
      SimplifiedAst.Expression.ApplyRef(name, args.map(substitute(m, _)), tpe, loc)
    case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
      SimplifiedAst.Expression.Apply(substitute(m, exp), args.map(substitute(m, _)), tpe, loc)
    case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
      SimplifiedAst.Expression.Unary(op, substitute(m, exp), tpe, loc)
    case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
      SimplifiedAst.Expression.Binary(op, substitute(m, exp1), substitute(m, exp2), tpe, loc)
    case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      SimplifiedAst.Expression.IfThenElse(substitute(m, exp1), substitute(m, exp2), substitute(m, exp3), tpe, loc)

    case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
      // Don't substitute bound variables. We handle this by temporarily modifying the substitution map, removing
      // substitutions that conflict with the bound arguments.
      val tempMap = m.filterNot { case (k, _) => k == ident.name }
      SimplifiedAst.Expression.Let(ident, offset, substitute(m, exp1), substitute(tempMap, exp2), tpe, loc)

    case SimplifiedAst.Expression.CheckTag(tag, exp, loc) =>
      SimplifiedAst.Expression.CheckTag(tag, substitute(m, exp), loc)
    case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
      SimplifiedAst.Expression.GetTagValue(tag, substitute(m, exp), tpe, loc)
    case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
      SimplifiedAst.Expression.Tag(enum, tag, substitute(m, exp), tpe, loc)
    case SimplifiedAst.Expression.GetTupleIndex(exp, offset, tpe, loc) =>
      SimplifiedAst.Expression.GetTupleIndex(substitute(m, exp), offset, tpe, loc)
    case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
      SimplifiedAst.Expression.Tuple(elms.map(substitute(m, _)), tpe, loc)
    case SimplifiedAst.Expression.CheckNil(exp, loc) =>
      SimplifiedAst.Expression.CheckNil(substitute(m, exp), loc)
    case SimplifiedAst.Expression.CheckCons(exp, loc) =>
      SimplifiedAst.Expression.CheckCons(substitute(m, exp), loc)
    case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
      SimplifiedAst.Expression.FSet(elms.map(substitute(m, _)), tpe, loc)
    case SimplifiedAst.Expression.UserError(tpe, loc) => e
    case SimplifiedAst.Expression.MatchError(tpe, loc) => e
    case SimplifiedAst.Expression.SwitchError(tpe, loc) => e
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
    case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.Ref(name, tpe, loc) => Set.empty
    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      val bound = args.map(_.ident.name)
      freeVariables(body).filterNot { v => bound.contains(v._1.name) }
    case SimplifiedAst.Expression.Hook(hook, tpe, loc) => Set.empty
    case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.MkClosureRef(ref, envVar, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) => args.flatMap(freeVariables).toSet
    case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
      freeVariables(exp) ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2)
    case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2) ++ freeVariables(exp3)
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
  }

}
