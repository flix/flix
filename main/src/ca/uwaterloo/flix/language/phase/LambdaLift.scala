package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.SimplifiedAst
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object LambdaLift {

  /**
    * Performs lambda lifting on all definitions in the AST.
    */
  def lift(root: SimplifiedAst.Root)(implicit genSym: GenSym): SimplifiedAst.Root = {
    val defs = root.constants.flatMap { case (_, defn) => lift(defn) }
    root.copy(constants = defs)
  }

  /**
    * Returns a map of definitions where each nested lambda inside the given declaration has been lifted out.
    *
    * The original definition is contained in the map.
    */
  def lift(decl: SimplifiedAst.Definition.Constant)(implicit genSym: GenSym): Map[Symbol.Resolved, SimplifiedAst.Definition.Constant] = {

    def visit(m: mutable.Map[Symbol.Resolved, SimplifiedAst.Definition.Constant], e: SimplifiedAst.Expression): SimplifiedAst.Expression =
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
        case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => e
        case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) => e
        case SimplifiedAst.Expression.Ref(name, tpe, loc) => e

        case lam: SimplifiedAst.Expression.Lambda =>
          // Lift the lambda to a top-level definition, and replacing the Lambda expression with a Ref.

          // First, recursively visit the lambda body, lifting any inner lambdas.
          val exp = visit(m, lam.body)

          // Then, generate a fresh name for the lifted lambda.
          val name = genSym.freshDefn(decl.name.parts)

          // If the lambda term has an envVar, then it has free variables. So rewrite the arguments and type to take an
          // additional parameter: the closure environment
          val (args, tpe) = lam.envVar match {
            case Some(envVar) =>
              val newArgs = lam.args :+ SimplifiedAst.FormalArg(envVar, Type.ClosureEnv)
              val newTpe = Type.Lambda(lam.tpe.args :+ Type.ClosureEnv, lam.tpe.retTpe)
              (newArgs, newTpe)
            case None => (lam.args, lam.tpe)
          }

          // Create a new top-level definition, using the fresh name and lifted body.
          val defn = SimplifiedAst.Definition.Constant(Ast.Annotations(Nil), name, args, exp, tpe, lam.loc)

          // Update the map that holds newly-generated definitions
          m += (name -> defn)

          // Finally, replace this current Lambda node with a Ref to the newly-generated name.
          SimplifiedAst.Expression.Ref(name, lam.tpe, lam.loc)

        case SimplifiedAst.Expression.Hook(hook, tpe, loc) => e
        case SimplifiedAst.Expression.MkClosureRef(ref, envVar, freeVars, tpe, loc) => e

        case SimplifiedAst.Expression.MkClosure(lambda, envVar, freeVars, tpe, loc) =>
          // Replace the MkClosure node with a MkClosureRef node, since the Lambda has been replaced by a Ref.
          visit(m, lambda) match {
            case ref: SimplifiedAst.Expression.Ref =>
              SimplifiedAst.Expression.MkClosureRef(ref, envVar, freeVars, tpe, loc)
            case _ => throw InternalCompilerException(s"Unexpected expression: '$lambda'.")
          }

        case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) =>
          SimplifiedAst.Expression.ApplyRef(name, args.map(visit(m, _)), tpe, loc)
        case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
          SimplifiedAst.Expression.Apply(visit(m, exp), args.map(visit(m, _)), tpe, loc)
        case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
          SimplifiedAst.Expression.Unary(op, visit(m, exp), tpe, loc)
        case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
          SimplifiedAst.Expression.Binary(op, visit(m, exp1), visit(m, exp2), tpe, loc)
        case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          SimplifiedAst.Expression.IfThenElse(visit(m, exp1), visit(m, exp2), visit(m, exp3), tpe, loc)
        case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
          SimplifiedAst.Expression.Let(ident, offset, visit(m, exp1), visit(m, exp2), tpe, loc)
        case SimplifiedAst.Expression.CheckTag(tag, exp, loc) =>
          SimplifiedAst.Expression.CheckTag(tag, visit(m, exp), loc)
        case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
          SimplifiedAst.Expression.GetTagValue(tag, visit(m, exp), tpe, loc)
        case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
          SimplifiedAst.Expression.Tag(enum, tag, visit(m, exp), tpe, loc)
        case SimplifiedAst.Expression.GetTupleIndex(exp, offset, tpe, loc) =>
          SimplifiedAst.Expression.GetTupleIndex(visit(m, exp), offset, tpe, loc)
        case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
          SimplifiedAst.Expression.Tuple(elms.map(visit(m, _)), tpe, loc)
        case SimplifiedAst.Expression.CheckNil(exp, loc) =>
          SimplifiedAst.Expression.CheckNil(visit(m, exp), loc)
        case SimplifiedAst.Expression.CheckCons(exp, loc) =>
          SimplifiedAst.Expression.CheckCons(visit(m, exp), loc)
        case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
          SimplifiedAst.Expression.FSet(elms.map(visit(m, _)), tpe, loc)
        case SimplifiedAst.Expression.UserError(tpe, loc) => e
        case SimplifiedAst.Expression.MatchError(tpe, loc) => e
        case SimplifiedAst.Expression.SwitchError(tpe, loc) => e
      }

    // Map to hold all newly generated definitions.
    val m = mutable.Map.empty[Symbol.Resolved, SimplifiedAst.Definition.Constant]

    // Closure convert the expression.
    val converted = ClosureConv.convert(decl.exp)

    // Perform lambda lifting. Returns the expression of the top-level function.
    val lifted = visit(m, converted)

    // Add the top-level function to the map of generated functions.
    val newConstant = SimplifiedAst.Definition.Constant(Ast.Annotations(Nil), decl.name, decl.formals, lifted, decl.tpe, decl.loc)
    m += (decl.name -> newConstant)
    m.toMap
  }

}
