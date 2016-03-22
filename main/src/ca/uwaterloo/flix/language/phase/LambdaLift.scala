package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.SimplifiedAst
import ca.uwaterloo.flix.language.ast.Symbol
import scala.collection.mutable

object LambdaLift {

  def lift(root: SimplifiedAst.Root)(implicit genSym: GenSym): SimplifiedAst.Root = {

    // Lambda lift every function definition.
    val defs = root.constants.foldLeft(Map.empty[Symbol.Resolved, SimplifiedAst.Definition.Constant]) {
      case (macc, (name, defn)) => macc ++ lift(defn)
    }

    root.copy(constants = defs)
  }


  def lift(decl: SimplifiedAst.Definition.Constant)(implicit genSym: GenSym): Map[Symbol.Resolved, SimplifiedAst.Definition.Constant] = {

    // Map to hold all newly generated definitions.
    val m = mutable.Map.empty[Symbol.Resolved, SimplifiedAst.Definition.Constant]

    def visit(e: SimplifiedAst.Expression): SimplifiedAst.Expression = e match {
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
      case SimplifiedAst.Expression.Lambda(ann, args, body, tpe, loc) =>
        val exp = visit(body)
        val ns = decl.name.parts
        val name = genSym.freshDefn(ns)
        val defn = SimplifiedAst.Definition.Constant(name, exp, tpe, loc)
        m += (name -> defn)
        SimplifiedAst.Expression.Ref(name, tpe, loc)

      case SimplifiedAst.Expression.Hook(hook, tpe, loc) => e
      case SimplifiedAst.Expression.Apply(name, args, tpe, loc) => ??? // TODO: Deprecated
      case SimplifiedAst.Expression.Apply3(lambda, args, tpe, loc) =>
        val e = visit(lambda)
        val as = args map visit
        SimplifiedAst.Expression.Apply3(e, as, tpe, loc)

      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
        val e1 = visit(exp)
        SimplifiedAst.Expression.Unary(op, e1, tpe, loc)

      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        SimplifiedAst.Expression.Binary(op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        SimplifiedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        SimplifiedAst.Expression.Let(ident, offset, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.CheckTag(tag, exp, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.CheckTag(tag, e, loc)

      case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.GetTagValue(tag, e, tpe, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.GetTupleIndex(exp, offset, tpe, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.GetTupleIndex(e, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visit
        SimplifiedAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.CheckNil(exp, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.CheckNil(e, loc)

      case SimplifiedAst.Expression.CheckCons(exp, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.CheckCons(e, loc)

      case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
        val es = elms map visit
        SimplifiedAst.Expression.FSet(es, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe, loc) => e
      case SimplifiedAst.Expression.MatchError(tpe, loc) => e
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => e

      case SimplifiedAst.Expression.MkClosure(exp, envVar, freeVars, tpe, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.MkClosure(e, envVar, freeVars, tpe, loc)

      case SimplifiedAst.Expression.ClosureVar(env, name, tpe, loc) => e

      case SimplifiedAst.Expression.ApplyClosure(exp, args, tpe, loc) =>
        val e = visit(exp)
        SimplifiedAst.Expression.ApplyClosure(e, args, tpe, loc)
    }

    // Closure convert the expression.
    val e = ClosureConv.Expressions.convert(decl.exp)

    // Perform lambda lifting. Returns the expression of the top-level function.
    val lam = visit(e)

    // Add the top-level function to the map of generated functions.
    m += (decl.name -> SimplifiedAst.Definition.Constant(decl.name, lam, decl.tpe, decl.loc))

    // Return the generated definitions.
    m.toMap
  }

}
