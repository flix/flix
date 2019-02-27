package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object Continuations extends Phase[Root, Root] {

  /**
    * Transforms all expressions in the given AST `root` into continuation passing style (CPS).
    *
    * The transformation is currently very naive and could be optimized to yield better performance.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {

    // Put gen sym into implicit scope.
    implicit val _ = flix.genSym

//    // TODO
//    root.defs map {
//      case (sym, defn) => visitDefn(defn)
//    }

    root.toSuccess
  }

  def visitExp(exp0: Expression, kont0: Expression.Lambda, kont0Type: Type)(implicit genSym: GenSym): Expression = exp0 match {

    //
    // Unit. Apply `kont0` to the value.
    //
    case Expression.Unit => mkApplyCont(kont0, exp0)

    //
    // True. Apply `kont0` to the value.
    //
    case Expression.True => mkApplyCont(kont0, exp0)

    case Expression.False => mkApplyCont(kont0, exp0)

    case Expression.Char(lit) => mkApplyCont(kont0, exp0)

    case Expression.Float32(lit) => mkApplyCont(kont0, exp0)

    case Expression.Float64(lit) => mkApplyCont(kont0, exp0)

    case Expression.Int8(lit) => mkApplyCont(kont0, exp0)

    case Expression.Int16(lit) => mkApplyCont(kont0, exp0)

    case Expression.Int32(lit) => mkApplyCont(kont0, exp0)

    case Expression.Int64(lit) => mkApplyCont(kont0, exp0)

    case Expression.BigInt(lit) => mkApplyCont(kont0, exp0)

    case Expression.Str(lit) => mkApplyCont(kont0, exp0)

    case Expression.Var(sym, tpe, loc) => mkApplyCont(kont0, exp0)

      // todo sjj: make cases
      
    case Expression.Binary(sop, op, exp1, exp2, tpe: Type, loc) =>

      //TODO SJJ: What is a semantic operator

      // Introduce a fresh variable symbol for the lambda.
      val freshOperand1Sym = Symbol.freshVarSym("x") // TODO SJJ: What does the text do?
      val freshOperand1Var = Expression.Var(freshOperand1Sym, exp1.tpe, loc)
      val freshOperand2Sym = Symbol.freshVarSym("y")
      val freshOperand2Var = Expression.Var(freshOperand2Sym, exp2.tpe, loc)

      val body = mkApplyCont(kont0,Expression.Binary(sop, op, freshOperand1Var, freshOperand2Var, tpe, loc))
      val kont2 = mkLambda(freshOperand2Sym, freshOperand2Var.tpe, body)
      val kont15 = visitExp(exp2, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      val kont1 = mkLambda(freshOperand1Sym, freshOperand1Var.tpe, kont15)
      visitExp(exp1, kont1, kont0Type)


    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      //
      // Evaluate the conditional expression `exp1` passing a lambda that
      // selects the appropriate branch where to continue execution.
      //

      // Introduce a fresh variable symbol for the lambda.
      val freshCondSym = Symbol.freshVarSym("r")
      val freshCondVar = Expression.Var(freshCondSym, Type.Bool, loc)

      // Construct an expression that branches on the variable symbol and
      // continues execution in the CPS converted version of one of the two branches.
      val e2 = visitExp(exp2, kont0, kont0Type)
      val e3 = visitExp(exp3, kont0, kont0Type)
      val e = Expression.IfThenElse(freshCondVar, e2, e3, kont0Type, loc)

      // Constructs the lambda to pass as the continuation to the evaluation of the conditional.
      val lambda = mkLambda(freshCondSym, Type.Bool, e)

      // Recurse on the conditional.
      visitExp(exp1, lambda, kont0Type)
  }

  /**
    * Returns a lambda expression with the given symbol `sym` as a formal parameter,
    * the given type `argType` as its argument type and the given body `exp`.
    */
  private def mkLambda(sym: Symbol.VarSym, argType: Type, exp: Expression): Expression.Lambda = {
    val loc = exp.loc
    val fparam = FormalParam(sym, Modifiers.Empty, argType, loc)
    Expression.Lambda(List(fparam), exp, Type.mkArrow(argType, exp.tpe), loc)
  }

  /**
    * Returns an apply expression that applies the given continuation `kont0` to the value or variable expression `exp0`.
    */
  private def mkApplyCont(kont0: Expression, exp0: Expression) = {
    val kontReturnType = getReturnType(kont0.tpe)
    Expression.Apply(kont0, List(exp0), kont0.tpe, SourceLocation.Generated)
  }

  /**
    * Returns the return type of the given type `tpe` which must be an arrow type.
    */
  private def getReturnType(tpe: Type): Type = {
    assert(tpe.isArrow)
    tpe.typeArguments.last
  }

}
