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

    // todo map fra def sym til cps def sym

    // Create a map for each def from sym -> sym'
    val defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym] = root.defs map {
      case (sym, defn) => sym -> Symbol.freshDefnSym(sym)
    }

    // Rewrite f(...) = ... to f(...) = f'(..., id)
    val newDefs1 = root.defs map {
      case (sym, defn) => sym -> visitDefn(defn, defSymMap)
    }

    val newDefs2 = root.defs map {
      case (sym, defn) => defSymMap(sym) -> visitDefnAlt(defn, defSymMap)
    }

    root.copy(defs = newDefs1 ++ newDefs2).toSuccess
  }

  /**
    * Rewrite the body of each def to call a new def, which takes a continuation as parameter
    */
  def visitDefn(defn: Def, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Def = {
    // Make a fresh variable
    val freshSym = Symbol.freshVarSym()
    val freshSymVar = Expression.Var(freshSym, defn.tpe, defn.loc)
    // Make a new list of args, which includes the 'Id' function as a continuation
    val args = defn.fparams.map(f => Expression.Var(f.sym, f.tpe, f.loc)) :+ mkLambda(freshSym, defn.tpe, freshSymVar)
    // Rebind the body of 'def' to call the new function given by defSymMap
    val body = Expression.ApplyDefTail(defSymMap(defn.sym), args, defn.tpe, defn.loc)
    defn.copy(exp = body)
  }

  /**
    * Perform CPS transformation of each function
    */
  def visitDefnAlt(defn: Def, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Def = {
    // Add continuation as parameter to def
    val kontSym = Symbol.freshVarSym("kont")
    // TODO SJJ: How to make the resulttype of kont generic? Does it even matter as we are after the Typer?
    // Type of continuation is a function type from defn.tpe -> T
    val kontTpe = Type.mkArrow(defn.tpe, Type.freshTypeVar())
    // TODO SJJ: Modifiers?
    val kontFParam = FormalParam(kontSym, Modifiers.Empty , kontTpe , defn.loc)


    // visitExp should take defSymMap? (E.g. if f(x) = f(x-1), then f'(x, k) = f'(x-1, k) rather than f'(x, k) = f(x-1, k) )
    defn.copy(exp = visitExp(defn.exp, ???, ???), fparams = defn.fparams :+ kontFParam)
  }

  // todo sjj rename cps transform, only add to one visitDefn
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

    case Expression.Unary(sop, op, exp, tpe, loc) => {
      val freshOperandSym = Symbol.freshVarSym() // TODO SJJ: What does the text do?
      val freshOperandVar = Expression.Var(freshOperandSym, exp.tpe, loc)
      val body = Expression.Unary(sop, op, freshOperandVar, tpe, loc)
      val kont1 = mkLambda(freshOperandSym, freshOperandVar.tpe, mkApplyCont(kont0, body))
      visitExp(exp, kont1, kont0Type)
    }

    case Expression.Binary(sop, op, exp1, exp2, tpe: Type, loc) => {

      //TODO SJJ: What is a semantic operator

      // Introduce a fresh variable symbol for the lambda.
      val freshOperand1Sym = Symbol.freshVarSym() // TODO SJJ: What does the text do?
      val freshOperand1Var = Expression.Var(freshOperand1Sym, exp1.tpe, loc)
      val freshOperand2Sym = Symbol.freshVarSym()
      val freshOperand2Var = Expression.Var(freshOperand2Sym, exp2.tpe, loc)

      val body = mkApplyCont(kont0, Expression.Binary(sop, op, freshOperand1Var, freshOperand2Var, tpe, loc))
      val kont2 = mkLambda(freshOperand2Sym, freshOperand2Var.tpe, body)
      val kont15 = visitExp(exp2, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      val kont1 = mkLambda(freshOperand1Sym, freshOperand1Var.tpe, kont15)
      visitExp(exp1, kont1, kont0Type)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => {
      //
      // Evaluate the conditional expression `exp1` passing a lambda that
      // selects the appropriate branch where to continue execution.
      //

      // Introduce a fresh variable symbol for the lambda.
      val freshCondSym = Symbol.freshVarSym()
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

    // todo sjj: make cases

    case Expression.Let(sym, exp1, exp2, tpe, loc) => {
      val kont1 = mkLambda(sym, exp1.tpe, visitExp(exp2, kont0, kont0Type))
      visitExp(exp1, kont1, kont0Type)
    }

    case Expression.Tuple(elms, tpe, loc) => {
      val syms = elms.map(exp => {
        val sym = Symbol.freshVarSym()
        (exp, sym, Expression.Var(sym, exp.tpe, loc))})
      val baseCase: Expression = mkApplyCont(kont0, Expression.Tuple(syms.map(e => e._3), tpe, loc))
      syms.foldRight(baseCase){(syms, kont) =>
        val exp = syms._1
        val sym = syms._2
        val varr = syms._3

        val kont2 = mkLambda(sym, varr.tpe, kont)
        visitExp(exp, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      }
    }
  }

  // todo func to make list of exp to cps

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
