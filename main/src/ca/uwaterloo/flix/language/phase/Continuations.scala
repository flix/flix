package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object Continuations extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Transforms all expressions in the given AST `root` into continuation passing style (CPS).
    *
    * The transformation is currently very naive and could be optimized to yield better performance.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {

    // Put gen sym into implicit scope.
    implicit val _ = flix.genSym
    print("h"+root.defs)
    print("\n\ntest\n\n")
    // todo map fra def sym til cps def sym

    // Create a map for each def from sym -> sym'
    val defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym] = root.defs map {
      case (sym, defn) => sym -> Symbol.freshDefnSym(sym)
    }

    // Rewrite f(...) = ... to f(...) = f'(..., id)
    val newDefs1 = root.defs map {
      case (sym, defn) => sym -> visitDefn(defn, defSymMap)
    }

    // For each function f(...), make an f'(..., id)
    val newDefs2 = root.defs map {
      case (sym, defn) => defSymMap(sym) -> visitDefnAlt(defn, defSymMap)
    }

    val out = root.copy(defs = newDefs1 ++ newDefs2).toSuccess
    print()
    print(out.get.defs)
    out
  }

  /**
    * Rewrite the body of each def to call a new def, which takes a continuation as parameter
    */
  def visitDefn(defn: Def, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Def = {
    // todo sjj: make example
    // Make an id function, x -> x, to pass as continuation argument
    val freshSym = Symbol.freshVarSym("shim")
    val freshSymVar = Expression.Var(freshSym, defn.tpe, defn.eff, defn.loc)
    val id = mkLambda(freshSym, defn.tpe, freshSymVar)

    // todo sjj: what about Eff?
    // Rebind the body of 'def' to call the new function given by defSymMap
    val defnApply: Expression = Expression.Def(defSymMap(defn.sym), defn.tpe, defn.eff, defn.loc)
    val body = (defn.fparams :+ id.fparam).foldLeft(defnApply){
      (acc, fparam) =>
        Expression.Apply(acc, Expression.Var(fparam.sym, fparam.tpe, empEff(), fparam.loc), getReturnType(acc.tpe), empEff(), defn.loc)
    }

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

    // todo sjj: visitExp should take defSymMap? (E.g. if f(x) = f(x-1), then f'(x, k) = f'(x-1, k) rather than f'(x, k) = f(x-1, k) )
    // todo sjj: var correct here
    defn.copy(exp = visitExp(defn.exp, Expression.Var(kontSym, kontTpe, empEff(), defn.loc), kontTpe), fparams = defn.fparams :+ kontFParam)
  }

  // todo sjj rename cps transform, only add to one visitDefn
  def visitExp(exp0: Expression, kont0: Expression, kont0Type: Type)(implicit genSym: GenSym): Expression = exp0 match {

    //
    // Unit. Apply `kont0` to the value.
    //
    case Expression.Unit(loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    //
    // True. Apply `kont0` to the value.
    //
    case Expression.True(loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.False(loc) => mkApplyCont(kont0, exp0, empEff(),loc)

    case Expression.Char(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Float32(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Float64(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Int8(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Int16(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Int32(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Int64(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.BigInt(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Str(lit, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

    case Expression.Var(sym, tpe, eff, loc) => mkApplyCont(kont0, exp0, empEff(), loc)

      // todo sjj: make cases

    case Expression.Unary(op, exp, tpe, _, loc) => {
      val freshOperandSym = Symbol.freshVarSym() // TODO SJJ: What does the text do?
      val freshOperandVar = Expression.Var(freshOperandSym, exp.tpe, empEff(), loc)
      val body = Expression.Unary(op, freshOperandVar, tpe, empEff(), loc)
      val kont1 = mkLambda(freshOperandSym, freshOperandVar.tpe, mkApplyCont(kont0, body, empEff(), loc))
      visitExp(exp, kont1, kont0Type)
    }

    case Expression.Binary(op, exp1, exp2, tpe, _, loc) => {
      // Introduce a fresh variable symbol for the lambda.
      val freshOperand1Sym = Symbol.freshVarSym() // TODO SJJ: What does the text do?
      val freshOperand1Var = Expression.Var(freshOperand1Sym, exp1.tpe, empEff(), loc)
      val freshOperand2Sym = Symbol.freshVarSym()
      val freshOperand2Var = Expression.Var(freshOperand2Sym, exp2.tpe, empEff(), loc)

      val body = mkApplyCont(kont0, Expression.Binary(op, freshOperand1Var, freshOperand2Var, tpe, empEff(), loc), empEff(), loc)
      val kont2 = mkLambda(freshOperand2Sym, freshOperand2Var.tpe, body)
      val kont15 = visitExp(exp2, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      val kont1 = mkLambda(freshOperand1Sym, freshOperand1Var.tpe, kont15)
      visitExp(exp1, kont1, kont0Type)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) => {
      //
      // Evaluate the conditional expression `exp1` passing a lambda that
      // selects the appropriate branch where to continue execution.
      //

      // Introduce a fresh variable symbol for the lambda.
      val freshCondSym = Symbol.freshVarSym()
      val freshCondVar = Expression.Var(freshCondSym, Type.Bool, empEff(), loc)

      // Construct an expression that branches on the variable symbol and
      // continues execution in the CPS converted version of one of the two branches.
      val e2 = visitExp(exp2, kont0, kont0Type)
      val e3 = visitExp(exp3, kont0, kont0Type)
      val e = Expression.IfThenElse(freshCondVar, e2, e3, kont0Type, empEff(), loc)

      // Constructs the lambda to pass as the continuation to the evaluation of the conditional.
      val lambda = mkLambda(freshCondSym, Type.Bool, e)

      // Recurse on the conditional.
      visitExp(exp1, lambda, kont0Type)
    }

    // todo sjj: make cases

    case Expression.Let(sym, exp1, exp2, tpe, _, loc) => {
      val kont1 = mkLambda(sym, exp1.tpe, visitExp(exp2, kont0, kont0Type))
      visitExp(exp1, kont1, kont0Type)
    }

    case Expression.Tuple(elms, tpe, eff, loc) => {
      val syms = elms.map(exp => {
        val sym = Symbol.freshVarSym()
        (exp, sym, Expression.Var(sym, exp.tpe, empEff(), loc))})
      val baseCase: Expression = mkApplyCont(kont0, Expression.Tuple(syms.map(e => e._3), tpe, empEff(), loc), empEff(), loc)
      syms.foldRight(baseCase){(syms, kont) =>
        val exp = syms._1
        val sym = syms._2
        val varr = syms._3

        val kont2 = mkLambda(sym, varr.tpe, kont)
        visitExp(exp, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      }
    }

    case _ => exp0
  }

  // todo func to make list of exp to cps

  /**
    * Returns a lambda expression with the given symbol `sym` as a formal parameter,
    * the given type `argType` as its argument type and the given body `exp`.
    */
  private def mkLambda(sym: Symbol.VarSym, argType: Type, exp: Expression): Expression.Lambda = {
    val loc = exp.loc
    val fparam = FormalParam(sym, Modifiers.Empty, argType, loc)
    Expression.Lambda(fparam, exp, Type.mkArrow(argType, exp.tpe), empEff(), loc)
  }

  /**
    * Returns an apply expression that applies the given continuation `kont0` to the value or variable expression `exp0`.
    */
  private def mkApplyCont(kont0: Expression, exp0: Expression, eff: ca.uwaterloo.flix.language.ast.Eff, loc: SourceLocation) = {
    val kontReturnType = getReturnType(kont0.tpe)
    Expression.Apply(kont0, exp0, kontReturnType, eff, loc)
  }

  /**
    * Returns the return type of the given type `tpe` which must be an arrow type.
    */
  private def getReturnType(tpe: Type): Type = {
    if (tpe.isArrow) {
      tpe.typeArguments.last
    }
    else {
      tpe
    }
  }

  private def empEff(): ca.uwaterloo.flix.language.ast.Eff = ca.uwaterloo.flix.language.ast.Eff.Empty

}
