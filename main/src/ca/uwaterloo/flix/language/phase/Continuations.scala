package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object Continuations extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Transforms all expressions in the given AST `root` into continuation passing style (CPS).
    *
    * The transformation is currently very naive and could be optimized to yield better performance.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {

    if (flix.options.debug) {
      println("PRE CONTINUATIONS -------------------------------")
      println()
      println(PrettyPrinter.Typed.fmtRoot(root).fmt(TerminalContext.AnsiTerminal))
    }

    // Put gen sym into implicit scope.
    implicit val _ = flix.genSym

    // Create a map for each def from sym -> sym'
    val defSymMap: Map[Symbol.DefnSym, (Symbol.DefnSym, TypeParam)] = root.defs map {
      case (sym, defn) => {
        val genericTypeParam = TypeParam(Name.Ident(SourcePosition.Unknown, "Generic Type", SourcePosition.Unknown), Type.freshTypeVar(), defn.loc)
        sym -> (Symbol.freshDefnSym(sym), genericTypeParam)
      }
    }

    // Rewrite f(...) = ... to f(...) = f'(..., id)
    val newDefs1: Map[Symbol.DefnSym, Def] = root.defs map {
      case (sym, defn) => sym -> visitDefn(defn, defSymMap)
    }

    // For each function f(...), make an f'(..., k) with visit(f.body, k)
    val newDefs2: Map[Symbol.DefnSym, Def] = root.defs map {
      case (sym, defn) => defSymMap(sym)._1 -> defToCPS(defn, defSymMap)
    }

    val result = root.copy(defs = newDefs1 ++ newDefs2)

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println("POST CONTINUATIONS -------------------------------")
      println()
      println(PrettyPrinter.Typed.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
    }
    result.toSuccess
  }

  /**
    * Rewrite the body of each def to call a new def, which takes a continuation as parameter
    */
  def visitDefn(defn: Def, defSymMap: Map[Symbol.DefnSym, (Symbol.DefnSym, TypeParam)])(implicit genSym: GenSym): Def = {

    // todo sjj: aux help
    // todo sjj: should we make the id function polymorphic in its type (x: t -> x: t) rather than (x: defn.tpe -> defn.tpe)
    // Make an id function, x -> x, to pass as continuation argument
    val freshSym = Symbol.freshVarSym()
    val freshSymVar = Expression.Var(freshSym, getReturnType(defn.tpe), defn.eff, defn.loc)
    val id = mkLambda(freshSym, getReturnType(defn.tpe), freshSymVar)

    // find the arguments of f, which should be used to call f'
    assert(defn.fparams.length == 1)
    val fparam = defn.fparams.head
    val arg = Expression.Var(fparam.sym, fparam.tpe, empEff(), fparam.loc)

    // create f' as Def
    val (defnPrimeName, genericTypeParam) = defSymMap(defn.sym)
    val defnPrime = Expression.Def(defnPrimeName, fixFuncType(defn.tpe, getReturnType(defn.tpe))/*Type.mkUncurriedArrow(List(fparam.tpe, Type.mkArrow(getReturnType(defn.tpe), getReturnType(defn.tpe))), getReturnType(defn.tpe))*/, defn.eff, defn.loc)

    // the new body of f is to call f' with the arguments of f and a continuation (which is id)
    val body = Expression.ApplyWithKont(defnPrime, arg, id, getReturnType(defnPrime.tpe), defn.eff, defn.loc)

    defn.copy(exp = body)
  }

  /**
    * Perform CPS transformation of each function
    */
  def defToCPS(defn: Def, defSymMap: Map[Symbol.DefnSym, (Symbol.DefnSym, TypeParam)])(implicit genSym: GenSym): Def = {
    val genericTypeParam: TypeParam = defSymMap(defn.sym)._2

    // Add generic continuation as formal parameter to f'
    val kontSym = Symbol.freshVarSym("k")
    val kontTpe = Type.mkArrow(getReturnType(defn.tpe), genericTypeParam.tpe)
    val kontParam = FormalParam(kontSym, Modifiers.Empty, kontTpe, defn.loc)
    val kontVar = Expression.Var(kontSym, kontTpe, empEff(), defn.loc)

    // Body = visitExp(f.exp, k)
    val defnTpe = fixFuncType(defn.tpe, genericTypeParam.tpe)

    val body = visitExp(defn.exp, kontVar, genericTypeParam.tpe, defSymMap)

    defn.copy(exp = body, fparams = defn.fparams :+ kontParam, tparams = defn.tparams :+ genericTypeParam, tpe = defnTpe)
  }

  // todo sjj: check subtree for shift/reset (dyr funktion til start)
  // todo sjj rename cps transform, only add to one visitDefn
  def visitExp(exp0: Expression, kont0: Expression, kont0ReturnType: Type, defSymMap: Map[Symbol.DefnSym, (Symbol.DefnSym, TypeParam)])(implicit genSym: GenSym): Expression = {
    exp0 match {

      //
      // Unit. Apply `kont0` to the value.
      //
      case Expression.Unit(loc) => mkApplyCont(kont0, exp0, empEff(), loc)

      //
      // True. Apply `kont0` to the value.
      //
      case Expression.True(loc) => mkApplyCont(kont0, exp0, empEff(), loc)

      case Expression.False(loc) => mkApplyCont(kont0, exp0, empEff(), loc)

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
        visitExp(exp, kont1, kont0ReturnType, defSymMap)
      }

      case Expression.Binary(op, exp1, exp2, tpe, _, loc) => {
        // Introduce a fresh variable symbol for the lambda.
        val freshOperand1Sym = Symbol.freshVarSym() // TODO SJJ: What does the text do?
        val freshOperand1Var = Expression.Var(freshOperand1Sym, exp1.tpe, empEff(), loc)
        val freshOperand2Sym = Symbol.freshVarSym()
        val freshOperand2Var = Expression.Var(freshOperand2Sym, exp2.tpe, empEff(), loc)

        val body = mkApplyCont(kont0, Expression.Binary(op, freshOperand1Var, freshOperand2Var, tpe, empEff(), loc), empEff(), loc)
        val kont2 = mkLambda(freshOperand2Sym, freshOperand2Var.tpe, body)
        val kont15 = visitExp(exp2, kont2, kont0ReturnType, defSymMap) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
        val kont1 = mkLambda(freshOperand1Sym, freshOperand1Var.tpe, kont15)
        visitExp(exp1, kont1, kont0ReturnType, defSymMap)
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
        val e2 = visitExp(exp2, kont0, kont0ReturnType, defSymMap)
        val e3 = visitExp(exp3, kont0, kont0ReturnType, defSymMap)
        val e = Expression.IfThenElse(freshCondVar, e2, e3, kont0ReturnType, empEff(), loc)

        // Constructs the lambda to pass as the continuation to the evaluation of the conditional.
        val lambda = mkLambda(freshCondSym, Type.Bool, e)

        // Recurse on the conditional.
        visitExp(exp1, lambda, kont0ReturnType, defSymMap)
      }

      case Expression.Let(sym, exp1, exp2, tpe, _, loc) => {
        val kont1 = mkLambda(sym, exp1.tpe, visitExp(exp2, kont0, kont0ReturnType, defSymMap))
        visitExp(exp1, kont1, kont0ReturnType, defSymMap)
      }

      case Expression.Tuple(elms, tpe, eff, loc) => {
        visitExps(elms, kont0, kont0ReturnType, l => mkApplyCont(kont0, Expression.Tuple(l, tpe, eff, loc), eff, loc), defSymMap)
      }

      case Expression.Lambda(fparam, exp, tpe, eff, loc) => {
        // make a new lambda with type exp.tpe -> generic
        val genericTypeParam = TypeParam(Name.Ident(SourcePosition.Unknown, "Generic Type", SourcePosition.Unknown), Type.freshTypeVar(), exp.loc)
        val freshKontSym = Symbol.freshVarSym("k")
        val freshKontTpe = Type.mkArrow(getReturnType(exp.tpe), genericTypeParam.tpe)
        val freshKontParam = FormalParam(freshKontSym, Modifiers.Empty, freshKontTpe, exp.loc)
        val freshKontVar = Expression.Var(freshKontSym, freshKontTpe, empEff(), exp.loc)

        // kont0(x -> e) becomes kont0((x,k) -> visitExp(e, k))
        val e = visitExp(exp, freshKontVar, freshKontTpe, defSymMap)
        val lambda = Expression.LambdaWithKont(fparam, freshKontParam, e, tpe, eff, loc)
        mkApplyCont(kont0, lambda, eff, loc)
      }

      case Expression.Def(sym, tpe, eff, loc) => {
        if (defSymMap.contains(sym)) {
          val (defnPrimeName, genericTypeParam) = defSymMap(sym)
          //Expression.Def(defnPrimeName, )
          return mkApplyCont(kont0, Expression.Def(defnPrimeName, fixFuncType(tpe, kont0ReturnType), eff, loc), eff, loc)
        }
        ???
      }

      case Expression.Apply(exp1, exp2, tpe, eff, loc) => {
        val f = (l: List[Expression]) => Expression.ApplyWithKont(l.head, l.last, kont0, tpe, eff, loc)
        val out = visitExps(List(exp1, exp2), kont0, kont0ReturnType, f, defSymMap)
        out
      }

      case _ => exp0
    }
  }

  /**
    * Convert a List of Expressions to CPS.
    *
    * E.g. f(e1,e2) becomes visitExp(e1, x => visitExp(e2, y => k(f(x, y))))
    * @param exps list of Expression to convert to CPS
    * @param kont0 the base continuation
    * @param kont0ReturnType the type of the base continuation
    * @param f function to specialize the final Expression (i.e. the Expression of the caller)
    * @return Expression with the list converted to CPS
    */
  private def visitExps(exps: List[Expression], kont0: Expression, kont0ReturnType: Type, f: List[Expression] => Expression, defSymMap: Map[Symbol.DefnSym, (Symbol.DefnSym, TypeParam)])(implicit genSym: GenSym): Expression = {
    // TODO SJJ: Handle CPure expressions
    val loc = SourceLocation.Generated

    // create new symbols for the lambdas
    val freshSyms = exps map { exp =>
      val freshSym = Symbol.freshVarSym()
      (exp, freshSym)
    }

    // in the base case, we apply the continuation with f("parameters")
    val baseCase: Expression = f(freshSyms.map(x => Expression.Var(x._2, fixFuncType(x._1.tpe, kont0ReturnType), empEff(), loc)))

    // process all elements of the list
    freshSyms.foldRight(baseCase) {(syms, kont) =>
      val (exp, freshSym) = syms
      val freshSymVar = Expression.Var(freshSym, fixFuncType(exp.tpe, kont0ReturnType), empEff(), loc)
      val kont1 = mkLambda(freshSymVar.sym, freshSymVar.tpe, kont)
      visitExp(exp, kont1, kont0ReturnType, defSymMap)
    }
  }

  private def fixFuncType(tpe: Type, kont0ReturnType: Type): Type = {
    if (tpe.isArrow) {
      val kType = Type.mkArrow(getReturnType(tpe), kont0ReturnType)
      val lst: List[Type] = tpe.typeArguments.init :+ kType
      Type.mkUncurriedArrow(lst, kont0ReturnType)
    } else {
      tpe
    }
  }

  /**
    * Placeholder method for when control effects are supported.
    */
  private def isCPure(exp: Expression): Boolean = false

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
