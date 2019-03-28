package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, Type, TypedAst}
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

    // Create a map for each def from sym -> sym'
    val defSymMap: Map[Symbol.DefnSym, (Symbol.DefnSym, TypeParam)] = root.defs map {
      case (sym, defn) => {
        //todo sjj: is this how to create a generic type
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

    root.copy(defs = newDefs1 ++ newDefs2).toSuccess
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
    val defnPrime = Expression.Def(defnPrimeName, Type.mkUncurriedArrow(List(fparam.tpe, Type.mkArrow(getReturnType(defn.tpe), genericTypeParam.tpe)), genericTypeParam.tpe), defn.eff, defn.loc)

    // the new body of f is to call f' with the arguments of f and a continuation (which is id)
    val body = Expression.ApplyWithKont(defnPrime, arg, id, defnPrime.tpe, defn.eff, defn.loc)

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
    val defnTpe = Type.mkUncurriedArrow(List(defn.fparams.head.tpe, Type.mkArrow(getReturnType(defn.tpe), genericTypeParam.tpe)), genericTypeParam.tpe)

    Type.mkUncurriedArrow(List(Type.Int32, Type.mkArrow(Type.Int32, Type.Int32)), Type.Int32)
    val body = CPSTransform(defn.exp, kontVar, kontTpe)

    defn.copy(exp = body, fparams = defn.fparams :+ kontParam, tparams = defn.tparams :+ genericTypeParam, tpe = defnTpe)
  }

  // todo sjj: check subtree for shift/reset (dyr funktion til start)
  // todo sjj rename cps transform, only add to one visitDefn
  def CPSTransform(exp0: Expression, kont0: Expression, kont0Type: Type)(implicit genSym: GenSym): Expression = exp0 match {

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
      CPSTransform(exp, kont1, kont0Type)
    }

    case Expression.Binary(op, exp1, exp2, tpe, _, loc) => {
      // Introduce a fresh variable symbol for the lambda.
      val freshOperand1Sym = Symbol.freshVarSym() // TODO SJJ: What does the text do?
      val freshOperand1Var = Expression.Var(freshOperand1Sym, exp1.tpe, empEff(), loc)
      val freshOperand2Sym = Symbol.freshVarSym()
      val freshOperand2Var = Expression.Var(freshOperand2Sym, exp2.tpe, empEff(), loc)

      val body = mkApplyCont(kont0, Expression.Binary(op, freshOperand1Var, freshOperand2Var, tpe, empEff(), loc), empEff(), loc)
      val kont2 = mkLambda(freshOperand2Sym, freshOperand2Var.tpe, body)
      val kont15 = CPSTransform(exp2, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      val kont1 = mkLambda(freshOperand1Sym, freshOperand1Var.tpe, kont15)
      CPSTransform(exp1, kont1, kont0Type)
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
      val e2 = CPSTransform(exp2, kont0, kont0Type)
      val e3 = CPSTransform(exp3, kont0, kont0Type)
      val e = Expression.IfThenElse(freshCondVar, e2, e3, kont0Type, empEff(), loc)

      // Constructs the lambda to pass as the continuation to the evaluation of the conditional.
      val lambda = mkLambda(freshCondSym, Type.Bool, e)

      // Recurse on the conditional.
      CPSTransform(exp1, lambda, kont0Type)
    }

    case Expression.Let(sym, exp1, exp2, tpe, _, loc) => {
      val kont1 = mkLambda(sym, exp1.tpe, CPSTransform(exp2, kont0, kont0Type))
      CPSTransform(exp1, kont1, kont0Type)
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
        CPSTransform(exp, kont2, kont0Type) // TODO SJJ: Is kont0type the return type of the kont0 lambda?
      }
    }

    case Expression.Lambda(fparam, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym()
      val freshSymTpe = exp.tpe
      val freshSymVar = Expression.Var(freshSym, freshSymTpe, eff, loc)

      // fparam => y, where y is the result of visitExp(exp)
      val lambda = mkLambda(fparam.sym, tpe, freshSymVar)
      // freshSym => kont(exp => freshSym)
      val newKont = mkLambda(freshSym, freshSymTpe, Expression.Apply(kont0, lambda, tpe, eff, loc))
      CPSTransform(exp, newKont, kont0Type)
    // todo sjj: make cases
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
    val res = Expression.Lambda(fparam, exp, Type.mkArrow(argType, exp.tpe), empEff(), loc)
    val e = "test"
    res
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
