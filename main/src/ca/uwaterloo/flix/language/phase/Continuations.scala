package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.TerminalContext
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

    // Create a map for each def from f -> f'
    val defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym] = root.defs map {
      case (sym, defn) => sym -> Symbol.freshDefnSym(sym)
    }

    // Rewrite f(...) = ... to f(...) = f'(..., id)
    val newDefs1: Map[Symbol.DefnSym, Def] = root.defs map {
      case (sym, defn) => sym -> visitDefn(defn, defSymMap)
    }

    // For each function f(...), make an f'(..., k) with visit(f.body, k)
    val newDefs2: Map[Symbol.DefnSym, Def] = root.defs map {
      case (sym, defn) => defSymMap(sym) -> defToCPS(defn, defSymMap)
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
  def visitDefn(defn: Def, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Def = {

    // Make an id function, x -> x, to pass as continuation argument
    val freshSym = Symbol.freshVarSym()
    val returnType = getReturnType(defn.tpe)
    val freshSymVar = Expression.Var(freshSym, returnType, defn.eff, defn.loc)
    val id = mkLambda(freshSym, returnType, freshSymVar)

    // find the arguments of f, which should be used to call f'
    assert(defn.fparams.length == 1)
    val fparam = defn.fparams.head
    val arg = Expression.Var(fparam.sym, fparam.tpe, empEff(), fparam.loc)

    // create f' as Def
    val defnPrimeName = defSymMap(defn.sym)
    val defnPrime = Expression.Def(defnPrimeName, fixArrowType(defn.tpe, returnType), defn.eff, defn.loc)

    // the new body of f is to call f' with the arguments of f and a continuation (which is id)
    val body = Expression.ApplyWithKont(defnPrime, arg, id, getReturnType(defnPrime.tpe), defn.eff, defn.loc)

    defn.copy(exp = body)
  }

  /**
    * Perform CPS transformation of each function
    */
  def defToCPS(defn: Def, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Def = {
    val genericTypeParam = TypeParam(Name.Ident(SourcePosition.Unknown, "Generic Type", SourcePosition.Unknown), Type.freshTypeVar(), defn.loc)

    // Add generic continuation as formal parameter to f'
    val kontSym = Symbol.freshVarSym("k")
    val defCorrectedReturnType = fixArrowType(getReturnType(defn.tpe), genericTypeParam.tpe)
    val kontTpe = Type.mkArrow(defCorrectedReturnType, genericTypeParam.tpe)
    val kontParam = FormalParam(kontSym, Modifiers.Empty, kontTpe, defn.loc)
    val kontVar = Expression.Var(kontSym, kontTpe, empEff(), defn.loc)

    // Body = visitExp(f.exp, k)
    val defnTpe = fixArrowType(defn.tpe, genericTypeParam.tpe)
    val body = visitExp(defn.exp, kontVar, genericTypeParam.tpe, defSymMap)

    defn.copy(exp = body, fparams = defn.fparams.map(fp => fp.copy(tpe = fixArrowType(fp.tpe, genericTypeParam.tpe))) :+ kontParam, tparams = defn.tparams :+ genericTypeParam, tpe = defnTpe)
  }

  // todo sjj: check subtree for shift/reset (dyr funktion til start)
  // todo sjj rename cps transform, only add to one visitDefn
  def visitExp(exp0: Expression, kont0: Expression, kont0ReturnType: Type, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Expression = exp0 match {

      case Expression.Wild(tpe, eff, loc) => mkApplyCont(kont0, Expression.Wild(fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc)
      case Expression.Var(sym, tpe, eff, loc) => mkApplyCont(kont0, Expression.Var(sym, fixArrowType(tpe, kont0ReturnType), eff, loc), empEff(), loc)

      case Expression.Def(sym, tpe, eff, loc) => {
        if (defSymMap.contains(sym)) {
          val defnPrimeName = defSymMap(sym)
          return mkApplyCont(kont0, Expression.Def(defnPrimeName, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc)
        }
        //todo sjj: Can this ever be reached?
        ??? //exp0
      }

      case Expression.Eff(sym, tpe, eff, loc) => exp0

      case Expression.Hole(sym, tpe, eff, loc) => exp0

      case Expression.Unit(loc) => mkApplyCont(kont0, exp0, empEff(), loc)
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

      case Expression.Lambda(fparam, exp, tpe, eff, loc) => {
        // make a new lambda with type exp.tpe -> k0.tpe
        val freshKontSym = Symbol.freshVarSym("k")
        val freshKontTpe = Type.mkArrow(fixArrowType(getReturnType(tpe), kont0ReturnType), kont0ReturnType)
        val freshKontParam = FormalParam(freshKontSym, Modifiers.Empty, freshKontTpe, exp.loc)
        val freshKontVar = Expression.Var(freshKontSym, freshKontTpe, empEff(), exp.loc)

        // kont0(x -> e) becomes kont0((x,k) -> visitExp(e, k))
        val e = visitExp(exp, freshKontVar, kont0ReturnType, defSymMap)
        val lambda = Expression.LambdaWithKont(fparam, freshKontParam, e, fixArrowType(tpe, kont0ReturnType), eff, loc)
        mkApplyCont(kont0, lambda, eff, loc)
      }

      case Expression.LambdaWithKont(fparam1, fparam2, exp, tpe, eff, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")

      case Expression.Apply(exp1, exp2, tpe, eff, loc) => {
        visitExps(List(exp1, exp2), kont0ReturnType, l => Expression.ApplyWithKont(l.head, l.last, kont0, kont0ReturnType, eff, loc), defSymMap)
      }

      case Expression.ApplyWithKont(exp1, exp2, exp3, tpe, eff, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")

      case Expression.Unary(op, exp, tpe, eff, loc) => {
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.Unary(op, l.head, tpe, eff, loc), eff, loc), defSymMap)
      }

      case Expression.Binary(op, exp1, exp2, tpe, _, loc) => {
        visitExps(List(exp1, exp2), kont0ReturnType, l => mkApplyCont(kont0, Expression.Binary(op, l.head, l.last, fixArrowType(tpe, kont0ReturnType), empEff(), loc), empEff(), loc), defSymMap)
      }

      case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => {
        //todo sjj: could/should let be transformed into a lambda?
        visitExps(List(exp1), kont0ReturnType, l => Expression.Let(sym, l.head, visitExp(exp2, kont0, kont0ReturnType, defSymMap), kont0ReturnType, eff, loc), defSymMap)
      }

      case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => ??? //todo sjj

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => {
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

      case Expression.Match(exp, rules, tpe, eff, loc) =>
        // We wrongly assume that there is not shift/reset in the guard
        val newRules = rules.map(r => MatchRule(r.pat, r.guard, visitExp(r.exp, kont0, kont0ReturnType, defSymMap)))
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.Match(l.head, newRules, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.Switch(rules, tpe, eff, loc) =>
        //todo sjj: is this stupid to do before simplifier?
        val zero = Expression.SwitchError(tpe, eff, loc)
        visitExp(rules.foldRight(zero: Expression) {
          case ((e1, e2), acc) =>
            val cond = e1
            val body = e2
            Expression.IfThenElse(cond, body, acc, tpe, eff, loc)
        }, kont0, kont0ReturnType, defSymMap)

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l =>Expression.Tag(sym, tag, l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), defSymMap)

      case Expression.Tuple(elms, tpe, eff, loc) =>
        visitExps(elms, kont0ReturnType, l => mkApplyCont(kont0, Expression.Tuple(l, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.RecordEmpty(tpe, eff, loc) => ??? //todo sjj

      case Expression.RecordSelect(base, label, tpe, eff, loc) => ??? //todo sjj

      case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => ??? //todo sjj

      case Expression.RecordRestrict(label, rest, tpe, eff, loc) => ??? //todo sjj

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        visitExps(elms, kont0ReturnType, l => mkApplyCont(kont0, Expression.ArrayLit(l, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        visitExps(List(elm, len), kont0ReturnType, l => mkApplyCont(kont0, Expression.ArrayNew(l.head, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        visitExps(List(base, index), kont0ReturnType, l => mkApplyCont(kont0, Expression.ArrayLoad(l.head, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
        visitExps(List(base, index, elm), kont0ReturnType, l => Expression.ArrayStore(l.head, l.tail.head, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), defSymMap)

      case Expression.ArrayLength(base, tpe, eff, loc) =>
        visitExps(List(base), kont0ReturnType, l => mkApplyCont(kont0, Expression.ArrayLength(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, eff, loc) =>
        visitExps(List(base, startIndex, endIndex), kont0ReturnType, l => mkApplyCont(kont0, Expression.ArraySlice(l.head, l.tail.head, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.VectorLit(elms, tpe, eff, loc) =>
        visitExps(elms, kont0ReturnType, l => mkApplyCont(kont0, Expression.VectorLit(l, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.VectorNew(elm, len, tpe, eff, loc) =>
        visitExps(List(elm), kont0ReturnType, l => mkApplyCont(kont0, Expression.VectorNew(l.head, len, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.VectorLoad(base, index, tpe, eff, loc) =>
        visitExps(List(base), kont0ReturnType, l => mkApplyCont(kont0, Expression.VectorLoad(l.head, index, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
        visitExps(List(base, elm), kont0ReturnType, l => Expression.VectorStore(l.head, index, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), defSymMap)

      case Expression.VectorLength(base, tpe, eff, loc) =>
        visitExps(List(base), kont0ReturnType, l => mkApplyCont(kont0, Expression.VectorLength(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) =>
        visitExps(List(base, endIndex), kont0ReturnType, l => mkApplyCont(kont0, Expression.VectorSlice(l.head, startIndex, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.Ref(exp, tpe, eff, loc) => ??? //todo sjj

      case Expression.Deref(exp, tpe, eff, loc) => ??? //todo sjj

      case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

      case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? //todo sjj

      case Expression.Existential(fparam, exp, eff, loc) => ??? //todo sjj

      case Expression.Universal(fparam, exp, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.Universal(fparam, l.head, eff, loc), eff, loc), defSymMap)

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.Ascribe(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)


      case Expression.Cast(exp, tpe, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.Cast(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? //todo sjj

      case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => ??? //todo sjj

      case Expression.NativeField(field, tpe, eff, loc) => ??? //todo sjj

      case Expression.NativeMethod(method, args, tpe, eff, loc) =>
        // we assume that this type is not an arrowtype
        assert(!tpe.isArrow)
        visitExps(args, kont0ReturnType, l => mkApplyCont(kont0, Expression.NativeMethod(method, l, tpe, eff, loc), eff, loc), defSymMap)

      case Expression.NewChannel(exp, tpe, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.NewChannel(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.GetChannel(exp, tpe, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.GetChannel(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)


      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        visitExps(List(exp1, exp2), kont0ReturnType, l => mkApplyCont(kont0, Expression.PutChannel(l.head, l.last, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        visitExps(rules.map(_.chan), kont0ReturnType, l => {
          val newRules = rules.zip(l) map {
            case (SelectChannelRule(sym, chan, exp), newChan) => SelectChannelRule(sym, newChan, visitExp(exp, kont0, kont0ReturnType, defSymMap))
          }
          Expression.SelectChannel(newRules, default, kont0ReturnType, eff, loc)
        }, defSymMap)

      case Expression.Spawn(exp, tpe, eff, loc) =>
        val freshSym = Symbol.freshVarSym()
        val freshSymVar = Expression.Var(freshSym, exp.tpe, eff, loc)
        val id = mkLambda(freshSym, exp.tpe, freshSymVar)
        mkApplyCont(kont0, Expression.Spawn(visitExp(exp, id, kont0ReturnType, defSymMap), fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc)

      case Expression.Sleep(exp, tpe, eff, loc) =>
        visitExps(List(exp), kont0ReturnType, l => mkApplyCont(kont0, Expression.Sleep(l.head, fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc), defSymMap)

      case Expression.FixpointConstraint(c0, tpe, eff, loc) => ??? //todo sjj

      case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

      case Expression.FixpointSolve(exp, tpe, eff, loc) => ??? //todo sjj

      case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ??? //todo sjj

      case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

      case Expression.UserError(tpe, eff, loc) => mkApplyCont(kont0, Expression.UserError(fixArrowType(tpe, kont0ReturnType), eff, loc), eff, loc)

      case Expression.CPSShift(exp, tpe, eff, loc) =>
        exp match {
          case Expression.Lambda(lfparam, lexp, ltpe, leff, lloc) =>
            val freshSym = Symbol.freshVarSym()
            val freshSymVar = Expression.Var(freshSym, kont0ReturnType, eff, loc)
            val id = mkLambda(freshSym, kont0ReturnType, freshSymVar)
            val newLambda = Expression.Lambda(lfparam, visitExp(lexp, id, kont0ReturnType, defSymMap),ltpe, leff, lloc)

            Expression.Apply(newLambda, kont0, kont0ReturnType, eff, loc)

          case _ => throw InternalCompilerException("cps shift should contain lambda")
        }

      case Expression.CPSReset(exp, tpe, eff, loc) => ??? //todo sjj

      case Expression.SwitchError(tpe, eff, loc) => mkApplyCont(kont0, exp0, eff, loc)
    }

  /**
    * Convert a List of Expressions to CPS.
    *
    * E.g. f(e1,e2) becomes visitExp(e1, x => visitExp(e2, y => k(f(x, y))))
    * @param exps list of Expression to convert to CPS
    * @param kont0ReturnType the type of the base continuation
    * @param f function to specialize the final Expression (i.e. the Expression of the caller)
    * @return Expression with the list converted to CPS
    */
  private def visitExps(exps: List[Expression], kont0ReturnType: Type, f: List[Expression] => Expression, defSymMap: Map[Symbol.DefnSym, Symbol.DefnSym])(implicit genSym: GenSym): Expression = {
    // TODO SJJ: Handle CPure expressions
    val loc = SourceLocation.Generated

    // create new symbols for the lambdas
    val freshSyms = exps map { exp =>
      val freshSym = Symbol.freshVarSym()
      (exp, isCPure(exp), freshSym)
    }

    // in the base case, we apply the continuation with f("parameters")
    val baseCase: Expression = f(freshSyms.map(x => if (!x._2) Expression.Var(x._3, fixArrowType(x._1.tpe, kont0ReturnType), empEff(), loc) else x._1))

    // process all elements of the list
    freshSyms.foldRight(baseCase) {(syms, kont) =>
      val (exp, pure, freshSym) = syms
      if (!pure) {
        val freshSymVar = Expression.Var(freshSym, fixArrowType(exp.tpe, kont0ReturnType), empEff(), loc)
        val kont1 = mkLambda(freshSymVar.sym, freshSymVar.tpe, kont)
        visitExp(exp, kont1, kont0ReturnType, defSymMap)
      } else {
        kont
      }
    }
  }

  private def fixArrowType(tpe: Type, kont0ReturnType: Type): Type = {
    if (tpe.isArrow) {
      val kType = Type.mkArrow(fixArrowType(getReturnType(tpe), kont0ReturnType), kont0ReturnType)
      val lst: List[Type] = tpe.typeArguments.init :+ kType
      Type.mkUncurriedArrow(lst, kont0ReturnType)
    } else {
      tpe
    }
  }

  /**
    * Returns a lambda expression with the given symbol `sym` as a formal parameter,
    * the given type `argType` as its argument type and the given body `exp`.
    */
  private def mkLambda(sym: Symbol.VarSym, argType: Type, exp: Expression): Expression.Lambda = {
    val fparam = FormalParam(sym, Modifiers.Empty, argType, exp.loc)
    Expression.Lambda(fparam, exp, Type.mkArrow(argType, exp.tpe), empEff(), exp.loc)
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

  /**
    * Placeholder method for when control effects are supported.
    */
  private def isCPure(exp0: Expression): Boolean = exp0 match {
//    case Expression.Var(sym, tpe, eff, loc) => true

//    case Expression.Def(sym, tpe, eff, loc) => true

//    case Expression.Eff(sym, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Hole(sym, tpe, eff, loc) => ??? //todo sjj

    case Expression.Unit(loc) => true
    case Expression.True(loc) => true
    case Expression.False(loc) => true
    case Expression.Char(lit, loc) => true
    case Expression.Float32(lit, loc) => true
    case Expression.Float64(lit, loc) => true
    case Expression.Int8(lit, loc) => true
    case Expression.Int16(lit, loc) => true
    case Expression.Int32(lit, loc) => true
    case Expression.Int64(lit, loc) => true
    case Expression.BigInt(lit, loc) => true
    case Expression.Str(lit, loc) => true

//    case Expression.Lambda(fparam, exp, tpe, eff, loc) => isCPure(exp)
//
//    case Expression.LambdaWithKont(fparam1, fparam2, exp, tpe, eff, loc) => isCPure(exp)
//
//    case Expression.Apply(exp1, exp2, tpe, eff, loc) => isCPure(exp1) && isCPure(exp2)
//
//    case Expression.ApplyWithKont(exp1, exp2, exp3, tpe, eff, loc) => isCPure(exp1) && isCPure(exp2) && isCPure(exp3)
//
//    case Expression.Unary(op, exp, tpe, eff, loc) => isCPure(exp)
//
//    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) => isCPure(exp1) && isCPure(exp2)

//    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => isCPure(exp1) && isCPure(exp2)

//    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => ??? //todo sjj

//    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => isCPure(exp1) && isCPure(exp2) && isCPure(exp3)

//    case Expression.Match(exp, rules, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Switch(rules, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Tuple(elms, tpe, eff, loc) => elms.map(isCPure).forall(b => b)

//    case Expression.RecordEmpty(tpe, eff, loc) => ??? //todo sjj

//    case Expression.RecordSelect(base, label, tpe, eff, loc) => ??? //todo sjj

//    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => ??? //todo sjj

//    case Expression.RecordRestrict(label, rest, tpe, eff, loc) => ??? //todo sjj

//    case Expression.ArrayLit(elms, tpe, eff, loc) => ??? //todo sjj

//    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ??? //todo sjj

//    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ??? //todo sjj

//    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => ??? //todo sjj

//    case Expression.ArrayLength(base, tpe, eff, loc) => ??? //todo sjj

//    case Expression.ArraySlice(base, startIndex, endIndex, tpe, eff, loc) => ??? //todo sjj

//    case Expression.VectorLit(elms, tpe, eff, loc) => ??? //todo sjj

//    case Expression.VectorNew(elm, len, tpe, eff, loc) => ??? //todo sjj

//    case Expression.VectorLoad(base, index, tpe, eff, loc) => ??? //todo sjj

//    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ??? //todo sjj

//    case Expression.VectorLength(base, tpe, eff, loc) => ??? //todo sjj

//    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Ref(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Deref(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

//    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Existential(fparam, exp, eff, loc) => ??? //todo sjj

//    case Expression.Universal(fparam, exp, eff, loc) => ??? //todo sjj

//    case Expression.Ascribe(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Cast(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? //todo sjj

//    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => ??? //todo sjj

//    case Expression.NativeField(field, tpe, eff, loc) => ??? //todo sjj

//    case Expression.NativeMethod(method, args, tpe, eff, loc) => ??? //todo sjj

//    case Expression.NewChannel(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.GetChannel(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

//    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? //todo sjj

//    case Expression.Spawn(exp, tpe, eff, loc) => isCPure(exp)

//    case Expression.Sleep(exp, tpe, eff, loc) => isCPure(exp)

//    case Expression.FixpointConstraint(c0, tpe, eff, loc) => ??? //todo sjj

//    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

//    case Expression.FixpointSolve(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => ??? //todo sjj

//    case Expression.UserError(tpe, eff, loc) => ??? //todo sjj

//    case Expression.CPSShift(exp, tpe, eff, loc) => ??? //todo sjj

//    case Expression.CPSReset(exp, tpe, eff, loc) => ??? //todo sjj

    case _ => false
  }

}
