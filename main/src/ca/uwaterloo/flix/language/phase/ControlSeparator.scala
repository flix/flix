package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.Expr
import ca.uwaterloo.flix.language.ast.{Ast, CallByValueAst, ReducedAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.collection.MapOps

/**
  * This phase separates code that can have control effects from code that cannot by limiting them
  * to the righthand side of [[CallByValueAst.Stmt.LetVal]]. This means that while evaluating `a+b`
  * we know that `a` and `b` cannot have unhandled `do`s.
  *
  * This is achieved by let-binding `a` and `b` if they have escaping `do`'s.
  *
  * {{{
  *   (do RandomInt(12)) + (do RandomInt(42))
  * }}}
  * becomes
  * {{{
  *   letval x = do RandomInt(12);
  *   letval y = do RandomInt(42);
  *   ret (x + y)
  * }}}
  * This whole thing is a Statement (it does contain unhandled `do`s) but the addition itself is an
  * expression. The letval block ends with a statement so `ret` is used to lift the expression.
  */
object ControlSeparator {

  def run(root: ReducedAst.Root)(implicit flix: Flix): CallByValueAst.Root = flix.phase("ControlSeparator") {
    val ReducedAst.Root(defs0, enums0, anonClasses0, entryPoint, sources) = root
    val defs = MapOps.mapValues(defs0)(visitDef)
    val enums = MapOps.mapValues(enums0)(visitEnum)
    val anonClasses = anonClasses0.map(visitAnonClass(_)(new Context(), implicitly)) // TODO WHAT TO DO ABOUT CONTEXT?
    CallByValueAst.Root(defs, enums, anonClasses, entryPoint, sources)
  }

  def visitDef(defn: ReducedAst.Def)(implicit flix: Flix): CallByValueAst.Def = {
    val ReducedAst.Def(ann, mod, sym, cparams0, fparams0, ReducedAst.Stmt.Ret(exp, _, _), tpe, purity, loc) = defn // TODO PURITY
    val cparams = cparams0.map(visitFormalParam)
    val fparams = fparams0.map(visitFormalParam)
    // important! reify bindings
    implicit val ctx: Context = new Context()
    val stmt = insertBindings(_ => visitExpAsStmt(exp))
    CallByValueAst.Def(ann, mod, sym, cparams, fparams, stmt, tpe, loc)
  }

  def visitEnum(e: ReducedAst.Enum): CallByValueAst.Enum = {
    val ReducedAst.Enum(ann, mod, sym, cases0, tpeDeprecated, loc) = e
    val cases = MapOps.mapValues(cases0)(visitEnumCase)
    CallByValueAst.Enum(ann, mod, sym, cases, tpeDeprecated, loc)
  }

  def visitEnumCase(c: ReducedAst.Case): CallByValueAst.Case = {
    val ReducedAst.Case(sym, tpeDeprecated, loc) = c
    CallByValueAst.Case(sym, tpeDeprecated, loc)
  }

  def visitAnonClass(c: ReducedAst.AnonClass)(implicit ctx: Context, flix: Flix): CallByValueAst.AnonClass = c match {
    case ReducedAst.AnonClass(name, clazz, tpe, methods0, loc) =>
      val methods = methods0.map(visitJvmMethodSpec)
      CallByValueAst.AnonClass(name, clazz, tpe, methods, loc)
  }

  // invariant: context will be unchanged
  def visitExpAsStmt(exp: ReducedAst.Expr)(implicit ctx: Context, flix: Flix): CallByValueAst.Stmt = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      insertBindings(_ => ret(CallByValueAst.Expr.Cst(cst, tpe, loc)))
    case Expr.Var(sym, tpe, loc) =>
      ret(CallByValueAst.Expr.Var(sym, tpe, loc))
    case Expr.ApplyAtomic(op, exps0, tpe, purity, loc) =>
      insertBindings { _ =>
        val exps = exps0.map(visitExpAsExpr)
        ret(CallByValueAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc))
      }
    case Expr.ApplyClo(exp, exps0, ct, tpe, purity, loc) =>
      insertBindings(_ => {
        val e = visitExpAsExpr(exp)
        val exps = exps0.map(visitExpAsExpr)
        CallByValueAst.Stmt.ApplyClo(e, exps, ct, tpe, purity, loc)
      })
    case Expr.ApplyDef(sym, exps0, ct, tpe, purity, loc) =>
      insertBindings(_ => {
        val exps = exps0.map(visitExpAsExpr)
        CallByValueAst.Stmt.ApplyDef(sym, exps, ct, tpe, purity, loc)
      })
    case Expr.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      insertBindings(_ => {
        val formals = formals0.map(visitFormalParam)
        val actuals = actuals0.map(visitExpAsExpr)
        CallByValueAst.Stmt.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
      })
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      insertBindings(_ => {
        val e1 = visitExpAsExpr(exp1)
        val e2 = visitExpAsStmt(exp2)
        val e3 = visitExpAsStmt(exp3)
        CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
      })
    case Expr.Branch(exp, branches0, tpe, purity, loc) =>
      val branches = MapOps.mapValues(branches0)(visitExpAsStmt)
      CallByValueAst.Stmt.Branch(visitExpAsStmt(exp), branches, tpe, purity, loc)
    case Expr.JumpTo(sym, tpe, purity, loc) =>
      CallByValueAst.Stmt.JumpTo(sym, tpe, purity, loc)
    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val stmt1 = visitExpAsStmt(exp1)
      val stmt2 = visitExpAsStmt(exp2)
      CallByValueAst.Stmt.LetVal(sym, stmt1, stmt2, tpe, purity, loc)
    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      insertBindings { _ =>
        val e1 = visitExpAsExpr(exp1)
        CallByValueAst.Stmt.LetRec(varSym, index, defSym, e1, visitExpAsStmt(exp2), tpe, purity, loc)
      }
    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      CallByValueAst.Stmt.Scope(sym, visitExpAsStmt(exp), tpe, purity, loc)
    case Expr.TryCatch(exp, rules0, tpe, purity, loc) =>
      val e = visitExpAsStmt(exp)
      val rules = rules0.map(visitCatchRule)
      ret(CallByValueAst.Expr.TryCatch(e, rules, tpe, purity, loc))
    case Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      insertBindings { _ =>
        val methods = methods0.map(visitJvmMethodImpl)
        ret(CallByValueAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc))
      }
  }

  private def visitCatchRule(rule: ReducedAst.CatchRule)(implicit ctx: Context, flix: Flix): CallByValueAst.CatchRule = rule match {
    case ReducedAst.CatchRule(sym, clazz, exp) =>
      CallByValueAst.CatchRule(sym, clazz, visitExpAsStmt(exp))
  }

  private def visitJvmMethodImpl(method: ReducedAst.JvmMethodImpl)(implicit ctx: Context, flix: Flix): CallByValueAst.JvmMethodImpl = method match {
    case ReducedAst.JvmMethodImpl(ident, fparams, clo0, retTpe, purity, loc) =>
      val clo = visitExpAsExpr(clo0)
      CallByValueAst.JvmMethodImpl(ident, fparams.map(visitFormalParam), clo, retTpe, purity, loc)
  }

  private def visitJvmMethodSpec(spec: ReducedAst.JvmMethodSpec)(implicit ctx: Context, flix: Flix): CallByValueAst.JvmMethodSpec = spec match {
    case ReducedAst.JvmMethodSpec(ident, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      CallByValueAst.JvmMethodSpec(ident, fparams, tpe, purity, loc)
  }

  sealed trait Binding

  object Binding {
    case class Val(sym: Symbol.VarSym, binding: CallByValueAst.Stmt) extends Binding
  }

  /**
    * Knows how to bind.
    */
  class Context() {

    type Stack[a] = List[a]

    private var l: Stack[Binding] = Nil

    def bind(stmt: CallByValueAst.Stmt)(implicit flix: Flix): CallByValueAst.Expr.Var = {
      val loc = SourceLocation.Unknown
      val sym = Symbol.freshVarSym("cbv", Ast.BoundBy.Let, loc)
      l = Binding.Val(sym, stmt) :: l
      CallByValueAst.Expr.Var(sym, stmt.tpe, stmt.loc)
    }

    def withBindings[R](f: Unit => R): (R, Stack[Binding]) = {
      // track fresh bindings
      val old = l
      l = Nil
      val res = f(())
      val bindings = l
      l = old
      (res, bindings)
    }

    def reifyBindings(stmt: CallByValueAst.Stmt, bindings: Stack[Binding]): CallByValueAst.Stmt = {
      bindings.foldLeft(stmt) {
        case (acc, Binding.Val(sym, binding)) =>
          CallByValueAst.Stmt.LetVal(sym, binding, acc, stmt.tpe, stmt.purity, binding.loc.asSynthetic)
      }
    }

  }

  def insertBindings(f: Unit => CallByValueAst.Stmt)(implicit ctx: Context): CallByValueAst.Stmt = {
    val (s, bindings) = ctx.withBindings(f)
    ctx.reifyBindings(s, bindings)
  }

  /**
    * Translates the lifted expression into a CBV expression.
    * This often entail let-binding.
    */
  def visitExpAsExpr(exp: ReducedAst.Expr)(implicit ctx: Context, flix: Flix): CallByValueAst.Expr = exp match {
    case ReducedAst.Expr.Cst(cst, tpe, loc) =>
      CallByValueAst.Expr.Cst(cst, tpe, loc)
    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      CallByValueAst.Expr.Var(sym, tpe, loc)
    case ReducedAst.Expr.ApplyAtomic(op, exps0, tpe, purity, loc) =>
      // TODO FIX: this might break evaluation order
      val exps = exps0.map(visitExpAsExpr)
      CallByValueAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc)
    case ReducedAst.Expr.ApplyClo(exp, exps0, ct, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val exps = exps0.map(visitExpAsExpr)
      val ac = CallByValueAst.Stmt.ApplyClo(e, exps, ct, tpe, purity, loc)
      ctx.bind(ac)
    case ReducedAst.Expr.ApplyDef(sym, exps0, ct, tpe, purity, loc) =>
      val exps = exps0.map(visitExpAsExpr)
      val ad = CallByValueAst.Stmt.ApplyDef(sym, exps, ct, tpe, purity, loc)
      ctx.bind(ad)
    case ReducedAst.Expr.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      val formals = formals0.map(visitFormalParam)
      val actuals = actuals0.map(visitExpAsExpr)
      val adt = CallByValueAst.Stmt.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
      ctx.bind(adt)
    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)
      val ite = CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
      ctx.bind(ite)
    case ReducedAst.Expr.Branch(exp, branches0, tpe, purity, loc) =>
      val branches = MapOps.mapValues(branches0)(visitExpAsStmt)
      ctx.bind(CallByValueAst.Stmt.Branch(visitExpAsStmt(exp), branches, tpe, purity, loc))
    case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      ctx.bind(CallByValueAst.Stmt.JumpTo(sym, tpe, purity, loc))
    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val stmt1 = visitExpAsStmt(exp1)
      val stmt2 = visitExpAsStmt(exp2)
      ctx.bind(CallByValueAst.Stmt.LetVal(sym, stmt1, stmt2, tpe, purity, loc))
    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      ctx.bind(CallByValueAst.Stmt.LetRec(varSym, index, defSym, visitExpAsExpr(exp1), visitExpAsStmt(exp2), tpe, purity, loc))
    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      ctx.bind(CallByValueAst.Stmt.Scope(sym, visitExpAsStmt(exp), tpe, purity, loc))
    case Expr.TryCatch(exp, rules0, tpe, purity, loc) =>
      val e = visitExpAsStmt(exp)
      val rules = rules0.map(visitCatchRule)
      CallByValueAst.Expr.TryCatch(e, rules, tpe, purity, loc)
    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      CallByValueAst.Expr.NewObject(name, clazz, tpe, purity, methods.map(visitJvmMethodImpl), loc)
  }

  private def visitFormalParam(p: ReducedAst.FormalParam): CallByValueAst.FormalParam = p match {
    case ReducedAst.FormalParam(sym, mod, tpe, loc) =>
      CallByValueAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * Wrap an expression in a return statement.
    */
  private def ret(e: CallByValueAst.Expr): CallByValueAst.Stmt.Ret = {
    CallByValueAst.Stmt.Ret(e, e.tpe, e.purity, e.loc)
  }

}
