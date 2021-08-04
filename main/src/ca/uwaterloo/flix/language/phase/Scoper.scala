package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Entity.TypeCon
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Scopedness, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.ScopeError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation.ToSuccess


// MATT license
// MATT docs
object Scoper extends Phase[Root, Root] {

  private val noScope = (Scopedness.Unscoped, ScopeScheme.Unit, Set.empty[Symbol.VarSym])

  override def run(input: Root)(implicit flix: Flix): Validation[Root, ScopeError] = {
    // analyze defs
    // analyze sigs
    // analyze instances
    ???
  }

  private def checkDef(defn: Def): Validation[Unit, ScopeError] = defn match {
    case Def(sym, spec, impl) => checkImpl(impl)
  }

  private def checkImpl(impl: Impl): Validation[Unit, ScopeError] = impl match {
    case Impl(exp, inferredScheme) => checkExp(exp, Map.empty).map(_ => ()) // MATT add fparams here (their ScopeScheme should be annotated)
  }

  private def checkExp(exp0: Expression, senv: Map[Symbol.VarSym, ScopeScheme]): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = exp0 match {
    case Expression.Unit(loc) => noScope.toSuccess
    case Expression.Null(tpe, loc) => noScope.toSuccess
    case Expression.True(loc) => noScope.toSuccess
    case Expression.False(loc) => noScope.toSuccess
    case Expression.Char(lit, loc) => noScope.toSuccess
    case Expression.Float32(lit, loc) => noScope.toSuccess
    case Expression.Float64(lit, loc) => noScope.toSuccess
    case Expression.Int8(lit, loc) => noScope.toSuccess
    case Expression.Int16(lit, loc) => noScope.toSuccess
    case Expression.Int32(lit, loc) => noScope.toSuccess
    case Expression.Int64(lit, loc) => noScope.toSuccess
    case Expression.BigInt(lit, loc) => noScope.toSuccess
    case Expression.Str(lit, loc) => noScope.toSuccess
    case Expression.Default(tpe, loc) => noScope.toSuccess
    case Expression.Wild(tpe, loc) => noScope.toSuccess
    case Expression.Var(sym, tpe, loc) =>
      sym.scopedness match {
        case Scopedness.Scoped => (sym.scopedness, senv(sym), Set(sym)).toSuccess
        case Scopedness.Unscoped => (sym.scopedness, senv(sym), Set.empty[Symbol.VarSym]).toSuccess
      }
      (sym.scopedness, senv(sym), Set(sym)).toSuccess
    case Expression.Def(sym, tpe, loc) => (Scopedness.Unscoped, ???, Set.empty[Symbol.VarSym]).toSuccess // MATT lookup from earlier phase
    case Expression.Sig(sym, tpe, loc) => (Scopedness.Unscoped, ???, Set.empty[Symbol.VarSym]).toSuccess // MATT lookup from earlier phase
    case Expression.Hole(sym, tpe, eff, loc) => (Scopedness.Unscoped, mkScopeScheme(tpe), Set.empty[Symbol.VarSym]).toSuccess
    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val fparamSch = mkScopeScheme(fparam.tpe)
      for {
        (bodySco, bodySch, bodyVars) <- checkExp(exp, senv + (fparam.sym -> fparamSch))
        // MATT assert body unscoped
        freeVars = bodyVars - fparam.sym
        sco = if (freeVars.isEmpty) Scopedness.Unscoped else Scopedness.Scoped
        sch = ScopeScheme.Arrow(fparam.sym.scopedness, fparamSch, bodySch)
      } yield (sco, sch, freeVars)
    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      for {
        (_, funcSch, funcVars) <- checkExp(exp, senv)
        args <- Validation.traverse(exps)(checkExp(_, senv))
        sch <- Validation.fold(args, funcSch) {
          case (ScopeScheme.Arrow(paramSco, paramSch, rest), (argSco, argSch, _)) =>
            if (!(argSco <= paramSco)) {
              ??? // error bad arg
            } else if (!(argSch <= paramSch)) {
              ??? // error bad arg
            } else {
              rest.toSuccess
            }
        }
        argVars = args.flatMap(_._3)

      } yield (Scopedness.Unscoped, sch, funcVars ++ argVars)
    case Expression.Unary(sop, exp, tpe, eff, loc) => {
      for {
        (_, _, vars) <- checkExp(exp, senv)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    }
    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv)
        (_, _, vars2) <- checkExp(exp2, senv)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars1 ++ vars2)
    case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      for {
        (_, varSch, varVars) <- checkExp(exp1, senv)
        (sco, sch, vars) <- checkExp(exp2, senv + (sym -> varSch))
        freeVars = (varVars ++ vars) - sym
      } yield (sco, sch, freeVars)
    case Expression.LetRegion(sym, exp, tpe, eff, loc) => checkExp(exp, senv) // MATT right?
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv)
        (sco2, sch2, vars2) <- checkExp(exp2, senv)
        (sco3, sch3, vars3) <- checkExp(exp3, senv)
      } yield (sco2 max sco3, sch2 max sch3, vars1 ++ vars2 ++ vars3)
    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv)
        (sco, sch, vars2) <- checkExp(exp2, senv)
      } yield (sco, sch, vars1 ++ vars2)
    case Expression.Match(exp, rules, tpe, eff, loc) => ???
    case Expression.Choose(exps, rules, tpe, eff, loc) => ???
    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ???
    case Expression.Tuple(elms, tpe, eff, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, eff, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) => ???
    case Expression.ArrayLit(elms, tpe, eff, loc) => ???
    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ???
    case Expression.ArrayLength(base, eff, loc) => ???
    case Expression.ArrayStore(base, index, elm, loc) => ???
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???
    case Expression.Ref(exp, tpe, eff, loc) => ???
    case Expression.Deref(exp, tpe, eff, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ???
    case Expression.Existential(fparam, exp, loc) => ???
    case Expression.Universal(fparam, exp, loc) => ???
    case Expression.Ascribe(exp, tpe, eff, loc) => ???
    case Expression.Cast(exp, tpe, eff, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => ???
    case Expression.GetField(field, exp, tpe, eff, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => ???
    case Expression.GetStaticField(field, tpe, eff, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, eff, loc) => ???
    case Expression.NewChannel(exp, tpe, eff, loc) => ???
    case Expression.GetChannel(exp, tpe, eff, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ???
    case Expression.Spawn(exp, tpe, eff, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, eff, loc) => ???
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => ???
    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) => ???
    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => ???
    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) => ???
    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) => ???
    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) => ???
    case Expression.MatchEff(exp1, exp2, exp3, tpe, eff, loc) => ???
  }

  private def mkScopeScheme(tpe: Type): ScopeScheme = tpe.typeConstructor match {
    case Some(_: TypeConstructor.Arrow) =>
      val argSchemes = tpe.arrowArgTypes.map(mkScopeScheme)
      val retScheme = mkScopeScheme(tpe.arrowResultType)
      argSchemes.foldRight(retScheme) {
        case (arg, acc) => ScopeScheme.Arrow(Scopedness.Unscoped, arg, acc)
      }
  }

  private sealed trait ScopeScheme {
    def <=(other: ScopeScheme): Boolean = (this, other) match {
      case (ScopeScheme.Unit, ScopeScheme.Unit) => true
      case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
        // NB: Contravariant on the left
        paramSco2 <= paramSco1 && paramSch2 <= paramSch1 && retSch1 <= retSch2
      case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
    }

    def max(other: ScopeScheme): ScopeScheme = (this, other) match {
      case (ScopeScheme.Unit, ScopeScheme.Unit) => ScopeScheme.Unit
      case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
        // NB: Contravariant on the left
        ScopeScheme.Arrow(paramSco1 min paramSco2, paramSch1 min paramSch2, retSch1 max retSch2)
      case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
    }

    def min(other: ScopeScheme): ScopeScheme = (this, other) match {
      case (ScopeScheme.Unit, ScopeScheme.Unit) => ScopeScheme.Unit
      case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
        // NB: Contravariant on the left
        ScopeScheme.Arrow(paramSco1 max paramSco2, paramSch1 max paramSch2, retSch1 min retSch2)
      case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
    }

  }

  private object ScopeScheme {
    case object Unit extends ScopeScheme

    case class Arrow(paramSco: Scopedness, paramSch: ScopeScheme, retSch: ScopeScheme) extends ScopeScheme
  }

  private sealed trait Position {
    def of(other: Position): Position = (this, other) match {
      case (Position.Tail, Position.Tail) => Position.Tail
      case _ => Position.Nontail

    }
  }

  private object Position {
    case object Tail extends Position

    case object Nontail extends Position
  }
}
