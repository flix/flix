package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expression, Root}
import ca.uwaterloo.flix.language.phase.Stratifier.{DependencyGraph, union}
import ca.uwaterloo.flix.util.InternalCompilerException

object ControlFlowAnalysis {

  case class AbstractLattice(m: Map[Symbol.DefnSym, AbstractValue]) {
    def lookup(sym: Symbol.DefnSym): AbstractValue =
      m.getOrElse(sym, AbstractValue.Bot)
  }

  case class AbstractEnvironment(m: Map[Symbol.VarSym, AbstractValue]) {
    def lookup(sym: Symbol.VarSym): AbstractValue = m.get(sym) match {
      case None =>
        // TODO: XXX: Would be better to set formals etc. to bottom than just assume bottom...
        AbstractValue.Bot
      //throw InternalCompilerException(s"Unknown value of $sym.")
      case Some(v) => v
    }
  }

  sealed trait AbstractValue

  object AbstractValue {

    case object AnyVal extends AbstractValue

    case class Graph(d: DependencyGraph) extends AbstractValue

    case class Tag(v: AbstractValue) extends AbstractValue

    case class Array(vs: AbstractValue) extends AbstractValue

    case class Vector(vs: AbstractValue) extends AbstractValue

    case class Tuple(vs: AbstractValue) extends AbstractValue

    case object Bot extends AbstractValue

  }

  def join(e1: AbstractValue, e2: AbstractValue): AbstractValue = (e1, e2) match {
    case (AbstractValue.Bot, _) => e2
    case (_, AbstractValue.Bot) => e1
    case (AbstractValue.Graph(g1), AbstractValue.Graph(g2)) => AbstractValue.Graph(Stratifier.union(g1, g2))
    case _ => ???
  }

  def joinAll(vs: List[AbstractValue]): AbstractValue = vs.foldLeft(AbstractValue.Bot: AbstractValue)(join)

  private def fixpoint(root: Root): DependencyGraph = {

    // TODO: Repeatedly reanalyze a function if it calls a function that changes.
    // This essentially requires a call graph.
    // What about closures?

    ???
  }

  private def fixpointDef(def0: Def): DependencyGraph = {
    ???
  }

  // TODO: This really has to be done in a fixed point...
  // TODO: It seems unlike that this can simplt return an abstract value. It probably will have to return an abstract state...
  private def fixpointExp(exp0: Expression, env0: AbstractEnvironment, l: AbstractLattice): AbstractValue = exp0 match {
    case Expression.Unit(loc) => AbstractValue.AnyVal
    case Expression.True(loc) => AbstractValue.AnyVal
    case Expression.False(loc) => AbstractValue.AnyVal
    case Expression.Char(lit, loc) => AbstractValue.AnyVal
    case Expression.Float32(lit, loc) => AbstractValue.AnyVal
    case Expression.Float64(lit, loc) => AbstractValue.AnyVal
    case Expression.Int8(lit, loc) => AbstractValue.AnyVal
    case Expression.Int16(lit, loc) => AbstractValue.AnyVal
    case Expression.Int32(lit, loc) => AbstractValue.AnyVal
    case Expression.Int64(lit, loc) => AbstractValue.AnyVal
    case Expression.BigInt(lit, loc) => AbstractValue.AnyVal
    case Expression.Str(lit, loc) => AbstractValue.AnyVal

    case Expression.Wild(tpe, eff, loc) => AbstractValue.Bot

    case Expression.Var(sym, tpe, eff, loc) =>
      env0.lookup(sym)

    case Expression.Def(sym, tpe, eff, loc) =>
      l.lookup(sym)

    case Expression.Eff(sym, tpe, eff, loc) => ???

    case Expression.Hole(sym, tpe, eff, loc) => ???

    case Expression.Lambda(fparam, exp, tpe, eff, loc) =>
      // TODO...
      ???

    case Expression.Apply(exp1, exp2, tpe, eff, loc) =>
      val v1 = fixpointExp(exp1, env0, l)
      val v2 = fixpointExp(exp2, env0, l)
      v1 match {
        case AbstractValue.Bot => AbstractValue.Bot
        case _ => ???
      }

    case Expression.Unary(op, exp, tpe, eff, loc) =>
      AbstractValue.AnyVal

    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
      ???

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      ???

    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
      ???

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val v1 = fixpointExp(exp2, env0, l)
      val v2 = fixpointExp(exp3, env0, l)
      join(v1, v2)

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      // TODO: Deal with the match value.
      ???

    case Expression.Switch(rules, tpe, eff, loc) =>
      ???

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      ???

    case Expression.Tuple(elms, tpe, eff, loc) => ???

    case Expression.RecordEmpty(tpe, eff, loc) => ???

    case Expression.RecordExtension(base, label, fld, tpe, eff, loc) => ???

    case Expression.RecordProjection(base, label, tpe, eff, loc) => ???

    case Expression.RecordRestriction(base, label, tpe, eff, loc) => ???

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      val vs = elms.map(fixpointExp(_, env0, l))
      AbstractValue.Array(joinAll(vs))

    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ???

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      // TODO: This does not even use index...
      fixpointExp(base, env0, l) match {
        case AbstractValue.Bot => AbstractValue.Bot
        case AbstractValue.Array(v) => v
        case v => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
      }

    case Expression.ArrayLength(base, tpe, eff, loc) => ???

    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => ???

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => ???

    case Expression.VectorLit(elms, tpe, eff, loc) => ???

    case Expression.VectorNew(elm, len, tpe, eff, loc) => ???

    case Expression.VectorLoad(base, index, tpe, eff, loc) => ???

    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ???

    case Expression.VectorLength(base, tpe, eff, loc) => ???

    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ???

    case Expression.Ref(exp, tpe, eff, loc) => ???

    case Expression.Deref(exp, tpe, eff, loc) => ???

    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ???

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ???

    case Expression.Existential(fparam, exp, eff, loc) => ???

    case Expression.Universal(fparam, exp, eff, loc) => ???

    case Expression.Ascribe(exp, tpe, eff, loc) => ???

    case Expression.Cast(exp, tpe, eff, loc) => ???

    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => ???

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ???

    case Expression.NativeField(field, tpe, eff, loc) => ???

    case Expression.NativeMethod(method, args, tpe, eff, loc) => ???

    case Expression.NewRelation(sym, tpe, eff, loc) => ???

    case Expression.NewLattice(sym, tpe, eff, loc) => ???

    case Expression.Constraint(con, tpe, eff, loc) =>
      val g = Stratifier.getDependencyGraph(con)
      AbstractValue.Graph(g)

    case Expression.ConstraintUnion(exp1, exp2, tpe, eff, loc) =>
      val v1 = fixpointExp(exp1, env0, l)
      val v2 = fixpointExp(exp2, env0, l)
      join(v1, v2)

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => ???

    case Expression.FixpointCheck(exp, stf, tpe, eff, loc) => ???

    case Expression.FixpointDelta(exp, stf, tpe, eff, loc) => ???

    case Expression.UserError(tpe, eff, loc) => AbstractValue.Bot

  }


  // TODO: DOC
  def getDependencyGraphFromAbstractValue(exp: Expression): DependencyGraph = {
    // TODO
    val v = fixpointExp(exp, AbstractEnvironment(Map.empty), AbstractLattice(Map.empty))
    v match {
      case AbstractValue.Bot => DependencyGraph.Empty
      case AbstractValue.Graph(g) => g
      case _ => ???
    }
  }


}
