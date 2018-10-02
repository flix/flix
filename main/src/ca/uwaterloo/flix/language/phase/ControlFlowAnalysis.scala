package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expression, Root}
import ca.uwaterloo.flix.language.phase.Stratifier.{DependencyGraph, union}
import ca.uwaterloo.flix.util.InternalCompilerException

object ControlFlowAnalysis {

  // TODO: Do we want to perform this analysis after monomorphization? Probably? This allows us to avoid dealing with polymorphic functions...

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

  /**
    * A common super-type for abstract values.
    */
  sealed trait AbstractValue

  object AbstractValue {

    /**
      * Represents the bottom element.
      */
    case object Bot extends AbstractValue

    /**
      * Represents any Unit, True, False, Char, Float32, Float64, Int8, Int16, Int32, Int64, BigInt, or Str value.
      */
    case object AnyVal extends AbstractValue

    // TODO: case class Box() extends AbstractValue

    // TODO: case class Closure() extends AbstractValue

    /**
      * Represents any tagged value where the tag is abstracted away.
      */
    case class AnyTag(v: AbstractValue) extends AbstractValue

    /**
      * Represents any array value where the array indices are abstracted away.
      */
    case class Array(vs: AbstractValue) extends AbstractValue

    /**
      * Represents any vector value where the vector indices are abstracted away.
      */
    case class Vector(vs: AbstractValue) extends AbstractValue

    /**
      * Represents any tuple value where the tuple indices are maintained.
      */
    case class Tuple(vs: List[AbstractValue]) extends AbstractValue

    // TODO: RecordEmpty/RecordExt.

    case class Graph(d: DependencyGraph) extends AbstractValue

    // TODO: Relation/Lattice value.

    // TODO: Constraint Value

  }

  /**
    * Returns `true` if the abstract value `x` is less than or equal to the abstract value `y`.
    */
  def leq(x: AbstractValue, y: AbstractValue): Boolean = (x, y) match {
    case (AbstractValue.Bot, _) => true
    case (AbstractValue.AnyVal, AbstractValue.AnyVal) => true
    case (AbstractValue.AnyTag(v1), AbstractValue.AnyTag(v2)) => leq(v1, v2)
    // TODO: Rest
    case _ => false
  }

  /**
    * Returns the least upper bound of the two abstract values `v1` and `v2`.
    */
  def lub(x: AbstractValue, y: AbstractValue): AbstractValue = (x, y) match {
    case (AbstractValue.Bot, _) => y
    case (_, AbstractValue.Bot) => x
    case (AbstractValue.Graph(g1), AbstractValue.Graph(g2)) => AbstractValue.Graph(Stratifier.union(g1, g2))
    case _ => ???
  }

  /**
    * Returns the least upper bound of the given sequence of abstract values `vs`.
    */
  def lubAll(vs: List[AbstractValue]): AbstractValue = vs.foldLeft(AbstractValue.Bot: AbstractValue)(lub)

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

    case Expression.Wild(tpe, eff, loc) => ???

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
      // Compute the conditional (and ignore it).
      val v1 = fixpointExp(exp1, env0, l)

      // Compute the two branches.
      val v2 = fixpointExp(exp2, env0, l)
      val v3 = fixpointExp(exp3, env0, l)

      // The result is the join of the two branches.
      lub(v2, v3)

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      // TODO: Deal with the match value.
      ???

    case Expression.Switch(rules, tpe, eff, loc) =>
      ???

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      val v = fixpointExp(exp, env0, l)
      AbstractValue.AnyTag(v)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      val vs = elms.map(fixpointExp(_, env0, l))
      AbstractValue.Tuple(vs)

    case Expression.RecordEmpty(tpe, eff, loc) => ???

    case Expression.RecordSelect(base, label, tpe, eff, loc) => ???

    case Expression.RecordExtend(base, label, value, tpe, eff, loc) => ???

    case Expression.RecordRestrict(base, label, tpe, eff, loc) => ???

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      val vs = elms.map(fixpointExp(_, env0, l))
      val v = lubAll(vs)
      AbstractValue.Array(v)

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      // TODO: Need allocation site?
      val v = fixpointExp(elm, env0, l)
      AbstractValue.Array(v)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      // Evaluate the base and index expressions. The index is abstracted away.
      (fixpointExp(base, env0, l), fixpointExp(index, env0, l)) match {
        case (AbstractValue.Bot, _) => AbstractValue.Bot
        case (AbstractValue.Array(v), _) => v
        case (v, _) => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
      }

    case Expression.ArrayLength(base, tpe, eff, loc) =>
      // Evaluate and ignore the base expression.
      val v = fixpointExp(base, env0, l)
      AbstractValue.AnyVal

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
      lub(v1, v2)

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
