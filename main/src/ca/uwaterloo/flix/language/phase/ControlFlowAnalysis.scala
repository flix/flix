package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.FinalAst.{Def, Expression, Root}
import ca.uwaterloo.flix.language.phase.Stratifier.DependencyGraph
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
    case Expression.Unit => AbstractValue.AnyVal
    case Expression.True => AbstractValue.AnyVal
    case Expression.False => AbstractValue.AnyVal
    case Expression.Char(lit) => AbstractValue.AnyVal
    case Expression.Float32(lit) => AbstractValue.AnyVal
    case Expression.Float64(lit) => AbstractValue.AnyVal
    case Expression.Int8(lit) => AbstractValue.AnyVal
    case Expression.Int16(lit) => AbstractValue.AnyVal
    case Expression.Int32(lit) => AbstractValue.AnyVal
    case Expression.Int64(lit) => AbstractValue.AnyVal
    case Expression.BigInt(lit) => AbstractValue.AnyVal
    case Expression.Str(lit) => AbstractValue.AnyVal

    case Expression.Var(sym, tpe, loc) =>
      env0.lookup(sym)


    case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
      ???

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val clo = fixpointExp(exp, env0, l)
      AbstractValue.Bot

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      // TODO
      AbstractValue.Bot

    case Expression.ApplyEff(sym, args, tpe, loc) =>
      ???

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      ???

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      ???

    case Expression.ApplyEffTail(sym, args, tpe, loc) =>
      ???

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      ???

    case Expression.Unary(op, exp, tpe, eff, loc) =>
      AbstractValue.AnyVal

    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
      ???

    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      ???

    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      ???

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      // Compute the conditional (and ignore it).
      val v1 = fixpointExp(exp1, env0, l)

      // Compute the two branches.
      val v2 = fixpointExp(exp2, env0, l)
      val v3 = fixpointExp(exp3, env0, l)

      // The result is the join of the two branches.
      lub(v2, v3)


    case Expression.Tag(sym, tag, exp, tpe, loc) =>
      val v = fixpointExp(exp, env0, l)
      AbstractValue.AnyTag(v)

    case Expression.Tuple(elms, tpe, loc) =>
      val vs = elms.map(fixpointExp(_, env0, l))
      AbstractValue.Tuple(vs)

    case Expression.RecordEmpty(tpe, loc) => ???

    case Expression.RecordSelect(base, label, tpe, loc) => ???

    case Expression.RecordExtend(base, label, value, tpe, loc) => ???

    case Expression.RecordRestrict(base, label, tpe, loc) => ???

    case Expression.ArrayLit(elms, tpe, loc) =>
      val vs = elms.map(fixpointExp(_, env0, l))
      val v = lubAll(vs)
      AbstractValue.Array(v)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      // TODO: Need allocation site?
      val v = fixpointExp(elm, env0, l)
      AbstractValue.Array(v)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      // Evaluate the base and index expressions. The index is abstracted away.
      (fixpointExp(base, env0, l), fixpointExp(index, env0, l)) match {
        case (AbstractValue.Bot, _) => AbstractValue.Bot
        case (AbstractValue.Array(v), _) => v
        case (v, _) => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
      }

    case Expression.ArrayLength(base, tpe, loc) =>
      // Evaluate and ignore the base expression.
      val v = fixpointExp(base, env0, l)
      AbstractValue.AnyVal

    case Expression.ArrayStore(base, index, elm, tpe, loc) => ???

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???

    case Expression.Ref(exp, tpe, loc) => ???

    case Expression.Deref(exp, tpe, loc) => ???

    case Expression.Assign(exp1, exp2, tpe, loc) => ???

    case Expression.HandleWith(exp, bindings, tpe, loc) => ???

    case Expression.Existential(fparam, exp, loc) => ???

    case Expression.Universal(fparam, exp, loc) => ???

    case Expression.NativeConstructor(constructor, args, eff, loc) => ???

    case Expression.TryCatch(exp, rules, tpe, loc) => ???

    case Expression.NativeField(field, tpe, loc) => ???

    case Expression.NativeMethod(method, args, tpe, loc) => ???

    case Expression.NewRelation(sym, tpe, loc) => ???

    case Expression.NewLattice(sym, tpe, loc) => ???

    case Expression.Constraint(con, tpe, loc) =>
      val g = Stratifier.getDependencyGraph(con)
      AbstractValue.Graph(g)

    case Expression.ConstraintUnion(exp1, exp2, tpe, loc) =>
      val v1 = fixpointExp(exp1, env0, l)
      val v2 = fixpointExp(exp2, env0, l)
      lub(v1, v2)

    case Expression.FixpointSolve(exp, stf, tpe, loc) => ???

    case Expression.FixpointCheck(exp, stf, tpe, loc) => ???

    case Expression.FixpointDelta(exp, stf, tpe, loc) => ???

    case Expression.UserError(tpe, loc) => AbstractValue.Bot

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
