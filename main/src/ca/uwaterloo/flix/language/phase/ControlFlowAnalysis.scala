package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.phase.Stratifier.DependencyGraph
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object ControlFlowAnalysis {

  class Analysis(root: Root) {

    /**
      * A mutable map from functions symbols to their abstract argument values.
      */
    private val argValues: mutable.Map[Symbol.DefnSym, List[AbstractValue]] = mutable.Map.empty

    /**
      * A mutable map from function symbols to their abstract return values.
      */
    private val retValues: mutable.Map[Symbol.DefnSym, AbstractValue] = mutable.Map.empty

    /**
      * A mutable queue of pending function calls.
      */
    private val worklist: mutable.Queue[Symbol.DefnSym] = mutable.Queue.empty

    /**
      * Returns the abstract argument values of the function associated with the symbol `sym`.
      */
    def lookupArguments(sym: Symbol.DefnSym): List[AbstractValue] = argValues.getOrElse(sym, {
      root.defs(sym).formals.map(_ => AbstractValue.Bot)
    })

    /**
      * Returns the abstract return value of the function associated with the symbol `sym`.
      */
    def lookupReturn(sym: Symbol.DefnSym): AbstractValue =
      retValues.getOrElse(sym, AbstractValue.Bot)

    /**
      * Enqueues the function associated with the given symbol `sym` with the arguments `args` unless they are already subsumed.
      */
    def enqueue(sym: Symbol.DefnSym, newArgs: List[AbstractValue]): Unit = {
      // Lookup the arguments.
      val oldArgs = lookupArguments(sym)

      // Determine if they are subsumed.
      val unchanged = (newArgs zip oldArgs) forall {
        case (newArg, oldArg) => leq(newArg, oldArg)
      }

      if (!unchanged) {
        // Compute the least upper bound.
        val lubArgs = (newArgs zip oldArgs) map {
          case (newArg, oldArg) => lub(newArg, oldArg)
        }

        // Update the map and enqueue.
        argValues += sym -> lubArgs
        worklist += sym
      }
    }

    /**
      * Computes the fixpoint.
      */
    def fixpoint(): Unit = {

      while (worklist.nonEmpty) {
        val sym = worklist.dequeue()

        println(s"dequeue:  $sym, ws size = ${worklist.size}")

        //evalExp()


      }

    }


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

  def fixpoint(root: Root): Unit = {

    // Init a new empty analysis object.
    val a = new Analysis(root)

    // Enqueue all single unit argument functions.
    for ((sym, defn) <- root.defs) {
      // Check if the definition takes a single unit argument.
      if (isSingleUnitArg(defn.formals)) {
        // Enqueue a call to the function.
        a.enqueue(sym, List(AbstractValue.AnyPrimitive))
      }
    }

    // Compute the fixpoint.
    a.fixpoint()


    // TODO: Repeatedly reanalyze a function if it calls a function that changes.
    // This essentially requires a call graph.
    // What about closures?

    // TODO: Return type...
  }


  private def evalDef(def0: Def): DependencyGraph = {
    ???
  }

  // TODO: This really has to be done in a fixed point...
  // TODO: It seems unlike that this can simplt return an abstract value. It probably will have to return an abstract state...

  private def evalExp(exp0: Expression, env0: AbstractEnvironment, l: Analysis): AbstractValue = {

    def visitExp(exp0: Expression, env0: AbstractEnvironment, l: Analysis): AbstractValue = exp0 match {
      case Expression.Unit => AbstractValue.AnyPrimitive
      case Expression.True => AbstractValue.AnyPrimitive
      case Expression.False => AbstractValue.AnyPrimitive
      case Expression.Char(lit) => AbstractValue.AnyPrimitive
      case Expression.Float32(lit) => AbstractValue.AnyPrimitive
      case Expression.Float64(lit) => AbstractValue.AnyPrimitive
      case Expression.Int8(lit) => AbstractValue.AnyPrimitive
      case Expression.Int16(lit) => AbstractValue.AnyPrimitive
      case Expression.Int32(lit) => AbstractValue.AnyPrimitive
      case Expression.Int64(lit) => AbstractValue.AnyPrimitive
      case Expression.BigInt(lit) => AbstractValue.AnyPrimitive
      case Expression.Str(lit) => AbstractValue.AnyPrimitive

      case Expression.Var(sym, tpe, loc) =>
        env0.lookup(sym)

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
        ???

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val clo = visitExp(exp, env0, l)
        AbstractValue.Bot

      case Expression.ApplyDef(sym, args, tpe, loc) => invokeDef(sym, args)

      case Expression.ApplyEff(sym, args, tpe, loc) =>
        // TODO: Effects
        AbstractValue.Bot

      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        ???

      case Expression.ApplyDefTail(sym, args, tpe, loc) => invokeDef(sym, args)

      case Expression.ApplyEffTail(sym, args, tpe, loc) =>
        ???

      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => invokeDef(sym, actuals)

      case Expression.Unary(op, exp, tpe, eff, loc) =>
        // TODO
        AbstractValue.AnyPrimitive

      case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        ???

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        ???

      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        ???

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Evaluate the conditional.
        val v1 = visitExp(exp1, env0, l)

        // Evaluate the then and else branches.
        val v2 = visitExp(exp2, env0, l)
        val v3 = visitExp(exp3, env0, l)

        // Join the two branches.
        lub(v2, v3)

      case Expression.Branch(exp, branches, tpe, loc) =>
        ???

      case Expression.JumpTo(sym, tpe, loc) =>
        ???

      case Expression.Is(sym, tag, exp, loc) =>
        ???

      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val v = visitExp(exp, env0, l)
        AbstractValue.AnyTag(v)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        ???

      case Expression.Index(base, offset, tpe, loc) =>
        ???

      case Expression.Tuple(elms, tpe, loc) =>
        val vs = elms.map(visitExp(_, env0, l))
        AbstractValue.Tuple(vs)

      case Expression.RecordEmpty(tpe, loc) => ???

      case Expression.RecordSelect(base, label, tpe, loc) => ???

      case Expression.RecordExtend(base, label, value, tpe, loc) => ???

      case Expression.RecordRestrict(base, label, tpe, loc) => ???

      case Expression.ArrayLit(elms, tpe, loc) =>
        val vs = elms.map(visitExp(_, env0, l))
        val v = lubAll(vs)
        AbstractValue.Array(v)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        // TODO: Need allocation site?
        val v = visitExp(elm, env0, l)
        AbstractValue.Array(v)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        // Evaluate the base and index expressions. The index is abstracted away.
        (visitExp(base, env0, l), visitExp(index, env0, l)) match {
          case (AbstractValue.Bot, _) => AbstractValue.Bot
          case (AbstractValue.Array(v), _) => v
          case (v, _) => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
        }

      case Expression.ArrayLength(base, tpe, loc) =>
        // Evaluate and ignore the base expression.
        val v = visitExp(base, env0, l)
        AbstractValue.AnyPrimitive

      case Expression.ArrayStore(base, index, elm, tpe, loc) => ???

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???

      case Expression.Ref(exp, tpe, loc) => ???

      case Expression.Deref(exp, tpe, loc) => ???

      case Expression.Assign(exp1, exp2, tpe, loc) => ???

      case Expression.HandleWith(exp, bindings, tpe, loc) => ???

      case Expression.Existential(fparam, exp, loc) => ???

      case Expression.Universal(fparam, exp, loc) => ???

      case Expression.NativeConstructor(constructor, args, eff, loc) =>
        // Evaluate the arguments.
        val as = args.map(visitExp(_, env0, l))

        // Unsoundly ignore the constructed object.
        AbstractValue.Bot

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        // Unsoundly ignore exp.

        // Evaluate each catch rule.
        val values = rules map {
          case CatchRule(sym, clazz, body) => visitExp(body, env0, l)
        }

        // Join all the values.
        lubAll(values)

      case Expression.NativeField(field, tpe, loc) =>
        // Unsoundly ignore the field value.
        AbstractValue.Bot

      case Expression.NativeMethod(method, args, tpe, loc) =>
        // Evaluate the arguments.
        val as = args.map(visitExp(_, env0, l))

        // Unsoundly ignore the return value.
        AbstractValue.Bot

      case Expression.NewRelation(sym, tpe, loc) => AbstractValue.AnyRelation

      case Expression.NewLattice(sym, tpe, loc) => AbstractValue.AnyLattice

      case Expression.Constraint(con, tpe, loc) =>
        val g = Stratifier.getDependencyGraph(con)
        AbstractValue.Graph(g)

      case Expression.ConstraintUnion(exp1, exp2, tpe, loc) =>
        val v1 = visitExp(exp1, env0, l)
        val v2 = visitExp(exp2, env0, l)
        lub(v1, v2)

      case Expression.FixpointSolve(exp, stf, tpe, loc) =>
        visitExp(exp, env0, l)

      case Expression.FixpointCheck(exp, stf, tpe, loc) =>
        visitExp(exp, env0, l)

      case Expression.FixpointDelta(exp, stf, tpe, loc) =>
        visitExp(exp, env0, l)

      case Expression.UserError(tpe, loc) => AbstractValue.Bot

      case Expression.HoleError(sym, tpe, loc) => AbstractValue.Bot

      case Expression.MatchError(tpe, loc) => AbstractValue.Bot

      case Expression.SwitchError(tpe, loc) => AbstractValue.Bot

    }

    /**
      * Abstractly invokes the function associated with the symbol `sym` with the given arguments `args`.
      */
    def invokeDef(sym: Symbol.DefnSym, args: List[Expression]): AbstractValue = {
      // Evaluate the arguments.
      val as = args.map(a => visitExp(a, env0, l))

      // Enqueue the call with its arguments.
      l.enqueue(sym, as)

      // Lookup the current abstract value of the result.
      l.lookupReturn(sym)
    }

    visitExp(exp0, env0, l)
  }

  /**
    * Returns (an over-approximation) of the dependency graph of the given expression `exp`.
    */
  def getDependencyGraph(exp: Expression): DependencyGraph = {
    // Computes the fixed point.
    val v = evalExp(exp, AbstractEnvironment(Map.empty), new Analysis(null))

    v match {
      case AbstractValue.Bot => DependencyGraph.Empty
      case AbstractValue.Graph(g) => g
      case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
    }
  }

  /**
    * A common super-type for abstract values.
    */
  sealed trait AbstractValue

  object AbstractValue {

    // TODO: names and Comments

    /**
      * Represents the bottom element.
      */
    case object Bot extends AbstractValue

    /**
      * Represents any Unit, True, False, Char, Float32, Float64, Int8, Int16, Int32, Int64, BigInt, or Str value.
      */
    case object AnyPrimitive extends AbstractValue

    // TODO: case class Box() extends AbstractValue

    // TODO: case class Closure() extends AbstractValue

    /**
      * Represents any tagged value where the tag is abstracted away.
      */
    case class AnyTag(v: AbstractValue) extends AbstractValue

    /**
      * Approximation of arrays. Abstracts indices.
      */
    case class Array(vs: AbstractValue) extends AbstractValue

    /**
      * Approximation of tuples. Maintains indices.
      */
    case class Tuple(vs: List[AbstractValue]) extends AbstractValue

    // TODO: RecordEmpty/RecordExt.

    case class Graph(g: DependencyGraph) extends AbstractValue

    /**
      * Approximation of any relation value.
      */
    case object AnyRelation extends AbstractValue

    /**
      * Approximation of any lattice value.
      */
    case object AnyLattice extends AbstractValue

  }

  /**
    * Returns `true` if the abstract value `x` is less than or equal to the abstract value `y`.
    */
  private def leq(x: AbstractValue, y: AbstractValue): Boolean = (x, y) match {
    case (AbstractValue.Bot, _) => true
    case (AbstractValue.AnyPrimitive, AbstractValue.AnyPrimitive) => true
    case (AbstractValue.AnyTag(v1), AbstractValue.AnyTag(v2)) => leq(v1, v2)
    case (AbstractValue.Array(v1), AbstractValue.Array(v2)) => leq(v1, v2)
    case (AbstractValue.Tuple(vs1), AbstractValue.Tuple(vs2)) => (vs1 zip vs2) forall {
      case (v1, v2) => leq(v1, v2)
    }
    case (AbstractValue.Graph(g1), AbstractValue.Graph(g2)) => g1.xs subsetOf g2.xs
    case (AbstractValue.AnyRelation, AbstractValue.AnyRelation) => true
    case (AbstractValue.AnyLattice, AbstractValue.AnyLattice) => true
    case _ => false
  }

  /**
    * Returns the least upper bound of the two abstract values `v1` and `v2`.
    */
  private def lub(x: AbstractValue, y: AbstractValue): AbstractValue = (x, y) match {
    case (AbstractValue.Bot, _) => y
    case (_, AbstractValue.Bot) => x
    case (AbstractValue.AnyPrimitive, AbstractValue.AnyPrimitive) => AbstractValue.AnyPrimitive
    case (AbstractValue.AnyTag(v1), AbstractValue.AnyTag(v2)) => AbstractValue.AnyTag(lub(v1, v2))
    case (AbstractValue.Tuple(vs1), AbstractValue.Tuple(vs2)) =>
      val vs = (vs1 zip vs2).map {
        case (v1, v2) => lub(v1, v2)
      }
      AbstractValue.Tuple(vs)
    case (AbstractValue.Graph(g1), AbstractValue.Graph(g2)) =>
      val g = DependencyGraph(g1.xs ++ g2.xs)
      AbstractValue.Graph(g)
    case (AbstractValue.AnyRelation, AbstractValue.AnyRelation) => AbstractValue.AnyRelation
    case (AbstractValue.AnyLattice, AbstractValue.AnyLattice) => AbstractValue.AnyLattice
    case _ => throw InternalCompilerException(s"Unexpected abstract values: '$x' and '$y'. Possible type error?")
  }

  /**
    * Returns the least upper bound of the given sequence of abstract values `vs`.
    */
  private def lubAll(vs: List[AbstractValue]): AbstractValue = vs.foldLeft(AbstractValue.Bot: AbstractValue)(lub)

  /**
    * Returns `true` if the given argument list `xs` has a single argument of type unit.
    */
  private def isSingleUnitArg(xs: List[FormalParam]): Boolean = xs match {
    case FormalParam(sym, tpe) :: Nil => tpe == Type.Unit
    case _ => false
  }

}
