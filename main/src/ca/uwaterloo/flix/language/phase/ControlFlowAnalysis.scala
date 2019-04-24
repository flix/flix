package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.Ast.{DependencyEdge, DependencyGraph, Polarity}
import ca.uwaterloo.flix.language.ast.{Ast, MonoType, Symbol}
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object ControlFlowAnalysis {

  class Analysis(val root: Root) {

    /**
      * A mutable map from expression identifiers to dependency graphs.
      */
    private val dependencyGraphs: mutable.Map[Ast.UId, DependencyGraph] = mutable.Map.empty

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
      * Returns the over-approximated dependency graph of the given expression `uid.`
      */
    def getDependencyGraph(uid: Ast.UId): DependencyGraph = dependencyGraphs.get(uid) match {
      case None => DependencyGraph.Empty
      case Some(g) => g
    }

    /**
      * Updates the given expression `uid` to be associated with the given dependency graph `g`.
      */
    def updateDependencyGraph(uid: Ast.UId, g: DependencyGraph): Unit = {
      dependencyGraphs.put(uid, g)
    }

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

        // println(s"Enqueued: $sym") TODO
      }
    }

    /**
      * Computes the fixpoint.
      */
    def fixpoint(): Unit = {
      // Iterate until fixpoint.
      while (worklist.nonEmpty) {
        // Dequeue a function.
        val sym = worklist.dequeue()

        // Lookup the definition.
        val defn = root.defs(sym)

        // Retrieve the current abstract arguments.
        val args = lookupArguments(sym)

        // Construct an environment mapping the parameters to their values.
        val env = (defn.formals zip args).foldLeft(Map.empty[Symbol.VarSym, AbstractValue]) {
          case (acc, (FormalParam(s, _), value)) => acc + (s -> value)
        }

        // println(s"dequeue:  $sym, $env, ws size = ${worklist.size}") TODO

        // Evaluate the function body.
        evalExp(defn.exp, env, this)
      }
    }

  }

  /**
    * Runs and returns the result of the analysis.
    */
  def runAnalysis(root: Root): Analysis = {
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

    // Returns the result.
    a
  }


  /**
    * Evaluates the given expression `exp0` under the given environment `outerEnv0` with the associated analysis object `l`.
    */
  private def evalExp(exp0: Expression, outerEnv0: Map[Symbol.VarSym, AbstractValue], l: Analysis): AbstractValue = {

    def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, AbstractValue], lenv0: Map[Symbol.LabelSym, Expression]): AbstractValue = exp0 match {
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

      case Expression.Var(sym, tpe, loc) => env0.get(sym) match {
        case None => throw InternalCompilerException(s"Variable not found: '$sym'.")
        case Some(v) => v
      }

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
        val env = freeVars map {
          case FreeVar(s, _) => env0.get(s) match {
            case None => throw InternalCompilerException(s"Variable not found: '$sym'.")
            case Some(v) => v
          }
        }
        AbstractValue.ClosureSet(Map(sym -> env))

      case Expression.ApplyClo(exp, args, tpe, loc) => invokeClo(exp, args, env0, lenv0)

      case Expression.ApplyDef(sym, args, tpe, loc) => invokeDef(sym, args, env0)

      case Expression.ApplyEff(sym, args, tpe, loc) => AbstractValue.Bot // TODO: ApplyEff

      case Expression.ApplyCloTail(exp, args, tpe, loc) => invokeClo(exp, args, env0, lenv0)

      case Expression.ApplyDefTail(sym, args, tpe, loc) => invokeDef(sym, args, env0)

      case Expression.ApplyEffTail(sym, args, tpe, loc) => AbstractValue.Bot // TODO: ApplyEffTail

      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => invokeDef(sym, actuals, env0)

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val v = visitExp(exp, env0, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val v1 = visitExp(exp1, env0, lenv0)
        val v2 = visitExp(exp2, env0, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val v1 = visitExp(exp1, env0, lenv0)
        val env1 = env0 + (sym -> v1)
        visitExp(exp2, env1, lenv0)

      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => AbstractValue.Bot // TODO: LetRec

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Evaluate the conditional.
        val v1 = visitExp(exp1, env0, lenv0)

        // Evaluate the then and else branches.
        val v2 = visitExp(exp2, env0, lenv0)
        val v3 = visitExp(exp3, env0, lenv0)

        // Join the two branches.
        lub(v2, v3)

      case Expression.Branch(exp, branches, tpe, loc) =>
        // Evaluate the branching expression.
        val v = visitExp(exp, env0, branches)

        // Evaluate each of the branches.
        val vs = branches map {
          case (label, body) => visitExp(body, env0, branches)
        }

        // Join all the values.
        lub(v, lubAll(vs))

      case Expression.JumpTo(sym, tpe, loc) =>
        visitExp(lenv0(sym), env0, lenv0)

      case Expression.Is(sym, tag, exp, loc) =>
        val v = visitExp(exp, env0, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val v = visitExp(exp, env0, lenv0)
        AbstractValue.TagSet(Map(tag -> v))

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val v = visitExp(exp, env0, lenv0)
        v match {
          case AbstractValue.Bot => AbstractValue.Bot
          case AbstractValue.TagSet(m) => m.getOrElse(tag, AbstractValue.Bot)
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
        }

      case Expression.Index(base, offset, tpe, loc) =>
        val v = visitExp(base, env0, lenv0)
        v match {
          case AbstractValue.Bot => AbstractValue.Bot
          case AbstractValue.Tuple(vs) => vs(offset)
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
        }

      case Expression.Tuple(elms, tpe, loc) =>
        val vs = elms.map(visitExp(_, env0, lenv0))
        AbstractValue.Tuple(vs)

      case Expression.RecordEmpty(tpe, loc) => AbstractValue.Record(Map.empty)

      case Expression.RecordSelect(exp, label, tpe, loc) =>
        val v = visitExp(exp, env0, lenv0)
        v match {
          case AbstractValue.Bot => AbstractValue.Bot
          case AbstractValue.Record(m) =>
            m.getOrElse((label, tpe), AbstractValue.Bot)
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
        }

      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val newValue = visitExp(value, env0, lenv0)
        val recordValue = visitExp(rest, env0, lenv0)
        recordValue match {
          case AbstractValue.Bot => AbstractValue.Bot
          case AbstractValue.Record(m) =>
            val oldValue = m.getOrElse((label, tpe), AbstractValue.Bot)
            val lubValue = lub(newValue, oldValue)
            AbstractValue.Record(m + ((label, tpe) -> lubValue))
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$recordValue'.")
        }

      case Expression.RecordRestrict(label, rest, tpe, loc) =>
        visitExp(rest, env0, lenv0)

      case Expression.ArrayLit(elms, tpe, loc) =>
        val vs = elms.map(visitExp(_, env0, lenv0))
        val v = lubAll(vs)
        AbstractValue.Array(v)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        // TODO: Since arrays are multiple this is not stricly speaking correct.
        val v = visitExp(elm, env0, lenv0)
        AbstractValue.Array(v)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val v1 = visitExp(base, env0, lenv0)
        val v2 = visitExp(index, env0, lenv0)
        v1 match {
          case AbstractValue.Bot => AbstractValue.Bot
          case AbstractValue.Array(v) => v
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v1'.")
        }

      case Expression.ArrayLength(base, tpe, loc) =>
        val v = visitExp(base, env0, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val v1 = visitExp(base, env0, lenv0)
        val v2 = visitExp(index, env0, lenv0)
        val v3 = visitExp(elm, env0, lenv0)
        v1

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val v1 = visitExp(base, env0, lenv0)
        val v2 = visitExp(beginIndex, env0, lenv0)
        val v3 = visitExp(endIndex, env0, lenv0)
        v1 match {
          case AbstractValue.Bot => AbstractValue.Bot
          case AbstractValue.Array(v) => v
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v1'.")
        }

      case Expression.Ref(exp, tpe, loc) => AbstractValue.Bot // TODO: Ref

      case Expression.Deref(exp, tpe, loc) => AbstractValue.Bot // TODO: Deref

      case Expression.Assign(exp1, exp2, tpe, loc) => AbstractValue.Bot // TODO: Assign

      case Expression.HandleWith(exp, bindings, tpe, loc) => AbstractValue.Bot // TODO: HandleWith

      case Expression.Existential(fparam, exp, loc) =>
        // NB: We are simply ignoring the value of the parameter and assigning it bottom.
        val env1 = env0 + (fparam.sym -> AbstractValue.Bot)
        val v = visitExp(exp, env1, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.Universal(fparam, exp, loc) =>
        // NB: We are simply ignoring the value of the parameter and assigning it bottom.
        val env1 = env0 + (fparam.sym -> AbstractValue.Bot)
        val v = visitExp(exp, env1, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.NativeConstructor(constructor, args, eff, loc) =>
        // Evaluate the arguments.
        val as = args.map(visitExp(_, env0, lenv0))

        // Unsoundly ignore the constructed object.
        AbstractValue.Bot

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        // Unsoundly ignore exp.

        // Evaluate each catch rule.
        val values = rules map {
          case CatchRule(sym, clazz, body) => visitExp(body, env0, lenv0)
        }

        // Join all the values.
        lubAll(values)

      case Expression.NativeField(field, tpe, loc) =>
        // Unsoundly ignore the field value.
        AbstractValue.Bot

      case Expression.NativeMethod(method, args, tpe, loc) =>
        // Evaluate the arguments.
        val as = args.map(visitExp(_, env0, lenv0))

        // Unsoundly ignore the return value.
        AbstractValue.Bot

      //TODO: Add support for Channels to ControlFlowAnalysis.
      case Expression.NewChannel(tpe, exp, loc) => AbstractValue.Bot

      case Expression.GetChannel(exp, tpe, loc) => AbstractValue.Bot

      case Expression.PutChannel(exp1, exp2, tpe, loc) => AbstractValue.Bot

      case Expression.SelectChannel(rules, default, tpe, loc) => AbstractValue.Bot

      case Expression.ProcessSpawn(exp, tpe, loc) => AbstractValue.Bot

      case Expression.ProcessSleep(exp, tpe, loc) => AbstractValue.Bot

      case Expression.ProcessPanic(msg, tpe, loc) => AbstractValue.Bot

      case Expression.FixpointConstraint(con, tpe, loc) =>
        val g = visitConstraint(con)
        AbstractValue.Graph(g)

      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val v1 = visitExp(exp1, env0, lenv0)
        val v2 = visitExp(exp2, env0, lenv0)
        lub(v1, v2)

      case Expression.FixpointSolve(uid, exp, stf, tpe, loc) =>
        val v = visitExp(exp, env0, lenv0)
        v match {
          case AbstractValue.Bot =>
          case AbstractValue.Graph(g) =>
            l.updateDependencyGraph(uid, g)
          case _ => throw InternalCompilerException(s"Unexpected abstract value: '$v'.")
        }
        AbstractValue.Graph(DependencyGraph.Empty)

      case Expression.FixpointProject(pred, exp, tpe, loc) =>
        val p = visitPredicateWithParam(pred, env0, lenv0)
        val v = visitExp(exp, env0, lenv0)
        AbstractValue.Graph(DependencyGraph.Empty)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val v1 = visitExp(exp1, env0, lenv0)
        val v2 = visitExp(exp2, env0, lenv0)
        AbstractValue.AnyPrimitive

      case Expression.HoleError(sym, tpe, loc) => AbstractValue.Bot

      case Expression.MatchError(tpe, loc) => AbstractValue.Bot

      case Expression.SwitchError(tpe, loc) => AbstractValue.Bot

    }

    /**
      * Abstractly evaluates the given parameter with predicate `p0`.
      */
    def visitPredicateWithParam(p0: PredicateWithParam, env0: Map[Symbol.VarSym, AbstractValue], lenv0: Map[Symbol.LabelSym, Expression]): AbstractValue = p0 match {
      case PredicateWithParam(sym, exp) => visitExp(exp, env0, lenv0)
    }

    /**
      * Abstractly invokes the closure `exp` with the given arguments arguments `args`.
      */
    def invokeClo(exp: Expression, args: List[Expression], env0: Map[Symbol.VarSym, AbstractValue], lenv0: Map[Symbol.LabelSym, Expression]): AbstractValue = {
      val clo = visitExp(exp, env0, lenv0)
      val as = args.map(a => visitExp(a, env0, lenv0))

      clo match {
        case AbstractValue.Bot => AbstractValue.Bot

        case AbstractValue.ClosureSet(m) =>
          val vs = m map {
            case (sym, bindings) => invokeSingleClo(sym, bindings, as)
          }
          lubAll(vs)

        case _ => throw InternalCompilerException(s"Unexpected non-closure value: '$clo'.")
      }
    }

    /**
      * Abstractly invokes the closure with the given symbol `sym` with the bindings `bindings` and actual arguments `args`.
      */
    def invokeSingleClo(sym: Symbol.DefnSym, bindings: List[AbstractValue], args: List[AbstractValue]): AbstractValue = {
      // Lookup the definition.
      val defn = l.root.defs(sym)

      // Bindings for the capture variables are passed as arguments.
      val env1 = defn.formals.take(bindings.length).zip(bindings).foldLeft(Map.empty[Symbol.VarSym, AbstractValue]) {
        case (macc, (formal, actual)) => macc + (formal.sym -> actual)
      }
      // Now pass the actual arguments supplied by the caller.
      val env2 = defn.formals.drop(bindings.length).zip(args).foldLeft(env1) {
        case (macc, (formal, actual)) => macc + (formal.sym -> actual)
      }
      evalExp(defn.exp, env2, l)
    }

    /**
      * Abstractly invokes the function associated with the symbol `sym` with the given arguments `args`.
      */
    def invokeDef(sym: Symbol.DefnSym, args: List[Expression], env0: Map[Symbol.VarSym, AbstractValue]): AbstractValue = {
      // Evaluate the arguments.
      val as = args.map(a => visitExp(a, env0, Map.empty))

      // Enqueue the call with its arguments.
      l.enqueue(sym, as)

      // Lookup the current abstract value of the result.
      l.lookupReturn(sym)
    }

    visitExp(exp0, outerEnv0, Map.empty)
  }

  /**
    * Returns the dependency graph of the given constraint.
    */
  private def visitConstraint(c: Constraint): DependencyGraph = c match {
    case Constraint(cparams, head, body) =>
      // Determine if the head predicate has a symbol.
      visitHeadPredicateSymbol(head) match {
        case None => DependencyGraph.Empty
        case Some(headSym) =>
          val dependencies = body flatMap (b => visitDependencyEdge(headSym, b))
          DependencyGraph(dependencies.toSet)
      }
  }

  /**
    * Optionally returns the predicate symbol of the given head predicate `head0`.
    */
  private def visitHeadPredicateSymbol(head0: Predicate.Head): Option[Symbol.PredSym] = head0 match {
    case Predicate.Head.Atom(pred, terms, tpe, loc) => Some(pred.sym)
  }

  /**
    * Optionally returns the predicate symbol of the given body predicate `body0`.
    */
  private def visitDependencyEdge(head: Symbol.PredSym, body0: Predicate.Body): Option[DependencyEdge] = body0 match {
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) => polarity match {
      case Polarity.Positive => Some(DependencyEdge.Positive(head, pred.sym))
      case Polarity.Negative => Some(DependencyEdge.Negative(head, pred.sym))
    }

    case Predicate.Body.Filter(sym, terms, loc) => None

    case Predicate.Body.Functional(varSym, defSym, term, loc) => None
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
    case object AnyPrimitive extends AbstractValue

    /**
      * Approximation of any relation value.
      */
    case object AnyRelation extends AbstractValue

    /**
      * Approximation of any lattice value.
      */
    case object AnyLattice extends AbstractValue

    /**
      * Approximation of arrays. Abstracts indices.
      */
    case class Array(vs: AbstractValue) extends AbstractValue

    /**
      * Approximation of tuples. Maintains indices.
      */
    case class Tuple(vs: List[AbstractValue]) extends AbstractValue

    /**
      * Approximation of tuples. Maintains labels and types..
      */
    case class Record(m: Map[(String, MonoType), AbstractValue]) extends AbstractValue

    /**
      * Represents a collection of tags where each tag is mapped to an abstract value.
      */
    case class TagSet(m: Map[String, AbstractValue]) extends AbstractValue

    /**
      * Represents a closure where each def symbol is mapped to its environment arguments.
      */
    case class ClosureSet(m: Map[Symbol.DefnSym, List[AbstractValue]]) extends AbstractValue

    /**
      * Represents an over-approximation of the dependecy graph of a constraint system.
      */
    case class Graph(g: DependencyGraph) extends AbstractValue

  }

  /**
    * Returns `true` if the abstract value `x` is less than or equal to the abstract value `y`.
    */
  private def leq(x: AbstractValue, y: AbstractValue): Boolean = (x, y) match {
    case (AbstractValue.Bot, _) => true

    case (AbstractValue.AnyPrimitive, AbstractValue.AnyPrimitive) => true

    case (AbstractValue.AnyRelation, AbstractValue.AnyRelation) => true

    case (AbstractValue.AnyLattice, AbstractValue.AnyLattice) => true

    case (AbstractValue.Array(v1), AbstractValue.Array(v2)) => leq(v1, v2)

    case (AbstractValue.Tuple(vs1), AbstractValue.Tuple(vs2)) => (vs1 zip vs2) forall {
      case (v1, v2) => leq(v1, v2)
    }

    case (AbstractValue.Record(m1), AbstractValue.Record(m2)) =>
      m1.forall {
        case ((label, tpe), v1) =>
          val v2 = m2.getOrElse((label, tpe), AbstractValue.Bot)
          leq(v1, v2)
      }

    case (AbstractValue.TagSet(m1), AbstractValue.TagSet(m2)) => m1 forall {
      case (tag, v) => leq(v, m2.getOrElse(tag, AbstractValue.Bot))
    }

    case (AbstractValue.ClosureSet(m1), AbstractValue.ClosureSet(m2)) =>
      m1.forall {
        case (k, vs1) =>
          val vs2 = m2.getOrElse(k, Nil)
          (vs1 zip vs2) forall {
            case (v1, v2) => leq(v1, v2)
          }
      }

    case (AbstractValue.Graph(g1), AbstractValue.Graph(g2)) => g1.xs subsetOf g2.xs

    case _ => false
  }

  /**
    * Returns the least upper bound of the two abstract values `v1` and `v2`.
    */
  private def lub(x: AbstractValue, y: AbstractValue): AbstractValue = (x, y) match {
    case (AbstractValue.Bot, _) => y

    case (_, AbstractValue.Bot) => x

    case (AbstractValue.AnyPrimitive, AbstractValue.AnyPrimitive) => AbstractValue.AnyPrimitive

    case (AbstractValue.AnyRelation, AbstractValue.AnyRelation) => AbstractValue.AnyRelation

    case (AbstractValue.AnyLattice, AbstractValue.AnyLattice) => AbstractValue.AnyLattice

    case (AbstractValue.Array(v1), AbstractValue.Array(v2)) =>
      AbstractValue.Array(lub(v1, v2))

    case (AbstractValue.Tuple(vs1), AbstractValue.Tuple(vs2)) =>
      val vs = (vs1 zip vs2).map {
        case (v1, v2) => lub(v1, v2)
      }
      AbstractValue.Tuple(vs)

    case (AbstractValue.Record(m1), AbstractValue.Record(m2)) =>
      val m3 = (m1.keySet ++ m2.keySet) map {
        case (label, tpe) =>
          val v1 = m1.getOrElse((label, tpe), AbstractValue.Bot)
          val v2 = m2.getOrElse((label, tpe), AbstractValue.Bot)
          (label, tpe) -> lub(v1, v2)
      }
      AbstractValue.Record(m3.toMap)

    case (AbstractValue.TagSet(m1), AbstractValue.TagSet(m2)) =>
      val m3 = (m1.keySet ++ m2.keySet) map {
        case k =>
          val v1 = m1.getOrElse(k, AbstractValue.Bot)
          val v2 = m2.getOrElse(k, AbstractValue.Bot)
          k -> lub(v1, v2)
      }
      AbstractValue.TagSet(m3.toMap)

    case (AbstractValue.ClosureSet(m1), AbstractValue.ClosureSet(m2)) =>
      val m3 = (m1.keySet ++ m2.keySet) map {
        case k =>
          val env1 = m1.getOrElse(k, Nil)
          val env2 = m1.getOrElse(k, Nil)
          val vs = (env1 zip env2) map {
            case (v1, v2) => lub(v1, v2)
          }
          k -> vs
      }
      AbstractValue.ClosureSet(m3.toMap)

    case (AbstractValue.Graph(g1), AbstractValue.Graph(g2)) =>
      val g = DependencyGraph(g1.xs ++ g2.xs)
      AbstractValue.Graph(g)

    case _ =>
      throw InternalCompilerException(s"Unexpected incompatible abstract values: '$x' and '$y'.")
  }

  /**
    * Returns the least upper bound of the given sequence of abstract values `vs`.
    */
  private def lubAll(vs: Traversable[AbstractValue]): AbstractValue = vs.foldLeft(AbstractValue.Bot: AbstractValue)(lub)

  /**
    * Returns `true` if the given argument list `xs` has a single argument of type unit.
    */
  private def isSingleUnitArg(xs: List[FormalParam]): Boolean = xs match {
    case FormalParam(sym, tpe) :: Nil => tpe == MonoType.Unit
    case _ => false
  }

}
