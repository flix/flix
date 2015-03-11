package impl.lmsbackend

import Declarations._
import scala.collection.mutable

class Solver(rules: List[Rule]) {
  sealed abstract class Binding
  case class LeqBinding(v: Value) extends Binding
  case class ExactBinding(v: Value) extends Binding

  class RuleEval(val rule: Rule) {
    /*
    val locals = (rule.head :: rule.body).flatMap(collectLocalVars)
    def collectLocalVars(pred: Predicate): Set[LocalVar] = pred match {
      case pred: GlobalLeqPred => collectLocalVars(pred.pattern)
      case pred: LocalLeqPred => collectLocalVars(pred.pattern) + pred.variable
      case pred: FnCallPred => collectLocalVars(pred.argument) + pred.result
    }
    */
    case class State(val valuation: Map[LocalVar, Binding], val processors: List[State=>Unit]) {
      def accept(newValuation: Map[LocalVar, Binding]): Unit = {
        val newState = State(newValuation, processors.tail)
        processors.head(newState)
      }
      def accept(): Unit = accept(valuation)
      def liftedValuation: PartialFunction[LocalVar, Value] = {
        case l if valuation.isDefinedAt(l) => valuation(l) match {
          case LeqBinding(v) => v
          case ExactBinding(v) => v
        }
      }
    }

    def accept(pred: Predicate)(state: State): Unit = pred match {
      case GlobalLeqPred(pattern, variable) => acceptLeq(pattern)(globalEnv(variable))(state)
      case LocalLeqPred(pattern, variable) => state.valuation.get(variable) match {
        case None => error("variable ${variable.name} not bound")
        case Some(LeqBinding(bound)) => acceptLeq(pattern)(bound)(state)
        case Some(ExactBinding(bound)) => acceptLeq(pattern)(bound)(state)
      }
      case _: FnCallPred => ???
    }

    def acceptLeq(pattern: Pattern)(bound: Value)(state: State): Unit = {
      assert(pattern.tpe == bound.tpe)
      pattern match {
        case Constant(v) => if(v.leq(bound)) state.accept()
        case Variable(l) =>
          state.valuation.get(l) match {
            case None =>
              state.accept(state.valuation + (l -> LeqBinding(bound)))
            case Some(LeqBinding(existing)) =>
              state.accept(state.valuation + (l -> LeqBinding(bound.meet(existing))))
            case Some(ExactBinding(existing)) =>
              if(existing.leq(bound)) state.accept()
          }
        case TuplePattern(ps) =>
          val elems = bound.asInstanceOf[TupleValue].elems
          val newProcessors: List[State=>Unit] = (ps zip elems).map{ case (p, e) => acceptLeq(p)(e)(_) }
          state.copy(processors = newProcessors ++ state.processors).accept()
        case MapElem(keyPattern, valuePattern) =>
          val mapBound = bound.asInstanceOf[MapValue].map
          val keyVal = keyPattern.eval(state.liftedValuation)
          def processKey(key: Value) = {
            mapBound.get(key) match {
              case Some(value) =>
                val newProcessors: List[State=>Unit] =
                  (acceptExact(keyPattern)(key)(_)) ::
                    (acceptLeq(valuePattern)(value)(_)) ::
                    state.processors
                state.copy(processors = newProcessors).accept()
              case None =>
            }
          }
          keyVal match {
            case Some(key) => processKey(key)
            case None =>  mapBound.keys.foreach(processKey(_))
          }
      }
    }

    def acceptExact(pattern: Pattern)(other: Value)(state: State): Unit = {
      assert(pattern.tpe == other.tpe)
      pattern match {
        case Constant(v) => if(v == other) state.accept()
        case Variable(l) =>
          state.valuation.get(l) match {
            case None =>
              state.accept(state.valuation + (l -> ExactBinding(other)))
            case Some(LeqBinding(existing)) =>
              if(other.leq(existing)) state.accept(state.valuation + (l -> ExactBinding(other)))
            case Some(ExactBinding(existing)) =>
              if(existing == other) state.accept()
          }
        case TuplePattern(ps) =>
          val elems = other.asInstanceOf[TupleValue].elems
          val newProcessors: List[State=>Unit] = (ps zip elems).map{ case (p, e) => acceptExact(p)(e)(_) }
          state.copy(processors = newProcessors ++ state.processors).accept()
        case MapElem(keyPattern, valuePattern) =>
          val mapBound = other.asInstanceOf[MapValue].map
          if(mapBound.size == 1) mapBound.keys.foreach{key =>
            val newProcessors: List[State=>Unit] =
              (acceptExact(keyPattern)(key)(_)) ::
                (acceptExact(valuePattern)(mapBound(key))(_)) ::
                state.processors
            state.copy(processors = newProcessors).accept()
          }
      }
    }

    def apply(): Boolean = {
      var outputValue: Value = globalEnv.getOrElse(rule.head.variable, rule.head.variable.tpe.bot)
      var changed = false

      def updateOutputValue(state: State): Unit = {
        rule.head.pattern.eval(state.liftedValuation) match {
          case None => error(s"rule fails to bind all variables used in head ${rule}")
          case Some(newValue) =>
            if(!newValue.leq(outputValue)) {
              changed = true
              outputValue = outputValue.join(newValue)
            }
        }
      }

      val predAccepts: List[State=>Unit] = rule.body.map(pred => accept(pred)(_))
      val outputAccept: State=>Unit = updateOutputValue(_)
      State(Map(), predAccepts :+ outputAccept).accept()

      if(changed) globalEnv(rule.head.variable) = outputValue
      changed
    }
  }

  val globalEnv = mutable.Map[GlobalVar, Value]().withDefault(_.tpe.bot)

  def globalsInBody(rule: Rule) = rule.body.collect{case GlobalLeqPred(_, global) => global}

  val dependences: GlobalVar=>Traversable[Rule] = {
    val pairs: Seq[(GlobalVar, Rule)] = for {
      rule <- rules
      global <- globalsInBody(rule)
    } yield (global, rule)

    pairs.groupBy(_._1).mapValues(_.map(_._2)).withDefaultValue(List.empty)
  }

  def apply(): Unit = {
    val worklist = mutable.Set() ++ rules
    val ruleEvals = rules.map{
      rule => (rule, new RuleEval(rule))
    }.toMap
    while(!worklist.isEmpty) {
      val rule = worklist.head
      worklist -= rule
      if(ruleEvals(rule).apply()) {
        worklist ++= dependences(rule.head.variable)
      }
    }
  }

  def print(): Unit = globalEnv.keys.foreach{key =>
    println(key.show)
    println(globalEnv(key).show)
    println("=" * 60)
  }
}

