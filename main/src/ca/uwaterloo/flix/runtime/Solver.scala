package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.TypedAst.Collection.Relation
import ca.uwaterloo.flix.language.ast.TypedAst.Constraint.Rule
import ca.uwaterloo.flix.language.ast.TypedAst.Term
import ca.uwaterloo.flix.language.ast.TypedAst.Term.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Name, TypedAst}
import ca.uwaterloo.flix.language.backend.phase.Indexer
import ca.uwaterloo.flix.runtime.datastore.{DataStore, IndexedLattice, IndexedRelation}
import ca.uwaterloo.flix.util.{Validation, AsciiTable}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solver {

  /**
   * A case class representing a solver context.
   */
  case class SolverContext(root: TypedAst.Root)

}

/**
 * A solver based on semi-naive evaluation.
 */
class Solver(implicit sCtx: Solver.SolverContext) {

  /**
   * A common super-type for solver errors.
   */
  sealed trait SolverError {
    /**
     * Returns a human readable string representation of the error.
     */
    def format: String
  }

  object SolverError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
     * An error raised to indicate that the asserted fact does not hold in the minimal model.
     *
     * @param loc the location of the asserted fact.
     */
    case class AssertedFactViolated(loc: SourceLocation) extends SolverError {
      val format =
        s"""${consoleCtx.blue(s"-- SOLVER ERROR -------------------------------------------------- ${loc.formatSource}")}
           |
            |${consoleCtx.red(s">> Assertion violated: The asserted fact does not hold in the minimal model!")}
           |
           |${loc.underline}
         """.stripMargin
    }


  }

  /**
   * The primary data store that holds all relations and lattices.
   */
  val dataStore = new DataStore()

  /**
   * The work list of pending predicate names and their associated values.
   */
  val worklist = mutable.Queue.empty[(Name.Resolved, List[Value])]


  /**
   * Solves the Flix program.
   */
  def solve(): Unit = {
    val t = System.nanoTime()

    // adds all facts to the database.
    for (fact <- sCtx.root.facts) {
      val name = fact.head.asInstanceOf[TypedAst.Predicate.Head.Relation].name // TODO: Cast
      val values = fact.head.asInstanceOf[TypedAst.Predicate.Head.Relation].terms map (term => Interpreter.evalHeadTerm(term, sCtx.root, Map.empty)) // TODO: Cast
      inferredFact(name, values)
    }

    // iterate until fixpoint.
    while (worklist.nonEmpty) {
      // extract fact from the worklist.
      val (name, row) = worklist.dequeue()

      // re-evaluate all dependencies.
      val rules = dependencies(name)
      for (rule <- rules) {
        // TODO: Use `row` as the initial environment to speedup computation.
        evalBody(rule, Map.empty)
      }
    }

    val elapsed = System.nanoTime() - t
    println(s"Successfully solved in ${elapsed / 1000000} msec.")

    directives()
  }

  /**
   * Processes an inferred `fact` for the relation or lattice with the `name`.
   */
  def inferredFact(name: Name.Resolved, fact: List[Value]): Unit = sCtx.root.collections(name) match {
    case r: TypedAst.Collection.Relation =>
      val changed = dataStore.relations(name).inferredFact(fact.toArray)
      if (changed) {
        worklist += ((r.name, fact))
      }

    case l: TypedAst.Collection.Lattice =>
      val changed = dataStore.lattices(name).inferredFact(fact.toArray)
      if (changed) {
        worklist += ((l.name, fact))
      }

  }

  /**
   * Evaluates the head of the given `rule` under the given environment `env0`.
   */
  def evalHead(rule: Rule, env0: Map[String, Value]): Unit = rule.head match {
    case p: Predicate.Head.Relation =>
      val row = p.terms map (t => Interpreter.evalHeadTerm(t, sCtx.root, env0))
      inferredFact(p.name, row)
    case p: Predicate.Head.Trace =>
      val row = p.terms map (t => pretty(Interpreter.evalHeadTerm(t, sCtx.root, env0)))
      val out = "Trace(" + row.mkString(", ") + ")"
      Console.println(out)
    case p: Predicate.Head.Write => // NOP - used when the fixpoint has been found.
    case p: Predicate.Head.Error => // NOP - used when the fixpoint has been found.
  }


  /**
   * Evaluates the body of the given `rule` under the given initial environment `env0`.
   */
  def evalBody(rule: Rule, env0: Map[String, Value]): Unit = {

    /**
     * Performs tuple-at-a-time propagation.
     */
    def recur(ps: List[TypedAst.Predicate.Body], row: mutable.Map[String, Value]): Unit = ps match {
      case Nil => evalHead(rule, row.toMap)
      case Predicate.Body.Relation(name, terms, _, _) :: xs =>
        val collection = sCtx.root.collections(name) match {
          case r: Collection.Relation => dataStore.relations(name)
          case l: Collection.Lattice => dataStore.lattices(name)
        }

        val values = terms.map(t => peval(t, row.toMap))
        val offset2var = terms.zipWithIndex.foldLeft(Map.empty[Int, String]) {
          case (macc, (Term.Body.Var(ident, _, _), i)) => macc + (i -> ident.name)
          case (macc, _) => macc
        }

        for (row2 <- collection.lookup(values.toArray)) {
          val newRow = row.clone()
          for (i <- row2.indices) {
            offset2var.get(i) match {
              case None => // nop
              case Some(x) =>
                newRow += (x -> row2(i))
            }
          }
          recur(xs, newRow)
        }


      case Predicate.Body.Function(name, terms, _, _) :: xs => ???
      case Predicate.Body.NotEqual(ident1, ident2, _, _) :: xs =>
        val value1 = row(ident1.name)
        val value2 = row(ident2.name)
        if (value1 == value2) {
          recur(xs, row)
        }
      case Predicate.Body.Read(_, _, _, _) :: xs => ???
    }

    //  TODO: Sort the body predicates...
    recur(rule.body, mutable.Map.empty ++ env0)
  }

  /**
   * Evaluates the given body term `t` to a value.
   *
   * Returns `null` if the term is a free variable.
   */
  def peval(t: TypedAst.Term.Body, env: Map[String, Value]): Value = t match {
    case t: TypedAst.Term.Body.Wildcard => null
    case t: TypedAst.Term.Body.Var =>
      if (env contains t.ident.name)
        env(t.ident.name)
      else
        null
    case t: TypedAst.Term.Body.Lit => Interpreter.evalLit(t.lit)
  }

  /**
   * Returns all rules where the given `name` occurs in a body predicate of the rule.
   */
  def dependencies(name: Name.Resolved): List[TypedAst.Constraint.Rule] = sCtx.root.rules.filter {
    case rule => rule.body.exists {
      case r: Predicate.Body.Relation => name == r.name
      case _ => false
    }
  }

  /**
   * Processes all directives in the program.
   */
  def directives(): Unit = {
    for (directive <- sCtx.root.directives.prints) {
      print(directive)
    }

    val assertedFacts = @@(sCtx.root.directives.assertedFacts map checkAssertedFact)
    if (assertedFacts.hasErrors) {
      assertedFacts.errors.foreach(e => println(e.format))
    }

    for (directive <- sCtx.root.directives.assertedRules) {
      checkAssertedRule(directive)
    }
  }

  /**
   * Evaluates the given print `directive`.
   */
  def print(directive: Directive.Print): Unit = {
    val collection = sCtx.root.collections(directive.name)

    collection match {
      case r: TypedAst.Collection.Relation =>
        val table = dataStore.relations(directive.name).table
        val cols = r.attributes.map(_.ident.name)
        val ascii = new AsciiTable().withCols(cols: _*)
        for (row <- table.toSeq.sortBy(_.head.toString)) {
          ascii.mkRow(row.toList map pretty)
        }

        Console.println(r.name)
        ascii.write(System.out)
        Console.println()
        Console.println()

      case l: TypedAst.Collection.Lattice =>
        val table = dataStore.lattices(directive.name).table

        val cols = l.keys.map(_.ident.name) ::: l.values.map(_.ident.name + "<>")
        val ascii = new AsciiTable().withCols(cols: _*)
        for (row <- table.toSeq.sortBy(_.head.toString)) {
          ascii.mkRow(row.toList map pretty)
        }

        Console.println(l.name)
        ascii.write(System.out)
        Console.println()
        Console.println()
    }

  }

  // TODO: Move somewhere. Decide where
  def pretty(v: Value): String = v match {
    case Value.Unit => "()"
    case Value.Bool(b) => b.toString
    case Value.Int(i) => i.toString
    case Value.Str(s) => s.toString
    case Value.Tag(enum, tag, value) => enum + "." + tag + pretty(value)
    case Value.Tuple(elms) => "(" + (elms map pretty) + ")"
    case Value.Closure(_, _, _) => ??? // TODO: WHAT?
  }

  /**
   * Verifies that the given asserted fact `d` holds in the minimal model.
   */
  def checkAssertedFact(d: Directive.AssertFact): Validation[Boolean, SolverError] = {
    // TODO
    false.toSuccess
  }

  /**
   * Verifies that the given asserted rule `d` holds in the minimal model.
   */
  def checkAssertedRule(d: Directive.AssertRule): Validation[Boolean, SolverError] = {
    // TODO
    false.toSuccess
  }


}
