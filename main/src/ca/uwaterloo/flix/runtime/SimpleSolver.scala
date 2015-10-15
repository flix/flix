package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.TypedAst.Collection.Relation
import ca.uwaterloo.flix.language.ast.TypedAst.Constraint.Rule
import ca.uwaterloo.flix.language.ast.TypedAst.Term
import ca.uwaterloo.flix.language.ast.TypedAst.Term.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Name, TypedAst}
import ca.uwaterloo.flix.runtime.datastore.IndexedRelation
import ca.uwaterloo.flix.util.{Validation, AsciiTable}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SimpleSolver(implicit sCtx: Solver.SolverContext) extends Solver {

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
   *
   */
  val dbRel = mutable.Map.empty[Name.Resolved, List[List[Value]]]

  val dbLat = mutable.Map.empty[Name.Resolved, Map[List[Value], List[Value]]]

  class DataStore {

    /**
     *
     */
    val relations = mutable.Map.empty[Name.Resolved, IndexedRelation]

    /**
     * A map from names to sets of indexes attributes.
     *
     * For example, if the map contains "foo" -> Set(Seq(0), Seq(1, 2)) then the collection named "foo"
     * should have an index on its 0th attribute and its 1st and 2nd attributes together.
     */
    val indexes = mutable.Map.empty[Name.Resolved, Set[Seq[Int]]]

    /**
     * Computes indexes for all collections based on a left-to-right evaluation strategy of each rule.
     */
    def computeIndexes(): Unit = {
      // iterate through each rule.
      for (constraint <- sCtx.root.rules) {
        // maintain set of bound variables in each rule.
        val bound = mutable.Set.empty[String]
        // iterate through each collection predicate in the body.
        for (body <- constraint.body) {
          body match {
            case Predicate.Body.Relation(name, terms, _, _) =>
              // TODO: This has to be careful with lattices.

              // compute the indices of the determinate terms.
              val determinate = terms.zipWithIndex.foldLeft(Seq.empty[Int]) {
                case (xs, (t: Term.Body.Wildcard, i)) => xs
                case (xs, (t: Term.Body.Var, i)) =>
                  if (bound contains t.ident.name)
                    xs :+ i
                  else
                    xs
                case (xs, (t: Term.Body.Lit, i)) => xs :+ i
              }

              if (determinate.nonEmpty) {
                val idxs = indexes.getOrElse(name, Set.empty)
                indexes(name) = idxs + determinate
              }

              // update the set of bound variables.
              bound ++= body.freeVars
            case _ => // nop
          }
        }
      }
    }

    def init(): Unit = {
      for ((name, collection) <- sCtx.root.collections) {
        collection match {
          case r: Collection.Relation =>
            relations(name) = new IndexedRelation(r, indexes.getOrElse(name, Set.empty))

          case l: Collection.Lattice => // todo
        }
      }
    }


  }

  // TODO: Avoid indirection as much as possible.

  /**
   * The work list of pending predicate names and their associated values.
   */
  val worklist = mutable.Queue.empty[(Name.Resolved, List[Value])]

  val dataStore = new DataStore

  /**
   * Solves the Flix program.
   */
  def solve(): Unit = {
    val t = System.nanoTime()

    dataStore.computeIndexes()
    dataStore.init()
    println(dataStore.indexes)

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
      inferredRelationFact(r, fact.toArray)

      val changed = dataStore.relations(name).inferredFact(fact.toArray)
      if (changed) {
        worklist += ((r.name, fact))
      }

    case l: TypedAst.Collection.Lattice => inferredLatticeFact(l, fact.toArray)
  }

  /**
   * Processes a new fact `f` for the relation `r`.
   *
   * If the fact is new it is added to the worklist together with its dependencies.
   *
   * @param r the relation.
   * @param f the fact.
   */
  def inferredRelationFact(r: Collection.Relation, f: Array[Value]): Unit = {
    val row = f.toList
    val table = dbRel.getOrElse(r.name, List.empty)
    val rowExists = table.contains(row)
    if (!rowExists) {
      dbRel += (r.name -> (row :: table))
      worklist += ((r.name, row))
    }
  }

  /**
   * Processes a new fact `f` for the lattice `l`.
   *
   * If the fact is new it is added to the worklist together with its dependencies.
   *
   * @param l the lattice.
   * @param f the fact.
   */
  def inferredLatticeFact(l: Collection.Lattice, f: Array[Value]): Unit = {
    val defn = l
    val name = defn.name
    val row = f.toList

    val keys = l.keys
    val latAttr = l.values
    val map = dbLat.getOrElse(name, Map.empty)

    val (ks, vs) = row.splitAt(keys.size)

    map.get(ks) match {
      case None =>
        val result = map + (ks -> vs)
        dbLat += (name -> result)
        worklist += ((name, row))
      case Some(vs2) =>
        var changed = false

        val vs3 = (vs zip vs2 zip latAttr) map {
          case ((v1, v2), TypedAst.Attribute(_, tpe)) =>
            val lat = sCtx.root.lattices(tpe)
            val newLub = Interpreter.eval2(lat.lub, v1, v2, sCtx.root)
            val isSubsumed = Interpreter.eval2(lat.leq, newLub, v2, sCtx.root).toBool

            changed = changed || !isSubsumed
            newLub
        }

        if (changed) {
          val result = map + (ks -> vs3)
          dbLat += (name -> result)
          worklist += ((name, row)) // TODO: Row is incorrect here.
        }
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
        val relation = dataStore.relations(name)
        val values = terms.map(t => peval(t, row.toMap))
        val offset2var = terms.zipWithIndex.foldLeft(Map.empty[Int, String]) {
          case (macc, (Term.Body.Var(ident, _, _), i)) => macc + (i -> ident.name)
          case (macc, _) => macc
        }

        for (row2 <- relation.lookup(values.toArray)) {
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

    //  TODO: Ensure that filters occur last ...
    recur(rule.body, mutable.Map.empty ++ env0)



    /**
     * Extend the given environment `env` according to the given predicate `p`.
     */
    // TODO: Use loop instead.
    def visit(p: TypedAst.Predicate.Body, env: List[Map[String, Value]]): List[Map[String, Value]] = p match {
      case r: Predicate.Body.Relation => {

        val defn = sCtx.root.collections(r.name)
        defn match {
          case _: Collection.Relation =>

            val relation = dataStore.relations(r.name)
            val values = r.terms.toArray.map(t => peval(t, env.head)) // TODO: FIXME and switch to tuple-at-a-time.
            relation.lookup(values)

            // OLD implementation ----
            val table = dbRel(r.name)
            table flatMap {
              case row => unifyRelRow(row, r.terms) match {
                case None => List.empty
                case Some(m) => extend(env, m)
              }
            }
          // OLD implementation ----
          case l: Collection.Lattice =>
            val table = dbLat(r.name)

            val keyAttr = l.keys
            val (keyTerms, valueTerms) = r.terms.splitAt(keyAttr.size)

            val res = table flatMap {
              case (keys, values) =>
                unifyRelRow(keys, keyTerms) match {
                  case None => List.empty[Map[String, Value]]
                  case Some(m) => unifyLatRow(values, valueTerms) match {
                    case None => List.empty[Map[String, Value]]
                    case Some(m2) =>
                      extend(env, m2) // TODO: This is incorrect if env/m2 contains lattice variables.
                  }
                }
            }

            res.toList
        }
      }

      case r: Predicate.Body.Function =>
        val f = sCtx.root.constants(r.name)
        env flatMap {
          case m =>
            val result = Interpreter.evalCall(f, r.terms, sCtx.root, m).toBool
            if (!result)
              None
            else
              Some(m)
        }

      case Predicate.Body.NotEqual(ident1, ident2, _, _) =>
        env flatMap {
          case m =>
            val v1 = m(ident1.name)
            val v2 = m(ident2.name)
            if (v1 == v2)
              None
            else
              Some(m)
        }

      case Predicate.Body.Read(terms, path, _, _) => ???
    }

    // fold the environment over every rule in the body.
    val envs = rule.body.foldLeft(List(env0)) {
      case (env1, p) => visit(p, env1)
    }

    // evaluate the head predicate for every satisfying environment. 
    for (env <- envs) {
      evalHead(rule, env)
    }
  }


  /**
   * Unifies the given `row` with the given terms.
   */
  def unifyRelRow(row: List[Value], terms: List[Body]): Option[Map[String, Value]] = {
    assert(row.length == terms.length)

    (row zip terms).foldLeft(Option(Map.empty[String, Value])) {
      case (None, (value, term)) => None
      case (Some(macc), (value, term)) => term match {
        case Term.Body.Wildcard(tpe, loc) => Some(macc)
        case Term.Body.Var(ident, tpe, loc) => macc.get(ident.name) match {
          case None => Some(macc + (ident.name -> value))
          case Some(otherValue) =>
            if (value != otherValue)
              None
            else
              Some(macc)
        }
        case Term.Body.Lit(lit, tpe, loc) =>
          val otherValue = Interpreter.evalLit(lit)
          if (value != otherValue)
            None
          else
            Some(macc)
      }
    }
  }

  def unifyLatRow(values: List[Value], terms: List[Body]): Option[Map[String, Value]] = {
    assert(values.length == terms.length)

    (values zip terms).foldLeft(Option(Map.empty[String, Value])) {
      case (None, (value, term)) => None
      case (Some(macc), (value, term)) => term match {
        case Term.Body.Wildcard(tpe, loc) => Some(macc)
        case Term.Body.Var(ident, tpe, loc) => macc.get(ident.name) match {
          case None => Some(macc + (ident.name -> value))
          case Some(otherValue) =>
            if (value != otherValue)
              None
            else
              Some(macc)
        }
        case Term.Body.Lit(lit, tpe, loc) =>
          // TODO: Has to call leq.
          ???
      }
    }
  }


  /**
   * Extends the given environments `envs` with the environment `m`.
   */
  def extend(envs: List[Map[String, Value]], m: Map[String, Value]): List[Map[String, Value]] = {
    envs flatMap {
      case env =>
        m.foldLeft(Option(env)) {
          case (None, _) => None
          case (Some(macc), (key, value)) => macc.get(key) match {
            case None => Some(macc + (key -> value))
            case Some(otherValue) =>
              if (value != otherValue)
                None
              else
                Some(macc)
          }
        }
    }
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

      case l: TypedAst.Collection.Lattice => dbLat.get(directive.name) match {
        case None => // nop
        case Some(table) =>
          val cols = l.keys.map(_.ident.name) ::: l.values.map(_.ident.name + "<>")
          val ascii = new AsciiTable().withCols(cols: _*)
          for ((keys, elms) <- table.toSeq.sortBy(_._1.head.toString)) {
            ascii.mkRow((keys map pretty) ::: (elms map pretty))
          }

          Console.println(l.name)
          ascii.write(System.out)
          Console.println()
          Console.println()
      }
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

  def peval(t: TypedAst.Term.Body, env: Map[String, Value]): Value = t match {
    case t: TypedAst.Term.Body.Wildcard => null
    case t: TypedAst.Term.Body.Var =>
      if (env contains t.ident.name)
        env(t.ident.name)
      else
        null
    case t: TypedAst.Term.Body.Lit => Interpreter.evalLit(t.lit)
  }

}
