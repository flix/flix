package impl.verifier

import impl.logic._
import impl.runtime.{Unification, Interpreter}
import syntax.Terms.RichTerm

import scala.collection.mutable

object Verifier {

  case class Lattice(typ: Type, bot: Value, leq: Term.Abs, lub: Term.Abs, height: Term.Abs)

  def verify(program: Program): Unit = {
    val lattices = mutable.Set.empty[Lattice]
    for (declaration <- program.declarations) {
      declaration match {
        case Declaration.DeclareBot(bot, typ) if !typ.isInstanceOf[Type.Set] =>
          val leq = program.lookupLeq(typ).get
          val lub = program.lookupLub(typ).get
          val height = program.lookupHeight(typ).get
          lattices += Lattice(typ, bot, leq, lub, height)

        case _ => // nop
      }
    }

    for (lattice <- lattices) {
      verify(lattice)
    }
  }

  /**
   * Verifies that the given lattie satisfies all required lattice properties.
   */
  def verify(l: Lattice): Unit = {
    {
      println("Verifying Reflexivity:")
      val cases = tautology(Property.reflexivity(l.leq))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Reflexivity proved.")
      println()
    }

    {
      println("Verifying Anti-symmetry:")
      val cases = tautology(Property.antiSymmetri(l.leq))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Anti-symmetry proved.")
      println()
    }

    {
      println("Verifying Transitivity:")
      val cases = tautology(Property.transitivity(l.leq))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Transitivity proved.")
      println()
    }

    {
      println("Verifying Upperbound:")
      val cases = tautology(Property.upperBound(l.leq, l.lub))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Upperbound proved.")
      println()
    }

    {
      println("Verifying Least-Upperbound:")
      val cases = tautology(Property.leastUpperBound(l.leq, l.lub))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Least-Upperbound proved.")
      println()
    }

    {
      println("Verifying Non-Negative-Height:")
      val cases = tautology(Property.nonNegative(l.height))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Non-Negative-Height: proved.")
      println()
    }

    {
      println("Verifying Strictly-Decreasing:")
      val cases = tautology(Property.strictlyDecreasing(l.height, l.leq))
      cases.foreach(xs => println("  " + xs.map(_.fmt).mkString(", ") + ", proven."))
      println("Strictly-Decreasing: proved.")
      println()
    }
  }


  /**
   * Verifies whether the given term `t1` evaluates to `true` for all inputs.
   */
  def tautology(t0: Term): List[List[Term]] = {
    def iterate(t: Term, history: List[Term]): List[List[Term]] = evaluate(t) match {
      case Term.Bool(b) =>
        // Case 1: The term has been fully evaluated to a boolean.
        if (b)
        // The property has been proven to hold.
          List(history)
        else
        // The property does not hold!
          throw new RuntimeException("Violation for input: " + history.map(_.fmt).mkString(", "))

      case Term.Abs(x, typ, t1) =>
        // Case 2: Term has not yet been fully applied. 
        // Enumerate all terms of the argument and evaluate the term.
        enumerate(typ).toList.flatMap(a => iterate(evaluate(Term.App(t, a)), a :: history))

      case r =>
        // Case 3: The term cannot be evaluated further.
        // What remains is residual of constraints.
        // Translate the term into a constraint system.
        val constraint = BoolExp.compile(r)
        println(constraint.fmt)
        List(history)
    }

    iterate(t0, Nil)
  }

  /**
   * Computes the fixpoint of the given term `t0` by iteratively evaluating the term and lifting it.
   */
  def fixpoint(t0: Term): Term = {
    var last = t0
    var current = lift(evaluate(t0))
    while (current != last) {
      last = current
      current = lift(evaluate(current))
    }
    current
  }

  /**
   * Returns the partial evaluation of the given term `t`.
   */
  def evaluate(t: Term): Term = t match {
    case Term.Unit => Term.Unit
    case Term.Bool(b) => Term.Bool(b)
    case Term.Int(i) => Term.Int(i)
    case Term.Str(s) => Term.Str(s)
    case Term.Set(xs) => Term.Set(xs.map(evaluate))

    case Term.Var(x) => Term.Var(x)
    case Term.Abs(s, typ, t1) => Term.Abs(s, typ, t1)

    case Term.App(t1, t2) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)

      r1 match {
        case Term.Abs(x, _, tb) =>
          val y = Symbol.freshVariableSymbol(x)
          val r = tb.rename(x, y).substitute(y, r2)
          evaluate(r)
        case _ => throw new RuntimeException()
      }

    case Term.Match(t1, rules) =>
      val r1 = evaluate(t1)
      println(r1)
      // TODO: We need to apply the match partially.
      // TODO: Introduce asPatternValue and only allow unification on this.
      // THen introduce rewrite rules which are used in a fixpoint.

      matchRule(rules, r1) match {
        case None => throw new RuntimeException(s"Unmatched value ${r1.fmt}")
        case Some((t2, env)) => evaluate(t2.substitute2(env))
      }

    case Term.IfThenElse(t1, t2, t3) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      val r3 = evaluate(t3)

      r1.asValue match {
        case None => Term.IfThenElse(r1, r2, r3)
        case Some(v) => if (v.toBool) r2 else r3
      }

    case Term.UnaryOp(op, t1) =>
      apply(op, evaluate(t1))
    case Term.BinaryOp(op, t1, t2) =>
      apply(op, evaluate(t1), evaluate(t2))
    case Term.Tag(n, t1, typ) => Term.Tag(n, evaluate(t1), typ)
    case Term.Tuple2(t1, t2) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      Term.Tuple2(r1, r2)
    case Term.Tuple3(t1, t2, t3) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      val r3 = evaluate(t3)
      Term.Tuple3(r1, r2, r3)
    case Term.Tuple4(t1, t2, t3, t4) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      val r3 = evaluate(t3)
      val r4 = evaluate(t4)
      Term.Tuple4(r1, r2, r3, r4)
    case Term.Tuple5(t1, t2, t3, t4, t5) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      val r3 = evaluate(t3)
      val r4 = evaluate(t4)
      val r5 = evaluate(t5)
      Term.Tuple5(r1, r2, r3, r4, r5)
  }

  /**
   * Optionally returns a pair of a term and environment for which one of the given `rules` match the given value `v`.
   */
  private def matchRule(rules: List[(Pattern, Term)], t: Term): Option[(Term, Map[Symbol.VariableSymbol, Term])] = rules match {
    case Nil => None
    case (p, t1) :: rest => Unification.unify(p, t) match {
      case None => matchRule(rest, t)
      case Some(env) => Some((t1, env))
    }
  }

  /**
   * Lifts if-then-else expressions higher in the term given term `t`.
   */
  private def lift(t: Term): Term = t match {
    case Term.Unit => Term.Unit
    case Term.Bool(b) => Term.Bool(b)
    case Term.Int(i) => Term.Int(i)
    case Term.Str(s) => Term.Str(s)
    case Term.Set(xs) => Term.Set(xs.map(lift))

    case Term.Var(x) => Term.Var(x)
    case Term.Abs(s, typ, t1) => Term.Abs(s, typ, lift(t1))
    case Term.App(t1, t2) => Term.App(lift(t1), lift(t2))

    case Term.Match(t1, rules) => lift(t1) match {
      case Term.IfThenElse(x, y, z) => Term.IfThenElse(x, Term.Match(y, rules), Term.Match(z, rules))
      case t2 => Term.Match(t2, rules)
    }
    case Term.IfThenElse(t1, t2, t3) => Term.IfThenElse(lift(t1), lift(t2), lift(t3))

    case Term.UnaryOp(op, t1) => Term.UnaryOp(op, lift(t1))
    case Term.BinaryOp(op, t1, t2) => Term.BinaryOp(op, lift(t1), lift(t2))

    case Term.Tag(n, t1, typ) => lift(t1) match {
      case Term.IfThenElse(x, y, z) => Term.IfThenElse(x, Term.Tag(n, y, typ), Term.Tag(n, z, typ))
      case t2 => Term.Tag(n, t2, typ)
    }

    case Term.Tuple2(Term.IfThenElse(x, y, z), t2) => Term.IfThenElse(x, Term.Tuple2(y, t2), Term.Tuple2(z, t2))
    case Term.Tuple2(t1, Term.IfThenElse(x, y, z)) => Term.IfThenElse(x, Term.Tuple2(t1, y), Term.Tuple2(t1, z))

    case _ => t
  }

  /**
   * Returns the result of (partially) applying the unary operator `op` to the given term `t`.
   */
  private def apply(op: UnaryOperator, t: Term): Term = t.asValue match {
    case None => Term.UnaryOp(op, t)
    case Some(v) => op match {
      case UnaryOperator.Not => Term.Bool(!v.toBool)
      case UnaryOperator.UnaryPlus => Term.Int(v.toInt)
      case UnaryOperator.UnaryMinus => Term.Int(-v.toInt)
    }
  }

  /**
   * Returns the result of (partially) applying the binary operator `op` to the given terms `t1` and `t2`.
   */
  private def apply(op: BinaryOperator, t1: Term, t2: Term): Term = op match {
    case BinaryOperator.Or => (t1.asValue, t2.asValue) match {
      case (Some(v1), Some(v2)) => Term.Bool(v1.toBool || v2.toBool)
      case (Some(v1), None) if v1.toBool => Term.Bool(true)
      case (None, Some(v2)) if v2.toBool => Term.Bool(true)
      case _ => Term.BinaryOp(op, t1, t2)
    }
    case BinaryOperator.And => (t1.asValue, t2.asValue) match {
      case (Some(v1), Some(v2)) => Term.Bool(v1.toBool && v2.toBool)
      case (Some(v1), None) if !v1.toBool => Term.Bool(false)
      case (None, Some(v2)) if !v2.toBool => Term.Bool(false)
      case _ => Term.BinaryOp(op, t1, t2)
    }
    case BinaryOperator.Equal => (t1.asValue, t2.asValue) match {
      case (Some(v1), Some(v2)) => Term.Bool(v1 == v2)
      case _ => peel(t1, t2, (x: Term, y: Term) => Term.BinaryOp(BinaryOperator.Equal, x, y))
    }
  }

  /**
   * TODO: Should this have f or not?
   */
  private def peel(t1: Term, t2: Term, f: (Term, Term) => Term): Term = (t1, t2) match {
    case (Term.Tag(n1, x, _), Term.Tag(n2, y, _)) =>
      if (n1 != n2)
        Term.Bool(false)
      else
        peel(x, y, f)
    case _ => f(t1, t2)
  }


  /**
   * Returns *all* terms which inhabit the given type `typ`.
   *
   * If the type is finite then a closed term is returned.
   * Otherwise an open term is returned (i.e. a term with variables).
   */
  def enumerate(typ: Type, sum: Option[Type.Sum] = None): Set[Term] = typ match {
    case Type.Unit => Set(Term.Unit)
    case Type.Bool => Set(Term.Bool(true), Term.Bool(false))
    case Type.Int => Set(Term.Var(Symbol.freshVariableSymbol("i")))

    case Type.Tag(n, typ2) => enumerate(typ2).map(x => Term.Tag(n, x, sum.get))
    case Type.Sum(ts) => ts.flatMap(x => enumerate(x, Some(Type.Sum(ts)))).toSet

    case Type.Tuple2(typ1, typ2) =>
      for (t1 <- enumerate(typ1);
           t2 <- enumerate(typ2))
      yield Term.Tuple2(t1, t2)
    case Type.Tuple3(typ1, typ2, typ3) =>
      for (t1 <- enumerate(typ1);
           t2 <- enumerate(typ2);
           t3 <- enumerate(typ3))
      yield Term.Tuple3(t1, t2, t3)
    case Type.Tuple4(typ1, typ2, typ3, typ4) =>
      for (t1 <- enumerate(typ1);
           t2 <- enumerate(typ2);
           t3 <- enumerate(typ3);
           t4 <- enumerate(typ4))
      yield Term.Tuple4(t1, t2, t3, t4)
    case Type.Tuple5(typ1, typ2, typ3, typ4, typ5) =>
      for (t1 <- enumerate(typ1);
           t2 <- enumerate(typ2);
           t3 <- enumerate(typ3);
           t4 <- enumerate(typ4);
           t5 <- enumerate(typ5))
      yield Term.Tuple5(t1, t2, t3, t4, t5)

    case Type.Set(xs) => throw new UnsupportedOperationException("Unexpected type.")
    case Type.Var(x) => throw new UnsupportedOperationException("Unexpected type.")
    case Type.Function(a, b) => throw new UnsupportedOperationException("Impossible to enumerate functions.")
    case Type.Str => throw new UnsupportedOperationException("Impossible to enumerate strings.")
  }

}
