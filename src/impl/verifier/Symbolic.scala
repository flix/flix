package impl.verifier

import impl.logic._
import impl.runtime.Interpreter
import syntax.Symbols._
import syntax.Terms._
import syntax.Terms.RichTerm

object Symbolic {

  /**
   * Constraints over integers.
   */
  sealed trait Constraint {
    def fmt: String = ???
  }

  object Constraint {

    case class Var(x: Symbol.VariableSymbol) extends Constraint

    /**
     * Negation.
     */
    case class Not(c: Constraint) extends Constraint

    /**
     * Conjunction.
     */
    case class And(c1: Constraint, c2: Constraint) extends Constraint

    /**
     * Disjunction.
     */
    case class Or(c1: Constraint, c2: Constraint) extends Constraint

    case class Eq() extends Constraint

    case class Neq() extends Constraint

  }


  import Constraint._


  def verify(program: Program): Unit = {
    // TODO: Need a notion of lattices. ..

    for (declaration <- program.declarations) {
      declaration match {
        case Declaration.DeclareBot(t, typ) =>
          val leq = program.lookupLeq(typ).get
          leastElement(t, leq)

        case Declaration.DeclareLeq(leq, typ) =>
          tautology("refl", Property.reflexivity(leq))
          antiSymmetri(leq)
          transitivity(leq)

        case Declaration.DeclareLub(lub, typ) =>
          val baseType = typ.resultType
          val leq = program.lookupLeq(baseType).get
          upperBound(leq, lub)

        case Declaration.DeclareHeight(t, typ) =>
      }
    }
  }


  /**
   * Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
   */
  def antiSymmetri(leq: Term.Abs): Unit = {
    val f = Term.Abs('x, leq.typ, Term.Abs('y, leq.typ,
      (leq.call('x, 'y) && leq.call('y, 'x)) ==> (Term.Var('x) === Term.Var('y))))

    tautology("Anti-Symmetri", f)
  }

  /**
   * Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
   */
  def transitivity(leq: Term.Abs): Unit = {
    val f = Term.Abs('x, leq.typ, Term.Abs('y, leq.typ, Term.Abs('z, leq.typ,
      (leq.call('x, 'y) && leq.call('y, 'z)) ==> leq.call('x, 'z))))

    tautology("Transitivity", f)
  }

  /**
   * Least Element: ∀x. ⊥ ⊑ x.
   */
  def leastElement(bot: Value, leq: Term.Abs): Unit = {
    val f = Term.App(leq, bot.toTerm)
    tautology("Least Element", f)
  }

  /**
   * Upper Bound: ∀x, y, z. x ⊑ (x ⨆ y) ∧ y ⊑ (x ⨆ y).
   */
  def upperBound(leq: Term.Abs, lub: Term.Abs): Unit = {
    //    val f = Term.Abs(x, typ, Term.Abs(y, typ, Term.Abs(z, typ,
    //      leq.call(x, lub.call(x, y))
    //    )))

    //tautology("Upper Bound", f)
  }


  /**
   * Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
   */
  def leastUpperBound(leq: Term.Abs, lub: Term.Abs): String = ???


  /**
   * Verifies whether the given term `t1` evaluates to `true` for all inputs.
   */
  def tautology(name: String, t0: Term): Unit = {
    def iterate(t: Term): Unit = {
      val abs = evaluate(t)
      abs match {
        case Term.Bool(true) => println("OK")
        case Term.Bool(false) => println("NOTOK"); ??? // TODO: Need ADT

        case x: Term.Abs =>
          for (a <- enumerate(x.typ)) {
            print("  Elm: " + a.fmt + " ")
            val r = evaluate(Term.App(x, a))
            iterate(r)
          }

        case x => ???
        //          if (isNormalForm(x))
        //            genVc(x)
        //          else ???
      }
    }
    println(name)
    iterate(t0)
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

    //    case Term.Match

    case Term.UnaryOp(op, t1) =>
      val r1 = evaluate(t1)
      r1.asValue match {
        case None => Term.UnaryOp(op, r1)
        case Some(v1) => Interpreter.apply(op, v1).toTerm
      }
    case Term.BinaryOp(op, t1, t2) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      (r1.asValue, r2.asValue) match {
        case (Some(v1), Some(v2)) => Interpreter.apply(op, v1, v2).toTerm
        case _ => Term.BinaryOp(op, r1, r2)
      }
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
    case (p, t1) :: rest => unify(p, t) match {
      case None => matchRule(rest, t)
      case Some(env) => Some((t1, env))
    }
  }

  private def unify(p: Pattern, t: Term): Option[Map[Symbol.VariableSymbol, Term]] = (p, t) match {
    case (Pattern.Wildcard, _) => Some(Map.empty)
    case (Pattern.Var(x), _) => Some(Map(x -> t))

    case (Pattern.Unit, Term.Unit) => Some(Map.empty)
    case (Pattern.Bool(b1), Term.Bool(b2)) if b1 == b2 => Some(Map.empty)
    case (Pattern.Int(i1), Term.Int(i2)) if i1 == i2 => Some(Map.empty)
    case (Pattern.Str(s1), Term.Str(s2)) if s1 == s2 => Some(Map.empty)

    case (Pattern.Tag(s1, p1), Term.Tag(s2, t1, _)) if s1 == s2 => unify(p1, t1)

    case (Pattern.Tuple2(p1, p2), Term.Tuple2(t1, t2)) =>
      for (ent1 <- unify(p1, t1);
           ent2 <- unify(p2, t2))
      yield ent1 ++ ent2
    case (Pattern.Tuple3(p1, p2, p3), Term.Tuple3(t1, t2, t3)) =>
      for (ent1 <- unify(p1, t1);
           ent2 <- unify(p2, t2);
           ent3 <- unify(p3, t3))
      yield ent1 ++ ent2 ++ ent3
    case (Pattern.Tuple4(p1, p2, p3, p4), Term.Tuple4(t1, t2, t3, t4)) =>
      for (ent1 <- unify(p1, t1);
           ent2 <- unify(p2, t2);
           ent3 <- unify(p3, t3);
           ent4 <- unify(p4, t4))
      yield ent1 ++ ent2 ++ ent3 ++ ent4
    case (Pattern.Tuple5(p1, p2, p3, p4, p5), Term.Tuple5(t1, t2, t3, t4, t5)) =>
      for (ent1 <- unify(p1, t1);
           ent2 <- unify(p2, t2);
           ent3 <- unify(p3, t3);
           ent4 <- unify(p4, t4);
           ent5 <- unify(p5, t5))
      yield ent1 ++ ent2 ++ ent3 ++ ent4 ++ ent5

    case _ => None
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


  //  /**
  //   * Returns `true` iff the given term `t` is in normal form.
  //   */
  //  def isNormalForm(t: Term): Boolean = {
  //    def isInt(t: Term): Boolean = t match {
  //      case Term.Int(i) => true
  //
  //    }
  //
  //    t match {
  //      case Term.Unit => false
  //      case Term.Bool(b) => true
  //      case Term.Int(i) => false
  //      case Term.Str(s) => false
  //
  //
  //      case _: Term.Tag => false
  //      case _: Term.Tuple2 => false
  //      case _: Term.Tuple3 => false
  //      case _: Term.Tuple4 => false
  //      case _: Term.Tuple5 => false
  //    }
  //  }
  //
  //
  //  def genVc(t: Term): Constraint = t match {
  //    case Term.BinaryOp(op, t2, t3) => ???
  //
  //    case Term.IfThenElse(t1, t2, t3) =>
  //      val c1 = genVc(t1)
  //      val c2 = genVc(t2)
  //      val c3 = genVc(t3)
  //      Or(And(c1, c2), And(Not(c1), c3))
  //
  //    case Term.BinaryOp(BinaryOperator.Equal, t1, t2) => ???
  //  }


}
