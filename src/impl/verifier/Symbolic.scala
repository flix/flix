package impl.verifier

import impl.logic._
import impl.runtime.Interpreter
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
    for (declaration <- program.declarations) {
      declaration match {
        case Declaration.DeclareBot(t, typ) =>
          val leq = program.lookupLeq(typ).get
          leastElement(t, leq)

        case Declaration.DeclareLeq(t, typ) =>
        case Declaration.DeclareLub(t, typ) =>
        case Declaration.DeclareHeight(t, typ) =>
      }
    }
  }

  /**
   * Least Element: ∀x. ⊥ ⊑ x.
   */
  def leastElement(bot: Value, leq: Term.Abs): Unit = {
    val t = Term.App(leq, bot.toTerm)
    val f = Interpreter.evaluate(t).toTerm // TODO: Need to simplify
    tautology(f.asInstanceOf[Term.Abs])
  }

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(leq: Term.Abs): Unit = {
    val x = Term.Var(Symbol.VariableSymbol("x"))
    val t = Term.App(Term.App(leq, x), x) // TODO: Use abs instead?
    val f = Interpreter.evaluate(t).toTerm // TODO: Need to simplify
    tautology(f.asInstanceOf[Term.Abs])
  }

  /**
   * Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
   */
  def antiSymmetri(typ: String, leq: String): String = ???

  /**
   * Verifies whether the given term `t1` evaluates to `true` for all inputs.
   */
  def tautology(t1: Term.Abs): Unit = {
    // TODO: Change type to term
    for (t2 <- enumerate(t1.typ)) {
      println(t2.fmt)
      val r = evaluate(Term.App(t1, t2))

      // TODO: It actually returns a set of constraints.
      r match {
        case Term.Bool(true) => // yay
        case Term.Bool(false) => ??? // nay!
        case x: Term.Abs => tautology(x)
        case x => if (isNormalForm(x))
          genVc(x)
        else ???
      }

    }
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

    //    case Term.Var
    //    case Term.Abs
    //    case Term.App

    case Term.IfThenElse(t1, t2, t3) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      val r3 = evaluate(t3)

      asValue(r1) match {
        case None => Term.IfThenElse(r1, r2, r3)
        case Some(v) => if (v.toBool) r2 else r3
      }

    //    case Term.Match

    case Term.UnaryOp(op, t1) =>
      val r1 = evaluate(t1)
      asValue(r1) match {
        case None => Term.UnaryOp(op, r1)
        case Some(v1) => Interpreter.apply(op, v1).toTerm
      }
    case Term.BinaryOp(op, t1, t2) =>
      val r1 = evaluate(t1)
      val r2 = evaluate(t2)
      (asValue(r1), asValue(r2)) match {
        case (Some(v1), Some(v2)) => Interpreter.apply(op, v1, v2).toTerm
        case _ => Term.BinaryOp(op, r1, r2)
      }
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
   * Optionally returns the term as a value.
   *
   * NB: *Does not* perform any computation to reduce the term to a value.
   */
  def asValue(t: Term): Option[Value] = ???

  /**
   * Returns *all* terms which inhabit the given type `typ`.
   */
  def enumerate(typ: Type, sum: Option[Type.Sum] = None): Set[Term] = typ match {
    case Type.Unit => Set(Term.Unit)
    case Type.Bool => Set(Term.Bool(true), Term.Bool(false))
    case Type.Int => Set(Term.Var(Symbol.freshVariableSymbol("i")))
    case Type.Str => throw new UnsupportedOperationException()
    case Type.Sum(ts) => ts.flatMap(x => enumerate(x, Some(Type.Sum(ts)))).toSet
    case Type.Tag(n, typ2) => enumerate(typ2).map(x => Term.Tag(n, x, sum.get))

  }


  /**
   * Returns `true` iff the given term `t` is in normal form.
   */
  def isNormalForm(t: Term): Boolean = {
    def isInt(t: Term): Boolean = t match {
      case Term.Int(i) => true

    }

    t match {
      case Term.Unit => false
      case Term.Bool(b) => true
      case Term.Int(i) => false
      case Term.Str(s) => false


      case _: Term.Tag => false
      case _: Term.Tuple2 => false
      case _: Term.Tuple3 => false
      case _: Term.Tuple4 => false
      case _: Term.Tuple5 => false
    }
  }


  def genVc(t: Term): Constraint = t match {
    case Term.BinaryOp(op, t2, t3) => ???

    case Term.IfThenElse(t1, t2, t3) =>
      val c1 = genVc(t1)
      val c2 = genVc(t2)
      val c3 = genVc(t3)
      Or(And(c1, c2), And(Not(c1), c3))

    case Term.BinaryOp(BinaryOperator.Equal, t1, t2) => ???
  }


}
