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
    val f = Interpreter.evaluate(Term.App(leq, bot.toTerm)).toTerm
    tautology(f.asInstanceOf[Term.Abs])
  }


  def tautology(t1: Term.Abs): Unit = {
    for (t2 <- enumerate(t1.typ)) {
      println(t2.fmt)
      val r = interpret(Term.App(t1, t2))

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
  def interpret(t: Term): Term = ???


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
