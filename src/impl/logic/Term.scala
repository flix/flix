package impl.logic

sealed trait Term {
  /**
   * Returns the term where all occurences (up to lambda- and let terms)
   * of the given variable `x` has been replaced by the variable `y`.
   */
  def rename(x: Symbol.VariableSymbol, y: Symbol.VariableSymbol): Term = substitute(x, Term.Var(y))

  /**
   * Returns the term where all occurences (up to lambda- and match terms)
   * of the given variable `x` has been replaced by the term `t`.
   */
  def substitute(x: Symbol.VariableSymbol, t: Term): Term = this match {
    case Term.Unit =>     Term.Unit
    case Term.Bool(b) =>  Term.Bool(b)
    case Term.Int(i) =>   Term.Int(i)
    case Term.Str(s) =>   Term.Str(s)
    case Term.Set(xs) =>  Term.Set(xs map (_.substitute(x, t)))

    case Term.Var(s) if s == x => t
    case Term.Var(s) => Term.Var(s)

    case Term.Abs(s, typ, t1) if s == x => this
    case Term.Abs(s, typ, t1) => Term.Abs(s, typ, t1.substitute(x, t))
    case Term.App(t1, t2) => Term.App(t1.substitute(x, t), t2.substitute(x, t))

    case Term.IfThenElse(t1, t2, t3) => Term.IfThenElse(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Match(t1, rules) => Term.Match(t1.substitute(x, t), rules map {
      case (p, t2) if p.freeVars contains x => (p, t2)
      case (p, t2) => (p, t2.substitute(x, t))
    })

    case Term.UnaryOp(op, t1) => Term.UnaryOp(op, t1.substitute(x, t))
    case Term.BinaryOp(op, t1, t2) => Term.BinaryOp(op, t1.substitute(x, t), t2.substitute(x, t))

    case Term.Tag(s, t1, typ) =>            Term.Tag(s, t1.substitute(x, t), typ)
    case Term.Tuple2(t1, t2) =>             Term.Tuple2(t1.substitute(x, t), t2.substitute(x, t))
    case Term.Tuple3(t1, t2, t3) =>         Term.Tuple3(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Tuple4(t1, t2, t3, t4) =>     Term.Tuple4(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Term.Tuple5(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t), t5.substitute(x, t))
  }
}

object Term {

  /**
   * The Unit term.
   */
  case object Unit extends Term

  /**
   * A boolean constant term.
   */
  case class Bool(b: scala.Boolean) extends Term

  /**
   * An integer constant term.
   */
  case class Int(i: scala.Int) extends Term

  /**
   * A string constant term.
   */
  case class Str(s: java.lang.String) extends Term

  /**
   * A set constant term.
   */
  case class Set(xs: scala.collection.immutable.Set[Term]) extends Term

  /**
   * A variable term.
   */
  case class Var(name: Symbol.VariableSymbol) extends Term

  /**
   * A lambda term.
   */
  case class Abs(s: Symbol.VariableSymbol, typ: Type, t: Term) extends Term

  /**
   * An application term.
   */
  case class App(t1: Term, t2: Term) extends Term

  /**
   * A if-then-else term.
   */
  case class IfThenElse(t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A match term.
   */
  case class Match(t: Term, rules: List[(Pattern, Term)]) extends Term

  /**
   * A unary operator term.
   */
  case class UnaryOp(op: UnaryOperator, t1: Term) extends Term

  /**
   * A binary operator term.
   */
  case class BinaryOp(op: BinaryOperator, t1: Term, t2: Term) extends Term

  /**
   * A tagged term.
   */
  case class Tag(name: Symbol.NamedSymbol, t: Term, typ: Type.Sum) extends Term

  /**
   * A 2-tuple term.
   */
  case class Tuple2(t1: Term, t2: Term) extends Term

  /**
   * A 3-tuple term.
   */
  case class Tuple3(t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-tuple term.
   */
  case class Tuple4(t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-tuple term.
   */
  case class Tuple5(t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term
}
