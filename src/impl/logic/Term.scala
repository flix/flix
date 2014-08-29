package impl.logic

sealed trait Term {
  /**
   * Returns the term where all occurences (up to lambda- and let terms)
   * of the given variable `x` has been replaced by the variable `y`.
   */
  def rename(x: Symbol.VariableSymbol, y: Symbol.VariableSymbol): Term = this match {
    case Term.Unit =>     this
    case Term.Bool(b) =>  this
    case Term.Int(i) =>   this
    case Term.Str(s) =>   this
    case Term.Set(xs) =>  Term.Set(xs map (_.rename(x, y)))

    case Term.Var(s) if s == x => Term.Var(y)
    case Term.Var(s) => this

    case Term.Abs(s, typ, t) if s == x =>   this
    case Term.Abs(s, typ, t) =>             Term.Abs(s, typ, t.rename(x, y))
    case Term.App(t1, t2) =>                Term.App(t1.rename(x, y), t2.rename(x, y))

    case Term.UnaryOp(op, t1) =>            Term.UnaryOp(op, t1.rename(x, y))
    case Term.BinaryOp(op, t1, t2) =>       Term.BinaryOp(op, t1.rename(x, y), t2.rename(x, y))
    case Term.IfThenElse(t1, t2, t3) =>     Term.IfThenElse(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y))
    case Term.Tagged(s, t, typ) =>          Term.Tagged(s, t.rename(x, y), typ)
    case Term.Tuple2(t1, t2) =>             Term.Tuple2(t1.rename(x, y), t2.rename(x, y))
    case Term.Tuple3(t1, t2, t3) =>         Term.Tuple3(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y))
    case Term.Tuple4(t1, t2, t3, t4) =>     Term.Tuple4(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y), t4.rename(x, y))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Term.Tuple5(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y), t4.rename(x, y), t5.rename(x, y))
  }

  /**
   * Returns the term where all occurences (up to lambda- and let terms)
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

    case Term.UnaryOp(op, t1) => Term.UnaryOp(op, t1.substitute(x, t))
    case Term.BinaryOp(op, t1, t2) => Term.BinaryOp(op, t1.substitute(x, t), t2.substitute(x, t))
    case Term.IfThenElse(t1, t2, t3) => Term.IfThenElse(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))

    case Term.Tagged(s, t1, typ) =>         Term.Tagged(s, t1.substitute(x, t), typ)
    case Term.Tuple2(t1, t2) =>             Term.Tuple2(t1.substitute(x, t), t2.substitute(x, t))
    case Term.Tuple3(t1, t2, t3) =>         Term.Tuple3(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Tuple4(t1, t2, t3, t4) =>     Term.Tuple4(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Term.Tuple5(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t), t5.substitute(x, t))
  }

  /**
   * Returns the set of free variables in the term.
   */
  def freeVariables: Set[Symbol.VariableSymbol] = this match {
    case Term.Unit =>     Set.empty
    case Term.Bool(b) =>  Set.empty
    case Term.Int(i) =>   Set.empty
    case Term.Str(s) =>   Set.empty
    case Term.Set(xs) =>  xs flatMap (_.freeVariables)

    case Term.Var(s) => Set(s)
    case Term.Abs(x, typ, t) =>               t.freeVariables - x
    case Term.App(t1, t2) =>                  t1.freeVariables ++ t2.freeVariables

    case Term.UnaryOp(op, t) =>               t.freeVariables
    case Term.BinaryOp(op, t1, t2) =>         t1.freeVariables ++ t2.freeVariables
    case Term.IfThenElse(t1, t2, t3) =>       t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables

    case Term.Tagged(s, t, typ) =>            t.freeVariables
    case Term.Tuple2(t1, t2) =>               t1.freeVariables ++ t2.freeVariables
    case Term.Tuple3(t1, t2, t3) =>           t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables
    case Term.Tuple4(t1, t2, t3, t4) =>       t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables ++ t4.freeVariables
    case Term.Tuple5(t1, t2, t3, t4, t5) =>   t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables ++ t4.freeVariables ++ t5.freeVariables
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
   * An if-then-else term.
   */
  case class IfThenElse(t1: Term, t2: Term, t3: Term) extends Term

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
  case class Tagged(name: Symbol.NamedSymbol, t: Term, typ: Type.Sum) extends Term

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
