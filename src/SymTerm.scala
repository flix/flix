sealed trait SymTerm {
  def /\(that: SymTerm): SymTerm = SymTerm.Conj(this, that);

  def \/(that: SymTerm): SymTerm = SymTerm.Disj(this, that);

  def ==>(that: SymTerm): SymTerm = SymTerm.Disj(SymTerm.Not(this), that);
}

object SymTerm {
  case class Var(name: String) extends SymTerm {
    def ===(that: Var)(implicit l: Lattice): SymTerm = Eq(this, that, l);
    def <=(that: Var)(implicit l: Lattice): SymTerm = Leq(this, that, l);
  }

  case class Eq(t1: Var, t2: Var, l: Lattice) extends SymTerm;
  case class Leq(t1: Var, t2: Var, l: Lattice) extends SymTerm;
  case class Not(t: SymTerm) extends SymTerm;
  case class Conj(t1: SymTerm, t2: SymTerm) extends SymTerm;
  case class Disj(t1: SymTerm, t2: SymTerm) extends SymTerm;

  def compile(t: SymTerm): SmtTerm = t match {
    case Var(n) => SmtTerm.Var(n);
    case Eq(v1, v2, l) => l.equal(v1, v2);
    case Leq(v1, v2, l) => l.order(v1, v2);
    case Not(t1) => SmtTerm.Not(compile(t1));
    case Conj(t1, t2) => SmtTerm.Conj(compile(t1), compile(t2));
    case Disj(t1, t2) => SmtTerm.Disj(compile(t1), compile(t2));
  }
}
