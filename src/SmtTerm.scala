sealed trait SmtTerm;

object SmtTerm {
  case class Var(name: String) extends SmtTerm;
  case class IntEq(t1: Var, t2: Var) extends SmtTerm;
  case class IntLeq(t1: Var, t2: Var) extends SmtTerm;
  case class Not(t: SmtTerm) extends SmtTerm;
  case class Conj(t1: SmtTerm, t2: SmtTerm) extends SmtTerm;
  case class Disj(t1: SmtTerm, t2: SmtTerm) extends SmtTerm;

  def vars(t: SmtTerm): Set[String] = t match {
    case Var(name) => Set(name);
    case IntEq(t1, t2) => vars(t1) ++ vars(t2);
    case IntLeq(t1, t2) => vars(t1) ++ vars(t2);
    case Not(t1) => vars(t1);
    case Conj(t1, t2) => vars(t1) ++ vars(t2);
    case Disj(t1, t2) => vars(t1) ++ vars(t2);
  }

  def compile(t: SmtTerm): String = t match {
    case Var(name) => name;
    case IntEq(t1, t2) => "(= " + compile(t1) + " " + compile(t2) + ")";
    case IntLeq(t1, t2) => "(<= " + compile(t1) + " " + compile(t2) + ")";
    case Not(t1) => "(not " + compile(t1) + ")";
    case Conj(t1, t2) => "(and " + compile(t1) + " " + compile(t2) + ")";
    case Disj(t1, t2) => "(or " + compile(t1) + " " + compile(t2) + ")";
  }

  def emit(t: SmtTerm): String = {
    val sb = new StringBuilder();
    for (v <- vars(t)) {
      sb.append(s"(declare-fun $v () Int)\n");
    }
    sb.append("(assert " + compile(t) + ")\n");
    sb.append("(check-sat)\n");
    sb.toString();
  }
}