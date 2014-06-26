import scala.language.implicitConversions;

import SymTerm._

object Main {

  implicit def symbol2term(s: Symbol): Var = Var(s.name);

  def main(args: Array[String]): Unit = {

    val equal = (v1: SymTerm.Var, v2: SymTerm.Var) => SmtTerm.Conj(
      SmtTerm.IntEq(SmtTerm.Var(v2.name + "_b"), SmtTerm.Var(v1.name + "_b")),
      SmtTerm.IntEq(SmtTerm.Var(v1.name + "_e"), SmtTerm.Var(v2.name + "_e"))
    );

    val order = (v1: SymTerm.Var, v2: SymTerm.Var) => SmtTerm.Conj(
      SmtTerm.IntLeq(SmtTerm.Var(v2.name + "_b"), SmtTerm.Var(v1.name + "_b")),
      SmtTerm.IntLeq(SmtTerm.Var(v1.name + "_e"), SmtTerm.Var(v2.name + "_e"))
    );

    implicit val lattice = new Lattice(equal, order);

    // Partial Order Constraints
    val reflexivity = 'x <= 'x;
    val antisymmetri = (('x <= 'y) /\ ('y <= 'x)) ==> ('x === 'y);
    val transitivity = (('x <= 'y) /\ ('y <= 'z)) ==> ('x <= 'z);

    println(reflexivity);
    println(antisymmetri);
    println(transitivity);
    println();

    println("; Reflexivity");
    println(SmtTerm.emit(SymTerm.compile(reflexivity)));
    println();

    println("; Antisymmetri");
    println(SmtTerm.emit(SymTerm.compile(antisymmetri)));
    println();

    println("; Transitivity");
    println(SmtTerm.emit(SymTerm.compile(transitivity)));
    println();

  }

}
