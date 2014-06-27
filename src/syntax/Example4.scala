package syntax

object Example4 {

  def main(args: Array[String]) {

    // types
    val Variable = new Sort()
    val Obj = new Sort()
    val Field = new Sort()

    // inputs facts
    val New = Relation2[Variable.type, Obj.type]()
    val Assign = Relation2[Variable.type, Variable.type]()
    val Load = Relation3[Variable.type, Variable.type, Field.type]()
    val Store = Relation3[Variable.type, Field.type, Variable.type]()

    // points-to relations
    val VarPointsTo = Relation2[Variable.type, Obj.type]()
    val HeapPointsTo = Relation3[Obj.type, Field.type, Obj.type]()

    // constraints, wierd apply style
    {
      val (var1, obj) = New();
      VarPointsTo(var1, obj);
    }

    {
      val (var1, var2) = Assign();
      val obj = VarPointsTo(var2);
      VarPointsTo(var1, obj);
    }

    {
      val (resultVar, baseVar, field) = Load();
      val baseObj = VarPointsTo(baseVar);
      val resultObj = HeapPointsTo(baseObj, field);
      VarPointsTo(resultVar, resultObj);
    }

    {
      val (baseVar, field, valueVar) = Store();
      val baseObj = VarPointsTo(baseVar);
      val valueObj = VarPointsTo(valueVar);
      HeapPointsTo(baseObj, field, valueObj);
    }
  }


  class Sort {
    def mkVar: Variable[this.type] = ???
  }

  case class Relation2[A, B]() {
    def apply(): (Variable[A], Variable[B]) = ???

    def apply(a: Variable[A]): Variable[B] = ???

    def apply(a: Variable[A], b: Variable[B]): Term = ???
  }


  case class Relation3[A, B, C]() {
    def apply(): (Variable[A], Variable[B], Variable[C]) = ???

    def apply(a: Variable[A], b: Variable[B]): Variable[C] = ???

    def apply(a: Variable[A], b: Variable[B], c: Variable[C]): Term = ???
  }

  class Variable[V]()

  trait Term;

}
