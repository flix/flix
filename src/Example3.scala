object Example3 {

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

    // constraints, inverted style
    New.exists((var1, obj) =>
      VarPointsTo(var1, obj))

    Assign.exists((var1, var2) =>
      VarPointsTo.exists((var2, obj) =>
        VarPointsTo(var1, obj)));

    Load.exists((resultVar, baseVar, field) =>
      VarPointsTo.exists((baseVar, baseObj) =>
        HeapPointsTo.exists((baseObj, field, resultObj) =>
          VarPointsTo(resultVar, resultObj))))

    Store.exists((baseVar, field, valueVar) =>
      VarPointsTo.exists((baseVar, baseObj) =>
        VarPointsTo.exists((valueVar, valueObj) =>
          HeapPointsTo(baseObj, field, valueObj))))
  }


  class Sort {
    def mkVar: Variable[this.type] = ???
  }

  case class Relation2[A, B]() {
    def apply(a: Variable[A], b: Variable[B]): Term = ???

    def exists(f: (Variable[A], Variable[B]) => Term): Term = ???
  }


  case class Relation3[A, B, C]() {
    def apply(a: Variable[A], b: Variable[B], c: Variable[C]): Term = ???

    def exists(f: (Variable[A], Variable[B], Variable[C]) => Term): Term = ???
  }

  class Variable[V]()

  trait Term;

}
