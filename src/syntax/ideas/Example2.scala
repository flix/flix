package syntax.ideas

object Example2 {

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

    // declare all variables ahead of time
    val (var1, var2, baseVar, resultVar) = (Variable.mkVar, Variable.mkVar, Variable.mkVar, Variable.mkVar)
    val (obj, baseObj, valueObj) = (Obj.mkVar, Obj.mkVar, Obj.mkVar)
    val field = Field.mkVar

    // constraints
    VarPointsTo(var1, obj) :- New(var1, obj)

    VarPointsTo(var1, obj) :- Assign(var1, var2) /\
      VarPointsTo(var2, obj)

    VarPointsTo(resultVar, obj) :-
      Load(resultVar, baseVar, field) /\
        VarPointsTo(baseVar, baseObj) /\
        HeapPointsTo(baseObj, field, obj)

    HeapPointsTo(baseObj, field, valueObj) :-
      Store(var1, field, var2) /\
        VarPointsTo(var1, baseObj) /\
        VarPointsTo(var2, valueObj)
  }


  class Sort {
    def mkVar: Variable[this.type] = ???
  }

  case class Relation2[A, B]() {
    def apply(a: Variable[A], b: Variable[B]): Term = ???
  }

  case class Relation3[A, B, C]() {
    def apply(a: Variable[A], b: Variable[B], c: Variable[C]): Term = ???
  }

  class Variable[V]()

  trait Term {
    def :-(t: Term): Term = ???

    def /\(t: Term): Term = ???
  }

}
