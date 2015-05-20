package syntax.ideas

object Example0 {

  def main(args: Array[String]) {

    // inputs facts
    val New = new Relation2()
    val Assign = new Relation2()
    val Load = new Relation3()
    val Store = new Relation3()

    // points-to relations
    val VarPointsTo = new Relation2()
    val HeapPointsTo = new Relation3()

    // untyped constraints
    VarPointsTo('var1, 'obj) :- New('var1, 'obj)

    VarPointsTo('var1, 'obj) :- Assign('var1, 'var2) /\
      VarPointsTo('var2, 'obj)

    VarPointsTo('resultVar, 'obj) :-
      Load('resultVar, 'baseVar, 'field) /\
        VarPointsTo('baseVar, 'baseObj) /\
        HeapPointsTo('baseObj, 'field, 'obj)

    HeapPointsTo('baseObj, 'field, 'valueObj) :-
      Store('var1, 'field, 'var2) /\
        VarPointsTo('var1, 'baseObj) /\
        VarPointsTo('var2, 'valueObj)
  }

  case class Relation2() {
    def apply(a: Symbol, b: Symbol): Term = ???
  }

  case class Relation3() {
    def apply(a: Symbol, b: Symbol, c: Symbol): Term = ???
  }

  trait Term {
    def :-(t: Term): Term = ???

    def /\(t: Term): Term = ???
  }

}
