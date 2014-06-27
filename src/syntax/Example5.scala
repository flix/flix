package syntax

object Example5 {

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

    // constraints, monadic style
    for (
      (var1, obj) <- New.asM
    ) yield VarPointsTo(var1, obj)

    for (
      (var1, var2) <- Assign.asM;
      obj <- VarPointsTo(var2)
    ) yield VarPointsTo(var1, obj);

    for (
      (resultVar, baseVar, field) <- Load.asM;
      baseObj <- VarPointsTo(baseVar);
      resultObj <- HeapPointsTo(baseObj, field)
    ) yield VarPointsTo(resultVar, resultObj);

    for (
      (baseVar, field, valueVar) <- Store.asM;
      baseObj <- VarPointsTo(baseVar);
      valueObj <- VarPointsTo(valueVar)
    ) yield HeapPointsTo(baseObj, field, valueObj)

  }


  class Sort {
    def mkVar: Variable[this.type] = new Variable[this.type]
  }

  case class Relation2[A, B]() {
    def apply(a: Variable[A], b: Variable[B]): Monad[(Variable[A], Variable[B])] = new Monad[(Variable[A], Variable[B])]

    def apply(a: Variable[A]): Monad[Variable[B]] = new Monad[Variable[B]]

    def asM: Monad[(Variable[A], Variable[B])] = new Monad[(Variable[A], Variable[B])]
  }


  case class Relation3[A, B, C]() {
    def apply(a: Variable[A], b: Variable[B], c: Variable[C]): Monad[(Variable[A], Variable[B], Variable[C])] = new Monad[(Variable[A], Variable[B], Variable[C])]

    def apply(a: Variable[A], b: Variable[B]): Monad[Variable[C]] = new Monad[Variable[C]]

    def asM: Monad[(Variable[A], Variable[B], Variable[C])] = new Monad[(Variable[A], Variable[B], Variable[C])]
  }

  class Variable[V]()

  class Monad[A] {
    def flatMap[B](f: A => Monad[B]): Monad[B] = new Monad[B]

    def map[B](f: A => B): Monad[B] = new Monad[B]

    def withFilter(p: A => Boolean): Monad[A] = this
  }

}
