package ca.uwaterloo.flix.api

/**
  * Public interface for Flix models.
  */
trait IModel {



  def getRelation(name: String): java.lang.Iterable[Array[IValue]]

  def getLattice(name: String): java.lang.Iterable[Array[IValue]]


  // def isFact()
  // def isEDBFact()
  // def isIDBFact()



  //  root: TypedAst.Root,
  //  constants: Map[Name.Resolved, Value],
  //  relations: Map[Name.Resolved, Iterator[List[Value]]], // TODO: Replace with Iterable.
  //  lattices: Map[Name.Resolved, Iterator[(List[Value], List[Value])]]

}
