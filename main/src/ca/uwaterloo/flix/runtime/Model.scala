package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Name}

/**
  * A case class representing the minimal model.
  *
  * @param constants the constant functions in the model.
  * @param relations the relational facts in the model.
  * @param lattices  the lattice facts in the model.
  */
case class Model(root: ExecutableAst.Root, // TODO: remove
                 constants: Map[Name.Resolved, AnyRef],
                 relations: Map[Name.Resolved, Iterable[List[AnyRef]]],
                 lattices: Map[Name.Resolved, Iterable[(List[AnyRef], List[AnyRef])]]) {

}