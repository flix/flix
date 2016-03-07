package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}

/**
  * A case class representing the minimal model.
  *
  * @param constants the constant functions in the model.
  * @param relations the relational facts in the model.
  * @param lattices  the lattice facts in the model.
  */
case class Model(root: ExecutableAst.Root, // TODO: remove
                 constants: Map[Symbol.Resolved, AnyRef],
                 relations: Map[Symbol.Resolved, Iterable[List[AnyRef]]],
                 lattices: Map[Symbol.Resolved, Iterable[(List[AnyRef], List[AnyRef])]]) {

}