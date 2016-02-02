package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{TypedAst, Name}
import ca.uwaterloo.flix.language.ast.TypedAst.Directive
import ca.uwaterloo.flix.util.AsciiTable

/**
  * A case class representing the minimal model.
  *
  * @param constants the constant functions in the model.
  * @param relations the relational facts in the model.
  * @param lattices  the lattice facts in the model.
  */
case class Model(root: TypedAst.Root, // TODO: remove
                 constants: Map[Name.Resolved, AnyRef],
                 relations: Map[Name.Resolved, Iterable[List[AnyRef]]],
                 lattices: Map[Name.Resolved, Iterable[(List[AnyRef], List[AnyRef])]]) {

}