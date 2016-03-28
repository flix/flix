package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}

/**
  * A class representing the minimal model.
  *
  * @param root        the abstract syntax tree of the program.
  * @param definitions the definitions in the program.
  * @param relations   the relational facts in the model.
  * @param lattices    the lattice facts in the model.
  */
class Model(root: ExecutableAst.Root,
            definitions: Map[Symbol.Resolved, () => AnyRef],
            relations: Map[Symbol.TableSym, Iterable[List[AnyRef]]],
            lattices: Map[Symbol.TableSym, Iterable[(List[AnyRef], List[AnyRef])]]) {

  def getRoot: ExecutableAst.Root = root

  def getConstant(sym: Symbol.Resolved): AnyRef = definitions(sym)()

  def getConstant(name: String): AnyRef = getConstant(Symbol.Resolved.mk(name))

  def getRelation(name: String): Iterable[List[AnyRef]] =
    getRelationOpt(name).get

  def getRelationOpt(name: String): Option[Iterable[List[AnyRef]]] =
    relations.get(Symbol.mkTableSym(name))

  def getLattice(name: String): Iterable[(List[AnyRef], List[AnyRef])] =
    getLatticeOpt(name).get

  def getLatticeOpt(name: String): Option[Iterable[(List[AnyRef], List[AnyRef])]] =
    lattices.get(Symbol.mkTableSym(name))

}