package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.DeadCodeError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

object DeadCode extends Phase[Root, Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, DeadCodeError] = flix.phase("DeadCode") {

    val newDefs = traverse(root.defs)  {
      case (sym, defn) => visitDef(defn).map(x => sym -> x)
    }

    mapN(newDefs) {
      case defs => root.copy(defs = defs.toMap)
    }
  }

  private def visitDef(defn: TypedAst.Def): Validation[TypedAst.Def, DeadCodeError] =
    if (defn.sym.name.contains("foo"))  {
      DeadCodeError(defn.loc).toFailure // Validation.Failure(..)
    } else  {
      defn.toSuccess // Validation.Success(...)
    }


}
