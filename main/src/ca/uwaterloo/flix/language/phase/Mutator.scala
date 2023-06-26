package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Mutator {

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Mutator") {

    // Pick a random def
    val pickedDef = random(root.defs.values.toList)

    // Mutate the def.
    val mutatedDef = mutateDef(pickedDef)

    // Rebuild the AST and return it.
    root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef)).toSuccess
  }

  private def mutateDef(defn: Def): Def = {
    // We mutate the expression and rebuild in the def.
    val mutatedExp = mutateExpr(defn.impl.exp)

    defn.copy(impl = defn.impl.copy(exp = mutatedExp))
  }

  private def mutateExpr(exp: Expression): Expression = ??? // TODO recurse on the expr, find a random node, and mutate it.

  private def random[A](l: List[A]): A = ??? // TODO

}
