package ca.uwaterloo.flix.language.phase.mutator

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root}
import ca.uwaterloo.flix.language.phase.mutator.boolean.{BoolBinaryMutator, BoolCstMutator, BoolUnaryMutator}
import ca.uwaterloo.flix.language.phase.mutator.numbers.{Decrementer, Incrementer}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

object Mutator {
  private val mutators: List[ExprMutator] = List(
    Incrementer,
    Decrementer,
    BoolCstMutator,
    BoolUnaryMutator,
    BoolBinaryMutator,
  )

  def run(root: Root)(implicit flix: Flix): List[Validation[Root, CompilationMessage]] = flix.phase("Mutator") {
    root.defs.values.toList.map { defn =>
      val mutatedDefs = mutateDef(defn)

      root.copy(defs = root.defs + mutatedDefs.map(mutatedDef => mutatedDef.sym -> mutatedDef)).toSuccess
    }
  }

  private def mutateDef(defn: Def): List[Def] = {
    val exp = defn.impl.exp

    mutators
      .flatMap(_.mutateExpr(exp))
      .map { mutatedExp =>
        defn.copy(impl = defn.impl.copy(exp = mutatedExp))
      }
  }
}
