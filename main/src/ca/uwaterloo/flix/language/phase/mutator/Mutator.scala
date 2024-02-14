package ca.uwaterloo.flix.language.phase.mutator

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root}
import ca.uwaterloo.flix.language.phase.mutator.binary.BinaryMutator
import ca.uwaterloo.flix.language.phase.mutator.constants.{BooleanCstMutator, Decrementer, Incrementer}
import ca.uwaterloo.flix.language.phase.mutator.unary.UnaryMutator
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

/*
* It's temporary here
*
* Set of mutators iteratively mutates source program and run tests on resulting mutant.
* Each expression can be mutated in several ways.
*
* Mutators:
*  Numbers (for Int and Float)
*      | x | -> | x + 1 |   (Cst)
*      | x | -> | x - 1 |   (Cst)
*
*           Booleans
*       | F  | -> | T  |    (Cst) Not sure need it
*       | T  | -> | F  |    (Cst) Not sure need it
*       | && | -> | || |
*       | || | -> | && |
*       | !  | -> |    |    (Unary)
*
*          Arithmetic
*        | + | -> | - |
*        | - | -> | + |
*        | * | -> | / |
*        | / | -> | * |
*        | % | -> | * |
*
*           Bitwise
*       | &  | -> | |  |
*       | |  | -> | &  |
*       | ^  | -> | &  |
*       | >> | -> | << |
*       | << | -> | >> |
*       | ~  | -> |    |    (Unary)
*
*      Conditional negate
*       | == | -> | != |
*       | != | -> | == |
*       | >  | -> | <= |
*       | <  | -> | >= |
*       | >= | -> | <  |
*       | <= | -> | >  |
*
*      Conditional boundary
*       | >  | -> | >= |
*       | <  | -> | <= |
*       | >= | -> | >  |
*       | <= | -> | <  |
*
* TODO: mutators for assignments: += -= *= ... and others can be added later
*
* Negative numbers do not need to be mutated due to the existence of incrementer and decrementer.
*/
object Mutator {
  private val mutators: List[ExprMutator] = List(
    Incrementer,
    Decrementer,
    BooleanCstMutator,
    UnaryMutator,
    BinaryMutator,
  )

  // TODO: refactor logic to run mutants iteratively and not collect all of them
  def run(root: Root)(implicit flix: Flix): List[Validation[Root, CompilationMessage]] = flix.phase("Mutator") {
    root.defs.values.toList.flatMap { defn =>
      mutateDef(defn).map { mutatedDef =>
        root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef)).toSuccess
      }
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
