package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FOpt {

  /**
    * A common super-type for all option operations.
    */
  sealed trait OptOperator extends LibraryOperator

  /**
    * All opt operations.
    */
  val Ops: immutable.Map[Name.Resolved, OptOperator] = List(
    "Opt/null" -> nul,
    "Opt/get" -> get,
    "Opt/getWithDefault" -> getWithDefault,
    "Opt/exists" -> exists,
    "Opt/forall" -> forall,
    "Opt/filter" -> filter,
    "Opt/map" -> map,
    "Opt/map2" -> map,
    "Opt/flatMap" -> flatMap,
    "Opt/toList" -> toList,
    "Opt/toSet" -> toSet,
    "Opt/withDefault" -> withDefault
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")
  val C = Type.Var("C")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends OptOperator {
    val tpe = Type.Abs(A, Opt(A) ~> Bool)
  }

  object get extends OptOperator {
    val tpe = Opt(A) ~> A
  }

  object getWithDefault extends OptOperator {
    val tpe = (Opt(A), A) ~> A
  }

  object exists extends OptOperator {
    val tpe = (A ~> Bool, Opt(A)) ~> Bool
  }

  object forall extends OptOperator {
    val tpe = (A ~> Bool, Opt(A)) ~> Bool
  }

  object filter extends OptOperator {
    val tpe = (A ~> Bool, Opt(A)) ~> Opt(A)
  }

  object map extends OptOperator {
    val tpe = (A ~> B, Opt(A)) ~> Opt(B)
  }

  object map2 extends OptOperator {
    val tpe = ((A, B) ~> C, Opt(A), Opt(B)) ~> Opt(C)
  }

  object flatMap extends OptOperator {
    val tpe = (A ~> Opt(B), Opt(A)) ~> Opt(B)
  }

  object flatMap2 extends OptOperator {
    val tpe = ((A, B) ~> Opt(C), Opt(A), Opt(B)) ~> Opt(C)
  }

  object withDefault extends OptOperator {
    val tpe = (Opt(A), Opt(A)) ~> Opt(A)
  }

  object toList extends OptOperator {
    val tpe = Opt(A) ~> Lst(A)
  }

  object toSet extends OptOperator {
    val tpe = Opt(A) ~> Set(A)
  }

}
