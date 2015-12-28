package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type

import scala.collection.immutable

package object library {

  /**
    * A common super-type for all library operators.
    */
  trait LibraryOperator

  /**
    * A map of all library operators.
    */
  val Library: immutable.Map[Name.Resolved, LibraryOperator] =
    FDebug.Ops ++ FList.Ops ++ FMap.Ops ++ FOpt.Ops ++ FSet.Ops

  /////////////////////////////////////////////////////////////////////////////
  // Mini Type DSL                                                           //
  /////////////////////////////////////////////////////////////////////////////

  implicit class RichType(thiz: Type) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz), that)
  }

  implicit class RichTuple(thiz: Unit) {
    def ~>(that: Type): Type = Type.Lambda(List.empty[Type], that)
  }

  implicit class RichTuple2(thiz: (Type, Type)) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz._1, thiz._2), that)
  }

  implicit class RichTuple3(thiz: (Type, Type, Type)) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz._1, thiz._2, thiz._3), that)
  }

  implicit def tuple2type(tuple: (Type, Type)): Type = Type.Tuple(List(tuple._1, tuple._2))

}
