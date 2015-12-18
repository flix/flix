package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type.Lambda

package object library {

  /////////////////////////////////////////////////////////////////////////////
  // Mini Type DSL                                                           //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Careful with occurs check. Probably need a Type.ForAll(X, Type)

  implicit class RichType(thiz: Type) {
    def ~>(that: Type): Type = Lambda(List(thiz), that)
  }

  implicit class RichTuple2(thiz: (Type, Type)) {
    def ~>(that: Type): Type = Lambda(List(thiz._1, thiz._2), that)
  }

  implicit class RichTuple3(thiz: (Type, Type, Type)) {
    def ~>(that: Type): Type = Lambda(List(thiz._1, thiz._2, thiz._3), that)
  }

  implicit def tuple2type(tuple: (Type, Type)): Type = Type.Tuple(List(tuple._1, tuple._2))

}
