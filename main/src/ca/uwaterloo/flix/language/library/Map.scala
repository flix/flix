package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

object Map {

  /**
    * ...
    * isEmpty : Set[A] => Bool
    */
  case class IsEmpty(loc: SourceLocation) {

  }



  case class ToList(loc: SourceLocation) {
    val tpe = TypedAst.Type.Lambda(List(), ???)
  }

}
