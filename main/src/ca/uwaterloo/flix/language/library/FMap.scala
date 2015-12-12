package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

object FMap {




  // TODO: Map
  // - getOrElse
  // - ++ (merge)
  // - removeKey
  // - foldValues
  // - isEmpty
  // - filterKeys/filterValues/exists/find/fold
  // - forall
  // - groupBy
  // - size
  // - member/notMembet
  // - lookup
  // - empty
  // - singleton
  // - insert
  // - delete
  // - update
  // - delete
  // - leftUnion
  // - difference
  // - intersection?
  // - map
  // - mapWithKey
  // - mapKeys
  // foldLeft/foldRigyht, foldLeftWithKey, foldRightWithKey
  // - elms
  // - keys
  // - toList
  // - fromList
  // filter/filterWithKey
  // paritition, partitionWithKey
  // isSubmapOf
  // isProperSubmapOf


  /**
    * ...
    * isEmpty : Set[A] => Bool
    */
  case class IsEmpty(loc: SourceLocation) {

  }



  case class ToList(loc: SourceLocation) {
    val tpe = TypedAst.Type.Lambda(???, ???)
  }

}
