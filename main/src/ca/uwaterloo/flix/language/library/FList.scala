package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.TypedAst.Type

object FList {

  /////////////////////////////////////////////////////////////////////////////
  // Queries                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `isEmpty : List[A] => Bool` function.
    */
  object IsEmpty {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A"))), Type.Bool)
  }

  /**
    * The `length : List[A] => Int` function.
    */
  object Length {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A"))), Type.Int)
  }

  // TODO: cons, head, tail

  /**
    * The `reverse : List[A] => List[A]` function.
    */
  object Reverse {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A"))), Type.Lst(Type.Var("A")))
  }

  // TODO: List
  // map
  //
  // -+ ,++
  // count
  // drop
  // dropRight
  // dropWhile
  // exists
  // filter
  // filterNot
  // fold
  // foldLeft
  // foldRight
  // reduce
  // head
  // tail
  // map
  // flatMap
  // all, every
  // toMap
  // take
  // drop
  // splitAt
  // takeWhile
  // dropWhile
  // isPrefixOf
  // isSuffixOf
  // in/elm/has
  // find
  // findIndex
  // zip
  // unzip

  // deleteBy
  // groupBy

}
