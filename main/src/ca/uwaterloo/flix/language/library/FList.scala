package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.TypedAst.Type

object FList {

  // TODO: Carefull with occurs check. Probably need a Type.Apply[]

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

  /////////////////////////////////////////////////////////////////////////////
  // Transformations                                                         //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (List[A], A => B) => Bool` function.
    */
  object Map {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A")), Type.Lambda(List(Type.Var("A")), Type.Bool)), Type.Bool)
  }

  // flatMap


  /**
    * The `reverse : List[A] => List[A]` function.
    */
  object Reverse {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A"))), Type.Lst(Type.Var("A")))
  }


  /////////////////////////////////////////////////////////////////////////////
  // Folds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // fold
  // foldLeft
  // foldRight

  /////////////////////////////////////////////////////////////////////////////
  // Special Folds                                                           //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `exists : (List[A], A => Bool) => Bool` function.
    */
  object Exists {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A")), Type.Lambda(List(Type.Var("A")), Type.Bool)), Type.Bool)
  }

  /**
    * The `forall : (List[A], A => Bool) => Bool` function.
    */
  object Forall {
    val tpe = Type.Lambda(List(Type.Lst(Type.Var("A")), Type.Lambda(List(Type.Var("A")), Type.Bool)), Type.Bool)
  }

  /**
    * The `all : List[Bool] => Bool` function.
    */
  object All {
    val tpe = Type.Lambda(List(Type.Lst(Type.Bool)), Type.Bool)
  }

  /**
    * The `any : List[Bool] => Bool` function.
    */
  object Any {
    val tpe = Type.Lambda(List(Type.Lst(Type.Bool)), Type.Bool)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Sub Lists                                                               //
  /////////////////////////////////////////////////////////////////////////////
  // take
  // drop
  // takeWhile
  // dropWhile
  // filter



  // TODO: List
  // map
  //
  // -+ ,++
  // count
  // drop
  // dropRight
  // dropWhile

  // reduce
  // head
  // tail
  // toMap
  // splitAt
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
