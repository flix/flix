package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._
import ca.uwaterloo.flix.runtime.Value

object FList {

  type ListType = scala.collection.immutable.List

  sealed trait ListOperator

  // TODO: Careful with occurs check. Probably need a Type.ForAll(X, Type)

  /////////////////////////////////////////////////////////////////////////////
  // Evaluation                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def eval(f: ListOperator, args: Array[Value]): Value = f match {
    case IsEmpty => if (args(0).asInstanceOf[ListType].isEmpty) Value.True else Value.False
    case Length => Value.mkInt(args(0).asInstanceOf[ListType].length)

  }

  /////////////////////////////////////////////////////////////////////////////
  // Mini Type DSL                                                           //
  /////////////////////////////////////////////////////////////////////////////
  val A = Type.Var("A")
  val B = Type.Var("A")

  implicit class RichType(thiz: Type) {
    def ~>(that: Type): Type = Lambda(List(thiz), that)
  }

  implicit class RichTuple2(thiz: (Type, Type)) {
    def ~>(that: Type): Type = Lambda(List(thiz._1, thiz._2), that)
  }

  implicit class RichTuple3(thiz: (Type, Type, Type)) {
    def ~>(that: Type): Type = Lambda(List(thiz._1, thiz._2, thiz._3), that)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Queries                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `isEmpty : List[A] => Bool` function.
    */
  object IsEmpty extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  /**
    * The `length : List[A] => Int` function.
    */
  object Length extends ListOperator {
    val tpe = Lst(A) ~> Int
  }

  // TODO: cons, head, tail

  /////////////////////////////////////////////////////////////////////////////
  // Transformations                                                         //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (List[A], A => B) => List[B]` function.
    */
  object Map {
    val tpe = (Lst(A), A ~> B) ~> Lst(B)
  }

  /**
    * The `flatMap : (List[A], A => List[B]) => List[B]` function.
    */
  object FlatMap {
    val tpe = (Lst(A), A ~> Lst(B)) ~> Lst(B)
  }

  /**
    * The `reverse : List[A] => List[A]` function.
    */
  object Reverse {
    val tpe = Lst(A) ~> Lst(A)
  }

  /**
    * The `filter : (List[A], A => Bool) => List[A]` function.
    */
  object Filter {
    val tpe = (Lst(A), A ~> Bool) ~> Lst(A)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Folds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `foldLeft : (List[A], B, (B, A) => B) => B` function.
    */
  object FoldLeft {
    val tpe = (Lst(A), B, (B, A) ~> B) ~> B
  }

  /**
    * The `foldRight : (List[A], B, (B, A) => B) => B` function.
    */
  object FoldRight {
    val tpe = (Lst(A), B, (A, B) ~> B) ~> B
  }

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
