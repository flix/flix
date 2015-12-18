package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._
import ca.uwaterloo.flix.runtime.Value

object FList {

  /**
    * A common super-type for all list operations.
    */
  sealed trait ListOperator

  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Evaluation                                                              //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Move
  def eval(f: ListOperator, args: Array[Value]): Value = f match {
    case IsNil => Value.mkBool(args(0).asInstanceOf[List[Value]].isEmpty)
    case Length => Value.mkInt(args(0).asInstanceOf[List[Value]].length)

    case And => Value.mkBool(args(0).asInstanceOf[List[Value]].foldLeft(true) {
      case (acc, x) => acc && x.asInstanceOf[Value.Bool].b
    })

  }

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `isNil : List[A] => Bool` function.
    */
  object IsNil extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  /**
    * The `head : List[A] => A` function.
    */
  object Head extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * The `tail : List[A] => List[A]` function.
    */
  object Tail extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  /**
    * The `memberOf : (A, List[A]) => Bool` function.
    */
  object MemberOf extends ListOperator {
    val tpe = (A, Lst(A)) ~> Bool
  }

  /**
    * The `find : (List[A], A => Bool) => Opt[A]` function.
    */
  object Find extends ListOperator {
    val tpe = (Lst(A), A ~> Bool) ~> Opt(A)
  }

  /**
    * The `length : List[A] => Int` function.
    */
  object Length extends ListOperator {
    val tpe = Lst(A) ~> Int
  }

  /**
    * The `append : (List[A], List[A]) => List[A]` function.
    */
  object Append extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
  }

  /**
    * The `isPrefixOf : (List[A], List[A]) => Bool` function.
    */
  object IsPrefixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /**
    * The `isInfixOf : (List[A], List[A]) => Bool` function.
    */
  object isInfixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /**
    * The `isSuffixOf : (List[A], List[A]) => Bool` function.
    */
  object IsSuffixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Transformations                                                         //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (List[A], A => B) => List[B]` function.
    */
  object Map extends ListOperator {
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
    val tpe = (Lst(A), A ~> Bool) ~> Bool
  }

  /**
    * The `forall : (List[A], A => Bool) => Bool` function.
    */
  object Forall {
    val tpe = (Lst(A), A ~> Bool) ~> Bool
  }

  /**
    * The `and : List[Bool] => Bool` function.
    */
  object And extends ListOperator {
    val tpe = Lst(Bool) ~> Bool
  }

  /**
    * The `or : List[Bool] => Bool` function.
    */
  object Or {
    val tpe = Lst(Bool) ~> Bool
  }

  /**
    * The `reduceLeft : (List[A], (A, A) => A) => A` function.
    */
  object ReduceLeft {
    val tpe = (Lst(A), (A, A) ~> A) ~> A
  }

  /**
    * The `reduceRight : (List[A], (A, A) => A) => A` function.
    */
  object ReduceRight {
    val tpe = (Lst(A), (A, A) ~> A) ~> A
  }

  /////////////////////////////////////////////////////////////////////////////
  // Sub Lists                                                               //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `filter : (List[A], A => Bool) => List[A]` function.
    */
  object Filter {
    val tpe = (Lst(A), A ~> Bool) ~> Lst(A)
  }

  /**
    * The `take : (List[A], Int) => List[A]` function.
    */
  object Take {
    val tpe = (Lst(A), Int) ~> Lst(A)
  }

  /**
    * The `takeWhile : (List[A], A => Bool) => List[A]` function.
    */
  object TakeWhile {
    val tpe = (Lst(A), A ~> Bool) ~> Lst(A)
  }

  /**
    * The `drop : (List[A], Int) => List[A]` function.
    */
  object Drop {
    val tpe = (Lst(A), Int) ~> Lst(A)
  }

  /**
    * The `dropWhile : (List[A], A => Bool) => List[A]` function.
    */
  object DropWhile {
    val tpe = (Lst(A), A ~> Bool) ~> Lst(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Zipping                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `zip : (List[A], List[B]) => List[(A, B)]` function.
    */
  object Zip {
    val tpe = (Lst(A), Lst(B)) ~> Lst((A, B))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `toMap : List[(A, B)] => Map[A, B]` function.
    */
  object ToMap {
    val tpe = Lst((A, B)) ~> Type.Map(A, B)
  }

  /**
    * The `toSet : List[A] => Set[A]` function.
    */
  object ToSet {
    val tpe = Lst(A) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Grouping                                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `groupBy : (List[A], (A, A) => Bool) => List[List[A]]` function.
    */
  object GroupBy {
    val tpe = (Lst(A), (A, A) ~> Bool) ~> Lst(Lst(A))
  }

}
