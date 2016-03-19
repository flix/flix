//
// def f: Vector[Int] = #[1, 2, 3]
//
// def f(xs: Vector[Int]): Int = xs[5] // indexing
//
// def f(xs: Vector[Int]): Int = match xs with {
//   case #[] => // empty vector
//   case #[1, 2, 3] // vector with three elements
//   case #[1, 2, rs...] // ...
// }
//
// def f(xs: Vector[Int]): Int = match xs with {
//   case x +: xs => // this is what spiewak wanted???
// }
//
//
//// I have ALWAYS wanted these
//object +: {
//  def unapply[A](vec: Vector[A]): Option[(A, Vector[A])] = ???
//}
//
//object :+ {
//  def unapply[A](vec: Vector[A]): Option[(Vector[A], A)] = ???
//}
