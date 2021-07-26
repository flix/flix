package ca.uwaterloo.flix.util.collection

// MATT license
// MATT docs
object ListOps {
  def unzip4[A, B, C, D](list: List[(A, B, C, D)]): (List[A], List[B], List[C], List[D]) = list match {
    case Nil => (Nil, Nil, Nil, Nil)
    case (a, b, c, d) :: tail =>
      val (listA, listB, listC, listD) = unzip4(tail)
      (a :: listA, b :: listB, c :: listC, d :: listD)
  }
}
