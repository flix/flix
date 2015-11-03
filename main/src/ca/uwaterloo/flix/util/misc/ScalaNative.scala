package ca.uwaterloo.flix.util.misc

object ScalaNative {
  // Note that this value is compiled as a static method
  val strTuple2 = ("foo", "bar")

  def mkTuple(t1: Int, t2: Int) = (t1, t2)
  def incrementTuple(tpl: (Int, Int)) = (tpl._1 + 1, tpl._2 + 1)
}
