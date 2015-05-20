package examples.scala

import api.Flix
import api.FlixImplicits._
import impl.logic.Value

object Graph01 extends App {
  val flix = new Flix

  def id(v: Value): Value = "foo"

  flix += ("id" -> id _)
  flix += """
    (def-type Edge (-> Str (Set Str)))
    (def-type Reachable (-> Str (Set Str)))
    (def-type Cycle (Set Str))

    (fact (Edge "a" {(id "b")}))
    (fact (Edge "b" {"a"}))

    (rule (Reachable x {y}) ((Edge x {y})))
    (rule (Reachable x {z}) ((Reachable x {y}) (Reachable y {z})))

    (rule (Cycle {x}) ((Reachable x {x})))
  """

  flix.solve()
  flix.print()
}
