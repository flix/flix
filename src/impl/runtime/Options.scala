package impl.runtime

case class Options(propagation: Propagation, simplify: Simplify)

/**
 * An option which controls whether values should be propagated before or after the join.
 *
 * For instance, assume that v1 join v2 = v3, and v2 is the new value.
 *
 * With the diff strategy only v2 is propagated. With full propagation v3 is propagated.
 *
 * In the case of set values this corresponds to what is sometimes called difference propagation.
 */
sealed trait Propagation
case object Propagation {
  case object Diff extends Propagation
  case object Full extends Propagation
}

/**
 * An option which controls whether horn clauses should be simplified before being added to the worklist.
 *
 * In particular, given the horn clause h: P(x) <= A(x), B(y), C(z), if (for some reason), C(z) is known
 * to hold, then only the formula P(x) <= A(x), B(y) will be added to the worklist when simplification is
 * enabled. Otherwise the whole formula P(x) <= A(x), B(y), C(z) is added.
 */
sealed trait Simplify
object Simplify {
  case object Enable extends Simplify
  case object Disable extends Simplify
}