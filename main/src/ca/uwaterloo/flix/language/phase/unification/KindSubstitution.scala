package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.Kind

case class KindSubstitution(m: Map[Kind.Var, Kind]) {

  val isEmpty: Boolean = m.isEmpty

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: KindSubstitution): KindSubstitution = {
    // Case 1: Return `that` if `this` is empty.
    if (this.isEmpty) {
      return that
    }

    // Case 2: Return `this` if `that` is empty.
    if (that.isEmpty) {
      return this
    }

    // Case 3: Merge the two substitutions.

    // NB: Use of mutability improve performance.
    import scala.collection.mutable
    val newKindMap = mutable.Map.empty[Kind.Var, Kind]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      newKindMap.update(x, this.apply(t))
    }

    // Add all bindings in `this` that are not in `that`.
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        newKindMap.update(x, t)
      }
    }

    KindSubstitution(newKindMap.toMap) ++ this
  }

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: KindSubstitution): KindSubstitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      KindSubstitution(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  // MATT docs
  def apply(k0: Kind): Kind = {
    def visit(k: Kind): Kind = k match {
      case v: Kind.Var => m.getOrElse(v, v)
      case Kind.Star => Kind.Star
      case Kind.Bool => Kind.Bool
      case Kind.Record => Kind.Record
      case Kind.Schema =>Kind.Schema
      case Kind.Arrow(k1, k2) => Kind.Arrow(visit(k1), visit(k2))
    }
    visit(k0)
  }
}

object KindSubstitution {
  val empty: KindSubstitution = KindSubstitution(Map.empty)

  def singleton(kvar: Kind.Var, k: Kind): KindSubstitution =
  // Ensure that we do not add any x -> x mappings.
    k match {
      case kvar2: Kind.Var if kvar.id == kvar2.id => empty
      case _ => KindSubstitution(Map(kvar -> k))
    }

}
