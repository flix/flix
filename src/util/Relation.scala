package util

trait Relation[K, V] {
  def get(i: Int, k: K): Relation[K, V]

  def query(keys: List[Option[K]]): Set[V] = {
    var r: Relation[K, V] = this
    var i = 0
    for (q <- keys) {
      q match {
        case None => i += 1 // nop
        case Some(x) => r = r.get(i, x);
      }
    }
    r.tuples
  }

  def tuples: Set[V]
}
