package util

trait Relation[K, V] {
  def get(i: Int, k: K): Relation[K, V]
  def tuples: Set[V]
}
