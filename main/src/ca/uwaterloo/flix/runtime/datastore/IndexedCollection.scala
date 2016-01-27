package ca.uwaterloo.flix.runtime.datastore

// TODO: Consider index expression:
// http://www.postgresql.org/docs/9.1/static/indexes-expressional.html
// and partial indexes:
// http://www.postgresql.org/docs/9.1/static/indexes-partial.html

trait IndexedCollection[ValueType] {

  def lookup(pat: Array[ValueType]): Iterator[Array[ValueType]]

}
