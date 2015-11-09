package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.runtime.Value

// TODO: Consider index expression:
// http://www.postgresql.org/docs/9.1/static/indexes-expressional.html
// and partial indexes:
// http://www.postgresql.org/docs/9.1/static/indexes-partial.html

trait IndexedCollection {

  def lookup(pat: Array[Value]): Iterator[Array[Value]]

}
