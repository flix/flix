package ca.uwaterloo.flix.util

import java.util.concurrent.ConcurrentLinkedQueue

object SharedContext {
  /**
    * Returns a fresh shared context.
    */
  def mk[T](): SharedContext[T] = new SharedContext(new ConcurrentLinkedQueue())
}

/**
  * A global shared context. Must be thread-safe.
  *
  * @param errors the errors in the AST, if any.
  */
case class SharedContext[T](errors: ConcurrentLinkedQueue[T])
