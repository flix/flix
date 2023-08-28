package ca.uwaterloo.flix.language.phase.jvm


/**
  * Represents all Flix types that are not object on the JVM including Void.
  */
trait VoidableType {
  /**
    * Returns a descriptor for the type. `Void` has descriptor `"V"`.
    */
  def toDescriptor: String
}

object VoidableType {
  case object Void extends VoidableType {
    override val toDescriptor: String = "V"

    /**
      * The erased string representation used in JVM names.
      */
    val toErasedString: String = "Void"
  }
}
