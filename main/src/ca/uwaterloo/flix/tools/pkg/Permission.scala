package ca.uwaterloo.flix.tools.pkg

sealed trait Permission

object Permission {

  case object JavaInterop extends Permission {
    override def toString: String = "java-interop"
  }

  case object UnsafeCast extends Permission {
    override def toString: String = "unsafe-cast"
  }

  case object Effect extends Permission {
    override def toString: String = "effect"
  }

  def ofString(s: String): Option[Permission] = s match {
    case "java-interop" => Some(JavaInterop)
    case "unsafe-cast" => Some(UnsafeCast)
    case "effect" => Some(Effect)
    case _ => None
  }
}
