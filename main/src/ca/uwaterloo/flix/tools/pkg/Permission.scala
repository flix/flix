package ca.uwaterloo.flix.tools.pkg

sealed trait Permission

object Permission {

  case object JavaInterop extends Permission

  case object UnsafeCast extends Permission

  case object Effect extends Permission

  def ofString(s: String): Option[Permission] = s match {
    case "java-interop" => Some(JavaInterop)
    case "unsafe-cast"  => Some(UnsafeCast)
    case "effect"       => Some(Effect)
    case _              => None
  }
}
