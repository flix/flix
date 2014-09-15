package util.ascii

sealed trait Align;

object Align {
  case object Left extends Align;
  case object Middle extends Align;
  case object Right extends Align;
}

