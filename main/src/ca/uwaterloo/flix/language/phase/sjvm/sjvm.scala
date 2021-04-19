package ca.uwaterloo.flix.language.phase

package object sjvm {


  val boolName = "Z"
  val boolDescriptor = "Z"
  val int8Name = "B"
  val int8Descriptor = "B"
  val int16Name = "S"
  val int16Descriptor = "S"
  val int32Name = "I"
  val int32Descriptor = "I"
  val int64Name = "J"
  val int64Descriptor = "J"
  val charName = "C"
  val charDescriptor = "C"
  val float32Name = "F"
  val float32Descriptor = "F"
  val float64Name = "D"
  val float64Descriptor = "D"
  val objectName = "java/lang/Object"
  val objectDescriptor = s"L$objectName;"

  val unitName = "flix/runtime/value/Unit"
  val unitDescriptor = s"L$unitName;"
  val stringName = "Ljava/lang/String"
  val stringDescriptor = s"L$stringName;"
}
