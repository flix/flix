package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType.PUnit
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType.RReference
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

object GenUnitClass {

  val InstanceFieldName = "INSTANCE"

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val unitType = RUnit.toRType
    val unitName = unitType.jvmName
    Map() + (unitName -> JvmClass(unitName, genByteCode(unitType)))
  }

  def genByteCode(unitType: RReference[PUnit])(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(unitType.jvmName, superClass = None)
    classMaker.mkField(InstanceFieldName, unitType, Mod.isPublic.isStatic.isFinal)
    classMaker.mkStaticConstructor(genStaticConstructor(unitType))
    classMaker.mkConstructor(START[StackNil] ~ THISINIT(JvmName.Java.Object) ~ RETURN)
    classMaker.closeClassMaker
  }

  def genStaticConstructor(unitType: RReference[PUnit]): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      NEW(unitType) ~
      DUP ~
      InvokeSimpleConstructor(unitType) ~
      PUTSTATIC(unitType.jvmName, InstanceFieldName, unitType, erasedType = false) ~
      RETURN
  }

}
