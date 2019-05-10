package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, NamespaceInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

import scala.reflect.runtime.universe._


class Context(ns : Set[NamespaceInfo])(implicit root: Root, flix : Flix) extends MnemonicsClass {


  //Setup
  private val ct: Reference = getContextClassType
  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  private val continuation : Field[Ref[MObject]] = cg.mkField("continuation")

  // Adding field for each namespace
  for (namespace <- ns) {
    // JvmType of the namespace
    val namespaceRef = getNamespaceClassType(namespace)

    // Name of the field for the `namespace` on the Context object
    val fieldName = getNamespaceFieldNameInContextClass(namespace)

    // Adding the field
    cg.mkUncheckedField(fieldName, namespaceRef)
  }

  def getUncheckedField(namespace: NamespaceInfo): UncheckedField ={
    // JvmType of the namespace
    val namespaceRef = getNamespaceClassType(namespace)

    // Name of the field for the `namespace` on the Context object
    val fieldName = getNamespaceFieldNameInContextClass(namespace)
    new UncheckedField(fieldName, namespaceRef)
  }

  val defaultConstrutor : VoidMethod1[Ref[Context]] = {

    val initNamespaces =
      (sig : FunSig1[Ref[Context], MVoid])  =>{
        var ins = NO_OP[StackNil]
        for (namespace <- ns) {
          val namespaceRef = getNamespaceClassType(namespace)
          val namespaceConstructor = new VoidMethod1[Ref[MObject]](JvmModifier.InvokeSpecial, namespaceRef, "<init>" )
          ins = ins |>>
            sig.getArg1.LOAD |>>
            NEW(namespaceRef) |>>
            DUP |>>
            namespaceConstructor.INVOKE |>>
            getUncheckedField(namespace).PUT_FIELD
            }
        ins
      }

    cg.mkConstructor1(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          initNamespaces(sig) |>>
          RETURN_VOID
    )
  }

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

