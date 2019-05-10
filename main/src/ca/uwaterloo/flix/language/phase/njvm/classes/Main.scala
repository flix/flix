package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, JvmClass, JvmName, JvmOps, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import org.objectweb.asm.Opcodes.{ACONST_NULL, INVOKESTATIC, RETURN}

import scala.reflect.runtime.universe._


class Main[T <: MnemonicsTypes : TypeTag](map : Map[JvmName, MnemonicsClass])(implicit root: Root, flix : Flix) extends MnemonicsClass {


  //Setup
  private val ct: Reference = getMainClassType
  private val cg: ClassGenerator = new ClassGenerator(ct, List())


  val mainMethod : VoidMethod1[MArray[MString]] = {

    //Get the root namespace in order to get the class type when invoking m_main
    val ns = JvmOps.getNamespace(Symbol.mkDefnSym("main"))

    val m_main_method = new Method1[Ref[MObject], T](JvmModifier.InvokeStatic, getNamespaceClassType(ns), "m_main" )

    cg.mkStaticVoidMethod1("main",
      _ =>
        CONST_NULL[StackNil, Ref[MObject]] |>>
        m_main_method.INVOKE |>>
        RETURN_VOID[T]
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

