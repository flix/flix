package ca.uwaterloo.flix.language.phase.njvm.interfaces


import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, MObject, MString, Method1, Method2, MnemonicsClass, MnemonicsTypes, VoidMethod2, getTupleInterfaceType}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType

class TupleInterface(map : Map[JvmName, MnemonicsClass], elms : List[NJvmType])(implicit root: Root, flix : Flix) extends MnemonicsClass {
  //Setup
  private val it: Reference = getTupleInterfaceType(elms)
  private val ig: InterfaceGenerator = new InterfaceGenerator(it, List())

  //Fields
  //Interface has no fields ig doesn't even allow to compile fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generate the getIndex interface method. Stores the capability to call the method
    */
  val getIndexMethod: List[Method1[Ref[TupleInterface], MnemonicsTypes]] = {
    val methods = List()
    for ((arg, ind) <- elms.zipWithIndex) {
      methods :+ (arg match {
        case PrimBool => ig.mkMethod1[Ref[TupleInterface], MBool]("getIndex" + ind)
        case PrimChar => ig.mkMethod1[Ref[TupleInterface], MChar]("getIndex" + ind)
        case PrimByte => ig.mkMethod1[Ref[TupleInterface], MByte]("getIndex" + ind)
        case PrimShort => ig.mkMethod1[Ref[TupleInterface], MShort]("getIndex" + ind)
        case PrimInt => ig.mkMethod1[Ref[TupleInterface], MInt]("getIndex" + ind)
        case PrimLong => ig.mkMethod1[Ref[TupleInterface], MLong]("getIndex" + ind)
        case PrimFloat => ig.mkMethod1[Ref[TupleInterface], MFloat]("getIndex" + ind)
        case PrimDouble => ig.mkMethod1[Ref[TupleInterface], MDouble]("getIndex" + ind)
        case Reference(_) => ig.mkMethod1[Ref[TupleInterface], Ref[MObject]]("getIndex" + ind)
        case _ => ???
     })
    }
    methods
  }

  /**
    * Generate the setIndex interface method. Stores the capability to call the method
    */
  val setIndexMethod: List[VoidMethod2[Ref[TupleInterface], MnemonicsTypes]] = {
    val methods = List()
    for ((arg, ind) <- elms.zipWithIndex) {
      methods :+ (arg match {
        case PrimBool => ig.mkVoidMethod2[Ref[TupleInterface], MBool]("setIndex" + ind)
        case PrimChar => ig.mkVoidMethod2[Ref[TupleInterface], MChar]("setIndex" + ind)
        case PrimByte => ig.mkVoidMethod2[Ref[TupleInterface], MByte]("setIndex" + ind)
        case PrimShort => ig.mkVoidMethod2[Ref[TupleInterface], MShort]("setIndex" + ind)
        case PrimInt => ig.mkVoidMethod2[Ref[TupleInterface], MInt]("setIndex" + ind)
        case PrimLong => ig.mkVoidMethod2[Ref[TupleInterface], MLong]("setIndex" + ind)
        case PrimFloat => ig.mkVoidMethod2[Ref[TupleInterface], MFloat]("setIndex" + ind)
        case PrimDouble => ig.mkVoidMethod2[Ref[TupleInterface], MDouble]("setIndex" + ind)
        case Reference(_) => ig.mkVoidMethod2[Ref[TupleInterface], Ref[MObject]]("setIndex" + ind)
        case _ => ???
      })

    }
    methods
  }


  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(it.name, ig.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    it.name -> this
}
