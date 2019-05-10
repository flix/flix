package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, ClosureInfo, JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

import scala.reflect.runtime.universe._


class Closure(closure : ClosureInfo)(implicit root: Root, flix : Flix) extends MnemonicsClass {

  private val MonoType.Arrow(targs, tresult) = closure.tpe
  private val elms = targs.map(getErasedJvmType)
  private val returnType = getErasedJvmType(tresult)
  private val cloFieldsTypes = closure.freeVars.map(freeVar => getErasedJvmType(freeVar.tpe))
  //Setup
  private val ct: Reference = getClosureClassType(closure)
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getFunctionInterfaceType(elms, returnType), NJvmType.Spawnable))


  private val creationContext : Field[Ref[MObject]] = cg.mkField("creationContext")

  //Fields
  for ((arg, ind) <- cloFieldsTypes.zipWithIndex) {
    arg match {
      case PrimBool => cg.mkPrimField[MBool]("clo" + ind)
      case PrimChar => cg.mkPrimField[MChar]("clo" + ind)
      case PrimByte => cg.mkPrimField[MByte]("clo" + ind)
      case PrimShort => cg.mkPrimField[MShort]("clo" + ind)
      case PrimInt => cg.mkPrimField[MInt]("clo" + ind)
      case PrimLong => cg.mkPrimField[MLong]("clo" + ind)
      case PrimFloat => cg.mkPrimField[MFloat]("clo" + ind)
      case PrimDouble => cg.mkPrimField[MDouble]("clo" + ind)
      case Reference(_) => cg.mkField[Ref[MObject]]("clo" + ind)
      case _ => ???
    }
  }

  private def getCloField[T1 <: MnemonicsTypes : TypeTag](ind : Int) : Field[T1] =
    new Field[T1]("clo" + ind)

  private def getCloPrimField[T1 <: MnemonicsPrimTypes : TypeTag](ind : Int) : PrimField[T1] =
    new PrimField[T1]("clo" + ind)


  for ((arg, ind) <- elms.zipWithIndex) {
    arg match {
      case PrimBool =>
        val field = cg.mkPrimField[MBool]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MBool]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
             sig.getArg2.LOAD |>>
             field.PUT_FIELD |>>
             RETURN_VOID
        )
      case PrimChar =>
        val field = cg.mkPrimField[MChar]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MChar]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimByte =>
        val field = cg.mkPrimField[MByte]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MByte]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimShort =>
        val field = cg.mkPrimField[MShort]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MShort]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimInt =>
        val field = cg.mkPrimField[MInt]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MInt]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimLong =>
        val field = cg.mkPrimField[MLong]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MLong]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimFloat =>
        val field = cg.mkPrimField[MFloat]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MFloat]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimDouble =>
        val field = cg.mkPrimField[MDouble]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], MDouble]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case Reference(_) =>
        val field = cg.mkField[Ref[MObject]]("arg" + ind)
        cg.mkVoidMethod2[Ref[Closure], Ref[MObject]]("setArg" + ind,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case _ => ???
    }
  }

  private def getArgField[T1 <: MnemonicsTypes : TypeTag](ind : Int) : Field[T1] =
    new Field[T1]("arg" + ind)

  private def getArgPrimField[T1 <: MnemonicsPrimTypes : TypeTag](ind : Int) : PrimField[T1] =
    new PrimField[T1]("arg" + ind)

  def setArgMethod[T1 <: MnemonicsTypes : TypeTag](ind : Int) : VoidMethod2[Ref[Closure], T1] =
    new VoidMethod2(JvmModifier.InvokeVirtual, ct,"setArg" + ind)

  returnType match {
    case PrimBool =>
      val field = cg.mkPrimField[MBool]("result")
      cg.mkMethod1[Ref[Closure], MBool]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimChar =>
      val field = cg.mkPrimField[MChar]("result")
      cg.mkMethod1[Ref[Closure], MChar]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimByte =>
      val field = cg.mkPrimField[MByte]("result")
      cg.mkMethod1[Ref[Closure], MByte]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimShort =>
      val field = cg.mkPrimField[MShort]("result")
      cg.mkMethod1[Ref[Closure], MShort]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimInt =>
      val field = cg.mkPrimField[MInt]("result")
      cg.mkMethod1[Ref[Closure], MInt]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimLong =>
      val field = cg.mkPrimField[MLong]("result")
      cg.mkMethod1[Ref[Closure], MLong]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimFloat =>
      val field = cg.mkPrimField[MFloat]("result")
      cg.mkMethod1[Ref[Closure], MFloat]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimDouble =>
      val field = cg.mkPrimField[MDouble]("result")
      cg.mkMethod1[Ref[Closure], MDouble]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case Reference(_) =>
      val field = cg.mkField[Ref[MObject]]("result")
      cg.mkMethod1[Ref[Closure], Ref[MObject]]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case _ => ???
  }

  private def getResultField[T1 <: MnemonicsTypes : TypeTag]: Field[T1] =
    new Field[T1]("result")

  private def getResultPrimField[T1 <: MnemonicsPrimTypes : TypeTag] : PrimField[T1] =
    new PrimField[T1]("result")

  private def getResultMethod[T1 <: MnemonicsPrimTypes : TypeTag] : Method1[Ref[Closure], T1] =
    new Method1(JvmModifier.InvokeVirtual,ct, "getResult")


  val defaultConstructor : UncheckedVoidMethod = {
    val setCloFields =
      (sig : UncheckedFunSig)  =>{
        var ins = NO_OP[StackNil]
        var ind: Int = 2
        for (arg <- cloFieldsTypes) {
          ins = ins |>>
            sig.getArg[Ref[Closure]](0).LOAD |>>
            (arg match {
              case PrimBool => sig.getArg[MBool](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimChar =>  sig.getArg[MChar](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimByte =>   sig.getArg[MByte](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimShort => sig.getArg[MShort](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimInt =>   sig.getArg[MInt](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimLong =>   sig.getArg[MLong](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimFloat =>  sig.getArg[MFloat](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case PrimDouble =>   sig.getArg[MDouble](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloPrimField(ind).PUT_FIELD
              case Reference(_) =>   sig.getArg[Ref[MObject]](ind).LOAD[StackNil ** Ref[Closure]] |>>
                getCloField(ind).PUT_FIELD
              case _ => ???
            })
          arg match {
            case PrimLong | PrimDouble => ind += 2
            case _ => ind += 1
          }
        }
        ins
      }

    cg.mkUncheckedConstructor(ct +: cloFieldsTypes,
      sig =>
        sig.getArg(0).asInstanceOf[Local[Ref[Closure]]].LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg(0).asInstanceOf[Local[Ref[Closure]]].LOAD |>>
          sig.getArg(1).asInstanceOf[Local[Ref[MObject]]].LOAD |>>
          creationContext.PUT_FIELD |>>
          setCloFields(sig) |>>
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

