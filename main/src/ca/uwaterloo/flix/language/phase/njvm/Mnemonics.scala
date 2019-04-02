package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, JvmName, JvmOps, JvmType}

import scala.reflect.runtime.universe._
import org.objectweb.asm.{ClassWriter, MethodVisitor, Label}
import org.objectweb.asm.Opcodes._

object Mnemonics {

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  /**
    * Enum describing all the possible JvmModifier.
    * It includes methods, fields, interfaces, Invoker for functions etc
    *
    */
  //TODO: DO better partition of the modifiers and better naming as this one isn't 100% correct
  sealed trait JvmModifier {
    def toInternal: Int = this match {
      case JvmModifier.Public => ACC_PUBLIC
      case JvmModifier.Private => ACC_PRIVATE
      case JvmModifier.Protected => ACC_PROTECTED
      case JvmModifier.Static => ACC_STATIC
      case JvmModifier.Final => ACC_FINAL
      case JvmModifier.Super => ACC_SUPER
      case JvmModifier.Synchronized => ACC_SYNCHRONIZED
      case JvmModifier.Volatile => ACC_VOLATILE
      case JvmModifier.Bridge => ACC_BRIDGE
      case JvmModifier.VarArgs => ACC_VARARGS
      case JvmModifier.Transient => ACC_TRANSIENT
      case JvmModifier.Native => ACC_NATIVE
      case JvmModifier.Interface => ACC_INTERFACE
      case JvmModifier.Abstract => ACC_ABSTRACT
      case JvmModifier.Strict => ACC_STRICT
      case JvmModifier.Synthetic => ACC_SYNTHETIC
      case JvmModifier.Annotation => ACC_ANNOTATION
      case JvmModifier.Enum => ACC_ENUM
      case JvmModifier.Mandated => ACC_MANDATED
      case JvmModifier.Deprecated => ACC_DEPRECATED

      case JvmModifier.InvokeSpecial => INVOKESPECIAL
      case JvmModifier.InvokeVirtual => INVOKEVIRTUAL
      case JvmModifier.InvokeInterface => INVOKEINTERFACE
      case JvmModifier.InvokeStatic => INVOKESTATIC
    }
  }

  object JvmModifier {

    case object Public extends JvmModifier

    case object Private extends JvmModifier

    case object Protected extends JvmModifier

    case object Static extends JvmModifier

    case object Final extends JvmModifier

    case object Super extends JvmModifier

    case object Synchronized extends JvmModifier

    case object Volatile extends JvmModifier

    case object Bridge extends JvmModifier

    case object VarArgs extends JvmModifier

    case object Transient extends JvmModifier

    case object Native extends JvmModifier

    case object Interface extends JvmModifier

    case object Abstract extends JvmModifier

    case object Strict extends JvmModifier

    case object Synthetic extends JvmModifier

    case object Annotation extends JvmModifier

    case object Enum extends JvmModifier

    case object Mandated extends JvmModifier

    case object Deprecated extends JvmModifier

    case object InvokeSpecial extends JvmModifier

    case object InvokeVirtual extends JvmModifier

    case object InvokeInterface extends JvmModifier

    case object InvokeStatic extends JvmModifier

  }

  /**
    * Types used by the framework to ensure correcteness at compile time.
    * There are some classes/interfaces in flix which we statically know will always exists such as
    * RefClasses, RecordExtend Classes, IRecord. So we can statically ensure that methods which use these classes
    * in their signature are well-formed.
    *
    * This is temporary it just serves as a way to translate certain JvmType.Reference by assigning it a permanent type
    * (In flix classes types such as tuples/enum can only be known at runtime)
    */
  sealed trait MnemonicsType

  object MnemonicsType {

    case object UnsupportedOperationException extends MnemonicsType

    case object RecordInterface extends MnemonicsType

  }

  /**
    * F class as in Mnemonics
    * It simply serves as way to interface with the underlying method of generating Java bytecode
    * in this case we use the org.web.asm library
    *
    * It contains methods in an ad-hoc manner. The class and its underlying methods don't serve to enforce the
    * type safety because of this all of them simply cast in the to correct stack type
    */
  class F[T](mv: MethodVisitor, ct: JvmType.Reference) {

    /**
      * Emits the correct Jvm load instruction give the localType
      *
      * @param: jvmType the jvmType of the local we want to load
      * @param: location the of the local
      * @pre: This method should only be called if local in the specified location matches the jvmType,
      *       to avoid verifier errors
      */
    def load[S](localType: JvmType, location: Int): F[S] = {
      val loadIns = AsmOps.getLoadInstruction(localType)
      mv.visitVarInsn(loadIns, location)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a getField instruction given the fieldName and it's type
      *
      * @param: fieldName the of the field
      * @param: fieldType the jvmType of the field we want to get
      * @pre: This method should only be called if field with the specified fieldName matches the jvmType,
      *       to avoid verifier errors
      */
    def getField[S](fieldName: String, fieldType: JvmType): F[S] = {
      mv.visitFieldInsn(GETFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a putField instruction given the fieldName and it's type
      *
      * @param: fieldName the of the field
      * @param: fieldType the jvmType of the field we want to get
      * @pre: This method should only be called if field with the specified fieldName matches the jvmType,
      *       to avoid verifier errors
      */
    def putField[S](fieldName: String, fieldType: JvmType): F[S] = {
      mv.visitFieldInsn(PUTFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a invoke instruction given the invokeCode, className, methodName, args and returnType
      *
      * @param: InvokeCode of the method we want to call the List of possible values are
      *         InvokeSpecial, InvokeVirtual, InvokeStatic, InvokeInterface
      * @param: className name of the class which contains the method we pretend to invoke
      * @param: methodName name of the method we pretend to invoke
      * @param: args List of the JvmType of the args to call the method
      * @param: returnType the JvmType of the return type of the calling method
      * @pre: There are a few conditions which need to be ensured in order for this method to not generate
      *       verifier errors. The invokeCode should match the type of method which is being called i.e. if it's an
      *       interface method invokecode should be InvokeInterface, if its a virtual function invokeCode should be
      *       InvokeVirtual and so on. The args and returnType should match the description of the method in the .class
      *       file which can be found by using the classname and methodname. The method should also obviously exist
      *
      */
    def invoke[S](invokeCode: JvmModifier, className: String, methodName: String,
                  args: List[JvmType], returnType: JvmType): F[S] = {

      //Last argument is true if invoking interface
      mv.visitMethodInsn(invokeCode.toInternal, className, methodName, AsmOps.getMethodDescriptor(args, returnType), false)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a return instruction given the fieldType
      *
      * @param: jt the jvmType of what we want to return.
      * @pre: This method should only be called if jt matches the jvmType of what is currently on the
      *       Top of stack, jt should also be differnt from JvmType.Void.
      *       Finally jt should also be the same type as the return type of the function we want to return from,
      *       to avoid verifier erros
      */
    def _return[S](jt: JvmType): F[S] = {
      val ret = AsmOps.getReturnInstruction(jt)
      mv.visitInsn(ret)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a return instruction which returns nothing (or void)
      *
      * @pre: This method should only be called if the return type of the function we want to return from is Void
      */
    def _return[S](): F[S] = {
      mv.visitInsn(RETURN)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a 'new' instruction which creates a new instance of the provided JvmType.Reference
      *
      */
    def _new[S](jt: JvmType.Reference): F[S] = {
      mv.visitTypeInsn(NEW, jt.name.toInternalName)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a 'dup'  which duplicates whatever is on top of stack
      *
      */
    def dup[S](): F[S] = {
      mv.visitInsn(DUP)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a Ldc instruction which loads a constant onto the top of the stack
      *
      */
    def ldc[S](value: Object): F[S] = {
      mv.visitLdcInsn(value)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a throw instruction which is used to throw an exception
      *
      * @pre: To avoid verifier errors there should be an instance of an exception on Top of the stack
      *
      */
    def _throw[S](): F[S] = {
      mv.visitInsn(ATHROW)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a ifeq instruction which jumps to the provided label if the value of top of the stack is a bool
      * which it's value is true
      *
      * @pre: To avoid verifier errors there should be a bool on top of the stack and the label should exist
      *
      */
    def ifeq[S](label: Label): F[S] = {
      mv.visitJumpInsn(IFEQ, label)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a goto instruction which jumps to the provided label unconditionally
      *
      * @pre: To avoid verifier errors the label should exist
      *
      */
    def goto[S](label: Label): F[S] = {
      mv.visitJumpInsn(GOTO, label)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits the provided label
      *
      * @pre: To avoid verifier errors the label shouldn't exist
      *
      */
    def emitLabel[S](label: Label): F[S] = {
      mv.visitLabel(label)
      this.asInstanceOf[F[S]]
    }
  }

  /**
    * This class and it's inner function |>> is used to provide a nice way to append a series of
    *  Mnemonics.Instructions. It is essentially syntatic sugar for function composition
    */
  implicit class ComposeOps[A <: Stack, B <: Stack](f: F[A] => F[B]) {
    def |>>[C <: Stack](g: F[B] => F[C]): F[A] => F[C] = (x: F[A]) => g(f(x))
  }

  /**
    * This function allows to create Runtime values by using a compile time value.
    * It takes a type parameter T and maps it to the correct runtime JvmType value
    *
    * This is used to circumvent the use of dependent types which are not supported by scala.
    * It allows us to have a framework which more type-safe compared to a version which doesn't use this features
    */
  def getJvmType[T: TypeTag](implicit root: Root, flix: Flix): JvmType = typeOf[T] match {
    case t if t =:= typeOf[JvmType.Void.type] => JvmType.Void
    case t if t =:= typeOf[JvmType.PrimBool.type] => JvmType.PrimBool
    case t if t =:= typeOf[JvmType.PrimChar.type] => JvmType.PrimChar
    case t if t =:= typeOf[JvmType.PrimByte.type] => JvmType.PrimByte
    case t if t =:= typeOf[JvmType.PrimShort.type] => JvmType.PrimShort
    case t if t =:= typeOf[JvmType.PrimInt.type] => JvmType.PrimInt
    case t if t =:= typeOf[JvmType.PrimLong.type] => JvmType.PrimLong
    case t if t =:= typeOf[JvmType.PrimFloat.type] => JvmType.PrimFloat
    case t if t =:= typeOf[JvmType.PrimDouble.type] => JvmType.PrimDouble

    case _ => getJvmTypeReference[T]
  }

  /**
    * Similar to the getJvmType function but this one returns a JvmType.Reference
    */
  def getJvmTypeReference[T: TypeTag](implicit root: Root, flix: Flix): JvmType.Reference = typeOf[T] match {

    case t if t =:= typeOf[JvmType.String.type] => JvmType.String
    case t if t =:= typeOf[JvmType.Object.type] => JvmType.Object

    case t if t =:= typeOf[JvmType.Reference] => JvmType.Object

    case t if t =:= typeOf[MnemonicsType.UnsupportedOperationException.type] => JvmType.Reference(JvmName.UnsupportedOperationException)
    case t if t =:= typeOf[MnemonicsType.RecordInterface.type] => JvmOps.getRecordInterfaceType()

  }

  /**
    * Singleton which contains the operations which compose the core of how we keep track of the JvmStack at compile.
    * Each of the functions in this object return a function which maps a stack state to another according to the
    * semantics of the Jvm instruction which it is trying to model
    */
  object Instructions {

    /**
      * Polymorphic return.
      */
    def RETURN[T: TypeTag](implicit root: Root, flix: Flix): F[StackNil ** T] => F[StackNil] = {

      val jvmType = getJvmType[T]

      t => t._return(jvmType)
    }

    /**
      * Returns without a value.
      */
    def RETURN: F[StackNil] => F[StackNil] = t => t._return()

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[R <: Stack]: F[R ** JvmType.PrimInt.type ** JvmType.PrimInt.type] => F[R ** JvmType.PrimInt.type] = ???


    /**
      * Creates a new instance of the type parameter T which should be a subtype of JvmType.Reference and puts
      * it on top of the stack
      */
    //TODO: T should be a subtype of JvmType.Reference not MnemonicsType
    def NEW[R <: Stack, T <: MnemonicsType : TypeTag](implicit root: Root, flix: Flix): F[R] => F[R ** T] =
      t => t._new(getJvmTypeReference[T])

    /**
      * Duplicates whatever is on Top of the stack
      */
    def DUP[R <: Stack, T: TypeTag]: F[R ** T] => F[R ** T ** T] =
      t => t.dup()

    /**
      * Loads a constant string onto the stack
      */
    def LDC_STRING[R <: Stack](value: String): F[R] => F[R ** JvmType.String.type] =
      t => t.ldc(value)


    /**
      * Throws the exception which is currently on top of the stack
      */
    def THROW: F[StackNil ** MnemonicsType.UnsupportedOperationException.type] => F[StackNil] =
      t => t._throw()

    def IFEQ[S <: Stack](label: Label): F[S ** JvmType.PrimBool.type] => F[S] = t => t.ifeq(label)

    def GOTO[S <: Stack](label: Label): F[S] => F[S] = t => t.goto(label)

    def EMIT[S <: Stack](label: Label): F[S] => F[S] = t => t.emitLabel(label)

  }

  /**
    * Capability which allows to load/store a local variable
    */
  class Local[T: TypeTag](location: Int)(implicit root: Root, flix: Flix) {

    private val localType = getJvmType[T]

    def LOAD[R <: Stack]: F[R] => F[R ** T] = t => t.load(localType, location)

    def STORE[R <: Stack]: F[R ** T] => F[R] = t => t.load(localType, location)
  }


  /**
    * Capability which allows to get/put a field
    */
  class Field[T: TypeTag](fieldName: String)(implicit root: Root, flix: Flix) {

    private val fieldType = getJvmType[T]

    def GET_FIELD[R <: Stack]: F[R ** JvmType.Reference] => F[R ** T] = t => t.getField(fieldName, fieldType)

    def PUT_FIELD[R <: Stack]: F[R ** JvmType.Reference ** T] => F[R] = t => t.putField(fieldName, fieldType)
  }


  /**
    * Capability which allows acess the function locals in this case a function with 0 arguments
    */
  //TODO: Maybe need to have different types for signatures of static method as the 1st local (0 index) of static
  //TODO: functions isn't a local of type JvmType.Reference
  class FunSig0[R: TypeTag]() {
    def getArg0(implicit root: Root, flix: Flix): Local[JvmType.Reference] = new Local(0)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 1 argument
    */
  //TODO: Same as FunSig0
  class FunSig1[T1: TypeTag, R: TypeTag] {

    def getArg0(implicit root: Root, flix: Flix): Local[JvmType.Reference] = new Local(0)

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(1)
  }


  /**
    * Capability which allows acess the function locals in this case a function with 2 arguments
    */
  //TODO: Same as FunSig0
  class FunSig2[T1: TypeTag, T2: TypeTag, R: TypeTag] {

    def getArg0(implicit root: Root, flix: Flix): Local[JvmType.Reference] = new Local(0)

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(1)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(2)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 3 arguments
    */
  //TODO: Same as FunSig0
  class FunSig3[T1: TypeTag, T2: TypeTag, T3: TypeTag, R: TypeTag] {

    def getArg0(implicit root: Root, flix: Flix): Local[JvmType.Reference] = new Local(0)

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(1)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(2)

    def getArg3(implicit root: Root, flix: Flix): Local[T3] = new Local(3)

  }


  /**
    * Capability which allows to invoke a method with 0 arguments
    */
  //TODO: Similar to FunSig might need to have different types in order to ensure the type safety with
  //TODO: Static methods, interface methods, etc. Also might need an INVOKE_VOID method as we want the stack frame after
  //TODO: after invoking a void method to be F[S] and not F[S ** JvmType.Void.type]
  class Method0[R: TypeTag](invokeCode: JvmModifier, ct: JvmType.Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** JvmType.Reference] => F[S ** R] =
      t => t.invoke(invokeCode, ct.name.toInternalName, methodName, List(), getJvmType[R])

  }

  /**
    * Capability which allows to invoke a method with 1 argument
    */
  //TODO: Similar to Method0
  class Method1[T1: TypeTag, R: TypeTag](invokeCode: JvmModifier, ct: JvmType.Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** JvmType.Reference ** T1] => F[S ** R] =
      t => t.invoke(invokeCode, ct.name.toInternalName, methodName, List(), getJvmType[R])
  }

  /**
    * Capability which allows to invoke a method with 2 arguments
    */
  //TODO: Similar to Method0
  class Method2[T1: TypeTag, T2: TypeTag, R: TypeTag](invokeCode: JvmModifier, ct: JvmType.Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** JvmType.Reference ** T1 ** T2] => F[S ** R] =
      t => t.invoke(invokeCode, ct.name.toInternalName, methodName, List(), getJvmType[R])
  }

  /**
    * Capability which allows to invoke a method with 3 arguments
    */
  //TODO: Similar to Method0
  class Method3[T1: TypeTag, T2: TypeTag, T3: TypeTag, R: TypeTag](invokeCode: JvmModifier, ct: JvmType.Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** JvmType.Reference ** T1 ** T2 ** T3] => F[S ** R] =
      t => t.invoke(invokeCode, ct.name.toInternalName, methodName, List(), getJvmType[R])
  }

  //    class MnemonicsLabel(label: Label) {
  //      def IFEQ[S <: Stack]: F[S ** JvmType.PrimBool.type] => F[S] = t => t.ifeq(label)
  //
  //      def GOTO[S <: Stack]: F[S] => F[S] = t => t.goto(label)
  //    }

  //  def mkBlock[S <: Stack, R <: Stack](f: MnemonicsLabel => F[S] => F[R]): F[S] => F[R] = {
  //
  //    val label = new Label
  //    val mnemonicsLabel = new MnemonicsLabel(label)
  //
  //    f(mnemonicsLabel)
  //  }


  /**
    * This class allows to generate classes. It includes support to generate(CompileField) and methods
    *
    * @param: ct classType of the class we want to generate
    * @param: modifiers list of modififers which we want to generate the class with (public, abstract, etc..)
    * @param: superClass the class we are generating superClass
    * @param: implementedInterfaces Array of interfaces which this new class shall implement
    */
  class ClassGenerator(ct: JvmType.Reference,
                       modifiers: List[JvmModifier], superClass: JvmType.Reference, implementedInterfaces: Array[JvmType.Reference])
                      (implicit root: Root, flix: Flix) {


    //Create the class writer and initialize, by providing the correct params.
    //The modifiers, superclass, implementedInterfaces
    private val cw: ClassWriter = {

      val superClassName = superClass.name.toInternalName
      val implementedInterfacesNames = implementedInterfaces.map(interface => interface.name.toInternalName)

      val cw = AsmOps.mkClassWriter()
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      cw.visit(AsmOps.JavaVersion, modifierVal, ct.name.toInternalName, null,
        superClassName, implementedInterfacesNames)
      cw
    }

    /**
      * This method generates in the current class we are generating a method with 0 arguments
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @param: f, transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *         will describe what instruction the method we are generating will execute.
      * @return: returns a Method0 capability allows now the possibility to invoke this new Method with 0 arguments
      */
    def mkMethod0[R: TypeTag](modifiers: List[JvmModifier], methodName: String,
                              f: FunSig0[R] => F[StackNil] => F[StackNil]): Method0[R] = {

      val returnType = getJvmType[R]
      val funSig = new FunSig0[R]()

      emitVirtualMethod(modifiers, methodName, List(), returnType, f(funSig))

      new Method0(JvmModifier.InvokeVirtual, ct, methodName)
    }


    /**
      * This method generates in the current class we are generating a method with 1 argument1
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @param: f, transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *         will describe what instruction the method we are generating will execute.
      * @return: returns a Method1 capability allows now the possibility to invoke this new Method with 1 argument
      */
    def mkMethod1[T1: TypeTag, R: TypeTag](modifiers: List[JvmModifier], methodName: String,
                                           f: FunSig1[T1, R] => F[StackNil] => F[StackNil]): Method1[T1, R] = {

      val arg1Type = getJvmType[T1]
      val returnType = getJvmType[R]
      val funSig = new FunSig1[T1, R]()

      emitVirtualMethod(modifiers, methodName, List(arg1Type), returnType, f(funSig))

      new Method1(JvmModifier.InvokeVirtual, ct, methodName)
    }


    /**
      * This method generates in the current class we are generating a method with 2 arguments
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @param: f, transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *         will describe what instruction the method we are generating will execute.
      * @return: returns a Method2 capability allows now the possibility to invoke this new Method with 2 arguments
      */
    def mkMethod2[T1: TypeTag, T2: TypeTag, R: TypeTag](modifiers: List[JvmModifier], methodName: String,
                                                        f: FunSig2[T1, T2, R] => F[StackNil] => F[StackNil]): Method2[T1, T2, R] = {

      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val returnType = getJvmType[R]

      val funSig = new FunSig2[T1, T2, R]()

      emitVirtualMethod(modifiers, methodName, List(arg1Type, arg2Type), returnType, f(funSig))

      new Method2(JvmModifier.InvokeVirtual, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a method with 3 arguments
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @param: f, transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *         will describe what instruction the method we are generating will execute.
      * @return: returns a Method3 capability allows now the possibility to invoke this new Method with 3 arguments
      */
    def mkMethod3[T1: TypeTag, T2: TypeTag, T3: TypeTag, R: TypeTag](modifiers: List[JvmModifier], methodName: String,
                                                                     f: FunSig3[T1, T2, T3, R] => F[StackNil] => F[StackNil]): Method3[T1, T2, T3, R] = {

      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]
      val returnType = getJvmType[R]

      val funSig = new FunSig3[T1, T2, T3, R]()

      emitVirtualMethod(modifiers, methodName, List(arg1Type, arg2Type, arg3Type), returnType, f(funSig))
      new Method3(JvmModifier.InvokeVirtual, ct, methodName)
    }

    /**
      * Auxiliary method which actually emits the method bytecode onto the current class.
      * Should be used by functions which make method
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @param: args List of the arguments JvmType of the function we are generating code for
      * @param: returnType the JvmType of the return type of the function we are generating code for
      * @param: f is a frame transfomer  it will describe what instruction the method we are generating will execute.
      */
    private def emitVirtualMethod(modifiers: List[JvmModifier], methodName: String,
                                  args: List[JvmType], returnType: JvmType, f: F[StackNil] => F[StackNil]): Unit = {
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val mv = cw.visitMethod(modifierVal, methodName,
        AsmOps.getMethodDescriptor(args, returnType), null, null)
      mv.visitCode()

      f(new F[StackNil](mv, ct))

      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }

    /**
      * Method which receives a list of JvmModifiers and fieldName. It will emit code to generate a field
      * with the correspoding type parameter T.
      *
      * @param: modifiers list of modififers which we want to generate the field with (public, private, final, etc..)
      * @param: fieldName name which we want to give to the field we are generating
      * @return returns the capability to acess the created field. Similar to the methods this ensure we only
      *         acess field we've generated
      */
    def compileField[T: TypeTag](modifiers: List[JvmModifier], fieldName: String): Field[T] = {

      val fieldType = getJvmType[T]
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val field = cw.visitField(modifierVal, fieldName, fieldType.toDescriptor, null, null)
      field.visitEnd()

      new Field(fieldName)
    }

    /**
      * Method which compiles the class. Should be called when done creating the class.
      * It will return the corresponding byte array (bytecode) for the class we are generating
      */
    def compile(): Array[Byte] = {
      cw.visitEnd()
      cw.toByteArray
    }
  }


  /**
    * This class allows to generate interfaces. It includes support to interface methods
    *
    * @param: it classType of the interaface we want to generate
    * @param: modifiers list of modififers which we want to generate the class with (public, abstract, etc..)
    * @param: superClass the class we are generating superClass
    * @param: implementedInterfaces Array of interfaces which this new class shall implement
    */
  class InterfaceGenerator(it: JvmType.Reference,
                           modifiers: List[JvmModifier], superClass: JvmType.Reference, implementedInterfaces: Array[JvmType.Reference])
                          (implicit root: Root, flix: Flix) {


    //Create the class writer and initialize, by providing the correct params.
    //The modifiers, superclass, implementedInterfaces
    private val cw: ClassWriter = {

      val superClassName = superClass.name.toInternalName
      val implementedInterfacesNames = implementedInterfaces.map(interface => interface.name.toInternalName)

      val cw = AsmOps.mkClassWriter()
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      cw.visit(AsmOps.JavaVersion, modifierVal, it.name.toInternalName, null,
        superClassName, implementedInterfacesNames)
      cw
    }


    /**
      * This method generates in the current interface we are generating a method with 0 arguments
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @return: returns a Method0 capability allows now the possibility to invoke this new Method with 0 arguments
      */
    def mkMethod0[R: TypeTag](modifiers: List[JvmModifier], methodName: String): Method0[R] = {

      val returnType = getJvmType[R]

      emitInterfacelMethod(modifiers, methodName, List(), returnType)

      new Method0(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a method with 1 argument
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @return: returns a Method1 capability allows now the possibility to invoke this new Method with arguments
      */
    def mkMethod1[T1: TypeTag, R: TypeTag](modifiers: List[JvmModifier], methodName: String): Method1[T1, R] = {

      val arg1Type = getJvmType[T1]
      val returnType = getJvmType[R]

      emitInterfacelMethod(modifiers, methodName, List(arg1Type), returnType)

      new Method1(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a method with 2 arguments
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @return: returns a Method2 capability allows now the possibility to invoke this new Method with 2 arguments
      */
    def mkMethod2[T1: TypeTag, T2: TypeTag, R: TypeTag](modifiers: List[JvmModifier], methodName: String): Method2[T1, T2, R] = {

      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val returnType = getJvmType[R]

      emitInterfacelMethod(modifiers, methodName, List(arg1Type, arg2Type), returnType)

      new Method2(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a method with 3 arguments
      * given the provided params. It return the capability to invoke method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @return: returns a Method3 capability allows now the possibility to invoke this new Method with 3 arguments
      */
    def mkMethod3[T1: TypeTag, T2: TypeTag, T3: TypeTag, R: TypeTag](modifiers: List[JvmModifier], methodName: String): Method3[T1, T2, T3, R] = {

      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]
      val returnType = getJvmType[R]

      emitInterfacelMethod(modifiers, methodName, List(arg1Type, arg2Type, arg3Type), returnType)
      new Method3(JvmModifier.InvokeInterface, it, methodName)
    }


    /**
      * Auxiliary method which actually emits the method bytecode onto the current interface.
      * Should be used by functions which make method.
      * Since this is an interface there is no actuall instructions which are emitted for the method
      *
      * @param: modifiers list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param: methodName name which we want to give to the method we are generating
      * @param: args List of the arguments JvmType of the function we are generating code for
      * @param: returnType the JvmType of the return type of the function we are generating code for
      * @param: f is a frame transfomer  it will describe what instruction the method we are generating will execute.
      */
    private def emitInterfacelMethod(modifiers: List[JvmModifier], methodName: String,
                                     args: List[JvmType], returnType: JvmType): Unit = {
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val mv = cw.visitMethod(modifierVal, methodName,
        AsmOps.getMethodDescriptor(args, returnType), null, null)
      mv.visitEnd()
    }

    /**
      * Method which compiles the interface. Should be called when done creating the class.
      * It will return the corresponding byte array (bytecode) for the class we are generating
      */
    def compile(): Array[Byte] = {
      cw.visitEnd()
      cw.toByteArray
    }
  }
  //TODO: Might need to similarly to InterfaceGenerator and ClassGenerator generate a StaticGenerator

  /**
    * Auxiliary method which returns the transformer to generate the body of a function which simply throws an
    * Exception (UnsupportedOperationException in this case)
    *
    * @param: message which we want to display when the exception is thrown
    */
  def newUnsupportedOperationExceptionInstructions(message: String)(implicit root: Root, flix: Flix): F[StackNil] => F[StackNil] = {
    Instructions.NEW[StackNil, MnemonicsType.UnsupportedOperationException.type] |>>
      Instructions.DUP |>>
      Instructions.LDC_STRING(message) |>>
      Api.JavaRuntimeFunctions.ExceptionConstructor.INVOKE |>>
      Instructions.THROW
  }
}

