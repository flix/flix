package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, FreeVar, Root}
import ca.uwaterloo.flix.language.ast.Symbol
import org.objectweb.asm.Opcodes._

/**
  * Generates byte code for the closure classes.
  */
object GenClosureClasses {

  /**
    * Returns the set of closures classes for the given set of definitions `defs`.
    */
  def gen(closures: Set[ClosureInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a closure class for each closure and collect the results in a map.
    //
    closures.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, closure) =>
        val jvmType = JvmOps.getClosureClassType(closure)
        val jvmName = jvmType.name
        val bytecode = genByteCode(closure)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }
  }

  /**
    * Returns the byte code for the closure with the given symbol `sym` and free variables `freeVars`.
    *
    * For example, given the symbol `mkAdder` with type (Int, Int) -> Int and the free variable `x`, we create:
    *
    * Clo$mkAdder extends Fn1$Int$Int {
    *   private int x;
    *   private int arg0;
    *   private int res;
    *
    *   public Clo$mkAdder(int x) {
    *     this.x = x;
    *   }
    *
    *   public setArg0(int arg0) {
    *     this.arg0 = arg0;
    *   }
    *
    *   public int getResult() {
    *     return this.res;
    *   }
    *
    *   public void apply(Context ctx) {
    *     this.res = this.x + this.arg0;
    *   }
    * }
    *
    *
    */
  private def genByteCode(closure: ClosureInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Args of the function
    val args = closure.tpe.typeArguments

    // `JvmType` of the interface for `closure.tpe`
    val functionInterface = JvmOps.getFunctionInterfaceType(closure.tpe)

    // The super interface.
    val superInterface = Array(functionInterface.name.toInternalName)

    // `JvmType` of the class for `defn`
    val classType = JvmOps.getClosureClassType(closure)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, superInterface)

    // Adding a setter and a field for each argument of the function
    for((arg, index) <- args.init.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedType(arg)

      // `arg$index` field
      AsmOps.compileField(visitor, s"arg$index", argType, isStatic = false, isPrivate = true)

      // `setArg$index()` method
      AsmOps.compileSetFieldMethod(visitor, classType.name, argType, s"arg$index", s"setArg$index")
    }

    // Jvm type of the result of the function
    val resultType = JvmOps.getErasedType(args.last)

    // Field for the result
    AsmOps.compileField(visitor, "result", resultType, isStatic = false, isPrivate = true)

    // Getter for the result field
    AsmOps.compileGetFieldMethod(visitor, classType.name, resultType, "result", "getResult")

    for(freeVar <- closure.freeVars) {
      // `JvmType` of `freeVar`
      val varType = JvmOps.getErasedType(freeVar.tpe)

      // `arg$index` field
      AsmOps.compileField(visitor, JvmOps.getVariableName(freeVar.sym), varType, isStatic = false, isPrivate = true)
    }


    List(0xCA.toByte, 0xFE.toByte, 0xBA.toByte, 0xBE.toByte).toArray
  }

}
