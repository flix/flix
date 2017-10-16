package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, FreeVar, Root}
import ca.uwaterloo.flix.language.ast.Symbol

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
    // TODO: Do we need a mechanism to find each closure expression?
    closures.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, closure) =>
      macc
      // TODO
        //val jvmType = JvmOps.getClosureClassType(closure)
        //val jvmName = jvmType.name
        //val bytecode = genByteCode(closure)
        //macc + (jvmName -> JvmClass(jvmName, bytecode))
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
    List(0xCA.toByte, 0xFE.toByte, 0xBA.toByte, 0xBE.toByte).toArray
  }

}
