import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import impl.logic.Value

object Macros {

  def demo(n: Int): Int => Int = macro demoImpl

  def demoImpl(c: whitebox.Context)(n: c.Expr[Int]): c.Expr[Int => Int] = {
    import c.universe._

    println("Hello from macro implementation.")

    c.Expr[Int => Int] {
      q"""
          (m: Int) => m + $n
       """
    }
  }

  /*
   * Macro for creating functions that unwrap and wrap Values.
   * The function provided to the macro must be partially applied,
   * e.g. m(func _)
   *
   * Given function func: A => B, generate the wrapper:
   *    (v: Value) => {
   *      val Value.A(a) = v
   *      val ret = func(a)
   *      Value.B(ret)
   *    }
   * For multiple parameters, given func: (A1, A2) => B, generate:
   *    (v: Value) => {
   *      val Value.Tuple3(Value.A1(a1), Value.A2(a2)) = v
   *      val ret = func(a1, a2)
   *      Value.B(ret)
   *    }
   */
  def m(f: => Any): Value => Value = macro impl

  /*
   * Macro implementation
   *
   * The main implementation (at the very bottom) is quite short.
   * However, it calls two helper methods (unwrapValue and wrapValue),
   * which are pretty complicated. Unfortunately, they need to be nested
   * inside impl(), because of the import c.universe._.
   *
   * If Value ever changes, this implementation will need to be updated.
   */
  def impl(c: whitebox.Context)(f: c.Tree): c.Expr[Value => Value] = {
    import c.universe._

    /*
     * Takes a list of types and the identifier of a Value to unwrap. The
     * types correspond to what the Value should unwrap to.
     * Returns a pair: code that unwraps the Value, and a list of expressions
     * (as syntax trees) to call the function with.
     *
     * For example, if v is a Value representing the argument types
     * ((Int, Bool), String), we unwrap v with:
     * val Value.Tuple2(
     *   Value.Tuple2(Value.Int(a: Int), Value.Bool(b: Boolean),
     *   Value.Str(c: String)) = v
     * We construct the arguments as:
     * (a, b), c
     * So the function call would look like: func((a, b), c)
     *
     * We return a list of expressions (as syntax trees), since we need to
     * call the function with expressions (as above), not just a list of
     * identifiers.
     */
    def unwrapValue(types: List[Type], valName: TermName): (c.Tree, List[c.Tree]) = {

      /*
       * Unwrap a non-tuple Value. The special cases are handled here, with
       * unwrapOthers handling the easy cases.
       */
      def unwrapSingleton(t: Type): (c.Tree, List[c.Tree]) = t match {
        case ty if ty =:= typeOf[scala.Unit] =>
          // The function actually takes a value of scala.Unit, so we need to
          // call it with an actual Unit, ().
          (pq"_", List(q"()"))
        case ty if ty <:< weakTypeOf[scala.collection.immutable.Set[_]] =>
          // ty is a Set[T], so typArgs.head gives us T
          unwrapSet(t.typeArgs.head)
        case ty if ty.typeSymbol.fullName.startsWith("scala.Tuple") =>
          // ty is a tuple (of some arity), so typeArgs gives us the types
          // that make up the tuple. When we get the arguments back, we have
          // to put them in a tuple literal.
          val (pattern, args) = unwrapTuple(t.typeArgs)
          (pattern, List(q"(..$args)"))
        case _ => unwrapOthers(t)
      }

      /*
       * Common code for unwrapping multiple arguments or a tuple, both of
       * which are represented by a Value.TupleN. Besides unwrapping the
       * tuple, we also recursively unwrap the tuple elements. For example,
       * we might have the following unwrap code:
       * Value.Tuple2(Value.Bool(a1: Boolean), Value.Int(a2: Int)).
       */
      def unwrapTuple(ts: List[Type]): (c.Tree, List[c.Tree]) = {
        val tuple = TermName("Tuple" + ts.size)
        val (inner, args) = ts.map(x => unwrapSingleton(x)).unzip
        (pq"Value.$tuple(..$inner)", args.flatten)
      }

      /*
       * Unwrapping the set is straightforward, but the set elements need to
       * be recursively unwrapped. For example, we first unwrap the set:
       * val Value.Set(ss: Set[Value]) = v.
       * Then we unwrap the elements of ss, and call the function with:
       * ss.map((x: Value) => { val Value.Int(a: Int) = x; a }).
       */
      def unwrapSet(t: Type): (c.Tree, List[c.Tree]) = {
        // Unwrap the set
        val argName = TermName(c.freshName("arg"))
        val pattern = pq"Value.Set($argName: Set[Value])"

        // Recursively unwrap the set elements
        val (inner, innerArgs) = unwrapSingleton(t)
        val x = TermName(c.freshName("x"))
        val arg = q"$argName.map(($x: Value) => { val $inner = $x; ..$innerArgs })"

        (pattern, List(arg))
      }

      /*
       * Handle the easy cases here.
       */
      def unwrapOthers(t: Type): (c.Tree, List[c.Tree]) = {
        val argName = TermName(c.freshName("arg"))
        val pattern = t match {
          case ty if ty =:= typeOf[scala.Boolean] => pq"Value.Bool($argName: $t)"
          case ty if ty =:= typeOf[scala.Int] => pq"Value.Int($argName: $t)"
          case ty if ty =:= typeOf[java.lang.String] => pq"Value.Str($argName: $t)"
          case _ => pq"Value.Native($argName: $t)"
        }
        (pattern, List(q"$argName"))
      }

      val (pattern, args) = types match {
        case Nil => (pq"_", List())
        case List(t) => unwrapSingleton(t)
        case _ => unwrapTuple(types)
      }

      (q"val $pattern = $valName", args)
    }

    /*
     * Takes an expression to wrap (as a syntax tree) and a (Scala) type.
     * Returns code that wraps the expression as a Value.
     *
     * For example, if b is a boolean, we wrap it as Value.Bool(b).
     *
     * The function takes a tree instead of an identifier (to wrap), because
     * it needs to be able to wrap tuple elements. x._1 should be treated as
     * the first element of x, rather than an identifier.
     */
    def wrapValue(exprToWrap: Tree, typeToWrap: Type): c.Tree = {

      /*
       * A set must be wrapped as a Value.Set, and its elements must also be
       * recursively wrapped. For example, if st is a Set[Int], we wrap it as
       * Value.Set(st.map({ (x: Int) => Value.Int(x) }).
       */
      def wrapSet(t: Type): c.Tree = {
        val x = TermName(c.freshName("x"))
        val inner = q"{ ($x: $t) => ${wrapValue(q"$x", t)} }"
        q"Value.Set($exprToWrap.map($inner))"
      }

      /*
       * A tuple must be wrapped as a Value.TupleN of Values. So we have to
       * recursively wrap the tuple elements. For example, if t is a pair of
       * Boolean and Int, we wrap it with:
       * Value.Tuple2(Value.Bool(t._1), Value.Int(t._2)).
       */
      def wrapTuple(types: List[Type]): c.Tree = {
        val tuple = TermName("Tuple" + types.size)
        val inner = types.zipWithIndex.map { case (t, i) =>
          val elt = TermName("_" + (i+1))
          wrapValue(q"$exprToWrap.$elt", t)
        }
        q"Value.$tuple(..$inner)"
      }

      typeToWrap match {
        case ty if ty =:= typeOf[scala.Unit] => q"Value.Unit"
        case ty if ty =:= typeOf[scala.Boolean] => q"Value.Bool($exprToWrap)"
        case ty if ty =:= typeOf[scala.Int] => q"Value.Int($exprToWrap)"
        case ty if ty =:= typeOf[java.lang.String] => q"Value.Str($exprToWrap)"
        case ty if ty <:< weakTypeOf[scala.collection.immutable.Set[_]] =>
          // ty is a Set[T], so typeArgs.head gives us T, unless the set is empty
          if (ty.typeArgs.isEmpty)
            q"Value.Set(Set.empty)"
          else
            wrapSet(ty.typeArgs.head)
        case ty if ty.typeSymbol.fullName.startsWith("scala.Tuple") =>
          // ty is a tuple (of some arity), so typeArgs gives us the types
          // that make up the tuple.
          wrapTuple(ty.typeArgs)
        case _ => q"Value.Native($exprToWrap)"
      }
    }

    /*
     * Main implementation begins here.
     */

    // The function type (A, B, C) => D is represented as List(A, B, C, D).
    val paramTypes = f.tpe.typeArgs.init
    val retType = f.tpe.typeArgs.last

    // Extract the name of the function. f looks something like:
    // { ((a1: A1, a2: A2) => func(a1, a2)) }.
    val q"{ ((..${_}) => $func(..${_})) }" = f

    val valName = TermName(c.freshName("v"))
    val retName = TermName(c.freshName("ret"))

    val (unwrapper, args) = unwrapValue(paramTypes, valName)
    val call = q"val $retName = $func(..$args)"
    val wrapper = wrapValue(q"$retName", retType)

    c.Expr[Value => Value] {
      q"($valName: Value) => { ..$unwrapper; ..$call; ..$wrapper }"
    }
  }

}
