package flix

import impl.logic.Value

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macros {
  /*
   * Macro for creating functions that unwrap and wrap Values.
   * The function provided to the macro must be partially applied, e.g.
   *    valueWrapperFunc(func _)
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
  def valueWrapperFunc(f: => Any): Value => Value = macro valueWrapperFuncImpl

  /*
   * Macro implementation
   *
   * The main implementation (at the very bottom) is quite short.
   * However, it calls two helper methods (unwrapValue and wrapValue),
   * which are pretty complicated. Unfortunately, they need to be nested
   * inside impl(), because of the import c.universe._.
   *
   * NOTE: If Value ever changes, this implementation will need to be updated.
   */
  def valueWrapperFuncImpl(c: whitebox.Context)(f: c.Tree): c.Expr[Value => Value] = {
    import c.universe._

    /*
     * Takes a list of types and the identifier of a Value to unwrap. The
     * types correspond to what the Value should unwrap to.
     * Returns a pair: code that unwraps the Value, and a list of expressions
     * (as syntax trees) to call the function with.
     *
     * For example, if v is a Value representing the argument types
     * ((Int, Bool), String), we unwrap v with:
     *    val Value.Tuple2(
     *      Value.Tuple2(Value.Int(a: Int), Value.Bool(b: Boolean),
     *      Value.Str(c: String)) = v
     * We construct the arguments as: (a, b), c
     * So the function call would look like: func((a, b), c)
     *
     * We return a list of expressions (as syntax trees), since we need to
     * call the function with expressions (as above), not just a list of
     * identifiers.
     */
    def unwrapValue(valName: c.TermName, types: List[c.Type]): (c.Tree, List[c.Tree]) = {
      /*
       * Unwrap a non-tuple Value. The special cases are handled here, with
       * unwrapOthers handling the easy cases.
       */
      def unwrapSingleton(t: c.Type): (c.Tree, List[c.Tree]) = t match {
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
       *    Value.Tuple2(Value.Bool(a1: Boolean), Value.Int(a2: Int)).
       */
      def unwrapTuple(ts: List[c.Type]): (c.Tree, List[c.Tree]) = {
        val tuple = TermName("Tuple" + ts.size)
        val (inner, args) = ts.map(x => unwrapSingleton(x)).unzip
        (pq"Value.$tuple(..$inner)", args.flatten)
      }

      /*
       * Unwrapping the set is straightforward, but the set elements need to
       * be recursively unwrapped. For example, we first unwrap the set:
       *    val Value.Set(ss: Set[Value]) = v.
       * Then we unwrap the elements of ss, and call the function with:
       *    ss.map((x: Value) => { val Value.Int(a: Int) = x; a }).
       */
      def unwrapSet(t: c.Type): (c.Tree, List[c.Tree]) = {
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
       * Unwrap a user-defined type. We either use Value.Tag or Value.Native.
       * Value.Tag has two further subcases. Consider the Flix type:
       *    (def-type FooTag (variant ((FZero) (FOne Int) (FTwo Int Str))))
       * and corresponding Scala type:
       *    sealed trait FooTag
       *    case object FZero extends FooTag
       *    case class FOne(n: Int) extends FooTag
       *    case class FTwo(m: Int, n: String) extends FooTag
       * We could be unwrapping one of the subclasses (FZero, FOne, or FTwo),
       * or the base trait (or abstract class), FooTag.
       *
       * NOTE: The base trait/class (e.g. FooTag) must be sealed.
       */
      def unwrapTagOrNative(t: c.Type): (c.Tree, List[c.Tree]) = {
        val classSym = t.typeSymbol.asClass
        // knownDirectSubclasses requires classSym to represent a sealed class.
        // Furthermore, it returns a set. To ensure the order is deterministic
        // and predictable, we use a sorted list.
        val subclasses = classSym.knownDirectSubclasses.toList.sortBy(_.fullName)

        if (classSym.isCaseClass) {
          // We have a subclass of the tag, e.g. FZero, FOne, or FTwo. We can
          // directly unwrap with something like:
          //    val Value.Tag(Symbol.NamedSymbol(_), Value.Int(n: Int), _) = v
          // When we call the function, we have to instantiate the case class:
          //    val ret = f(FOne(n))

          val className = TermName(classSym.name.toString)

          // Does the class constructor take 0, 1, or multiple parameters?
          val (inner, arg) = classSym.primaryConstructor.asMethod.paramLists.head match {
            case Nil => (q"Value.Unit", q"$className")
            case List(ty) =>
              val (pattern, args) = unwrapSingleton(ty.typeSignature)
              (pattern, q"$className(..$args)")
            case tys =>
              val (pattern, args) = unwrapTuple(tys.map(_.typeSignature))
              (pattern, q"$className(..$args)")
          }
          (pq"Value.Tag(Symbol.NamedSymbol(_), $inner, _)", List(arg))

        } else if (subclasses.nonEmpty && subclasses.forall(_.asClass.isCaseClass)) {
          // To determine if the trait is used for case classes, we check that
          // all known subclasses are case classes (and that there actually are
          // subclasses, since forall returns true for an empty list).

          // We have a trait (or abstract class), e.g. FooTag.
          //    val Value.Tag(Symbol.NamedSymbol(s), tagVal, _) = v
          // However, we don't know the type of tagVal, so we match on s:
          //    s match {
          //      case "FOne" => val Value.Int(n: Int) = tagVal; FOne(n)
          //      case "FTwo" => ...
          //      case "FZero" => FZero
          //    }
          // We then call f with the match expression.
          val tagVal = TermName(c.freshName("tagVal"))
          val cases = subclasses.map(sc => {
            val subclassName = TermName(sc.name.toString)

            // Does the class constructor take 0, 1, or multiple parameters?
            val body = sc.asClass.primaryConstructor.asMethod.paramLists.head match {
              case Nil => q"$subclassName"
              case List(ty) =>
                val (pattern, args) = unwrapSingleton(ty.typeSignature)
                q"val $pattern = $tagVal; $subclassName(..$args)"
              case tys =>
                val (pattern, args) = unwrapTuple(tys.map(_.typeSignature))
                q"val $pattern = $tagVal; $subclassName(..$args)"
            }
            cq"${sc.name.toString} => $body"
          })
          val sName = TermName(c.freshName("s"))
          val arg = q"$sName match { case ..$cases }"
          (pq"Value.Tag(Symbol.NamedSymbol($sName), $tagVal, _)", List(arg))

        } else {
          // Not a tag, so use Value.Native.
          val argName = TermName(c.freshName("arg"))
          (pq"Value.Native($argName: $t)", List(q"$argName"))
        }
      }

      /*
       * Handle the easy cases here.
       */
      def unwrapOthers(t: c.Type): (c.Tree, List[c.Tree]) = {
        val argName = TermName(c.freshName("arg"))
        val args = List(q"$argName")
        t match {
          case ty if ty =:= typeOf[scala.Boolean] => (pq"Value.Bool($argName: $t)", args)
          case ty if ty =:= typeOf[scala.Int] => (pq"Value.Int($argName: $t)", args)
          case ty if ty =:= typeOf[java.lang.String] => (pq"Value.Str($argName: $t)", args)
          case ty => unwrapTagOrNative(ty)
        }
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
    def wrapValue(exprToWrap: c.Tree, typeToWrap: c.Type): c.Tree = {
      /*
       * A set must be wrapped as a Value.Set, and its elements must also be
       * recursively wrapped. For example, if st is a Set[Int], we wrap it as
       *    Value.Set(st.map({ (x: Int) => Value.Int(x) }).
       */
      def wrapSet(setToWrap: c.Tree, t: c.Type): c.Tree = {
        val x = TermName(c.freshName("x"))
        val inner = q"{ ($x: $t) => ${wrapValue(q"$x", t)} }"
        q"Value.Set($setToWrap.map($inner))"
      }

      /*
       * A tuple must be wrapped as a Value.TupleN of Values. So we have to
       * recursively wrap the tuple elements. For example, if t is a pair of
       * Boolean and Int, we wrap it with:
       *    Value.Tuple2(Value.Bool(t._1), Value.Int(t._2)).
       */
      def wrapTuple(tupleToWrap: c.Tree, types: List[c.Type]): c.Tree = {
        val tuple = TermName("Tuple" + types.size)
        val inner = types.zipWithIndex.map { case (t, i) =>
          val elt = TermName("_" + (i+1))
          wrapValue(q"$tupleToWrap.$elt", t)
        }
        q"Value.$tuple(..$inner)"
      }

      // TODO(mhyee): Implement this
      def wrapTagOrNative(expr: c.Tree, t: c.Type): c.Tree = {
        q"Value.Native($expr)"
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
            wrapSet(exprToWrap, ty.typeArgs.head)
        case ty if ty.typeSymbol.fullName.startsWith("scala.Tuple") =>
          // ty is a tuple (of some arity), so typeArgs gives us the types
          // that make up the tuple.
          wrapTuple(exprToWrap, ty.typeArgs)
        case ty => wrapTagOrNative(exprToWrap, ty)
      }
    }

    /*
     * Main implementation begins here.
     */

    // The function type (A, B, C) => D is represented as List(A, B, C, D).
    val paramTypes = f.tpe.typeArgs.init
    val retType = f.tpe.typeArgs.last

    // Extract the name of the function. f looks something like:
    //    { ((a1: A1, a2: A2) => func(a1, a2)) }.
    val q"{ ((..${_}) => $func(..${_})) }" = f

    val valName = TermName(c.freshName("v"))
    val retName = TermName(c.freshName("ret"))

    val (unwrapper, args) = unwrapValue(valName, paramTypes)
    val call = q"val $retName = $func(..$args)"
    val wrapper = wrapValue(q"$retName", retType)

    c.Expr[Value => Value] {
      q"($valName: Value) => { ..$unwrapper; ..$call; ..$wrapper }"
    }
  }
}
