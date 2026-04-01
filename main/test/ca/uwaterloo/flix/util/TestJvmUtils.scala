package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestJvmUtils extends AnyFunSuite {

  // --- resolveTypeParamMapping: direct (declaring class == instantiated class) ---

  test("resolveTypeParamMapping.Direct.Comparator.compare") {
    // Comparator<T> declares compare(T, T) directly.
    val method = classOf[java.util.Comparator[_]].getMethod("compare", classOf[Object], classOf[Object])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.Comparator[_]])
    assert(result == Map("T" -> 0))
  }

  test("resolveTypeParamMapping.Direct.Callable.call") {
    // Callable<V> declares call() directly.
    val method = classOf[java.util.concurrent.Callable[_]].getMethod("call")
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.concurrent.Callable[_]])
    assert(result == Map("V" -> 0))
  }

  test("resolveTypeParamMapping.Direct.ArrayList.get") {
    // ArrayList<E> declares get(int) directly (not inherited).
    val method = classOf[java.util.ArrayList[_]].getMethod("get", classOf[Int])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.ArrayList[_]])
    assert(result == Map("E" -> 0))
  }

  test("resolveTypeParamMapping.Direct.TreeMap.get") {
    // TreeMap<K,V> declares get(Object) directly.
    val method = classOf[java.util.TreeMap[_, _]].getMethod("get", classOf[Object])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.TreeMap[_, _]])
    assert(result == Map("K" -> 0, "V" -> 1))
  }

  // --- resolveTypeParamMapping: inherited (one level) ---

  test("resolveTypeParamMapping.Inherited.UnaryOperator.apply") {
    // UnaryOperator<T> extends Function<T, T>.
    // apply is declared on Function with params [T, R].
    // Both T and R map to index 0 (UnaryOperator.T).
    val method = classOf[java.util.function.UnaryOperator[_]].getMethod("apply", classOf[Object])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.function.UnaryOperator[_]])
    assert(result == Map("T" -> 0, "R" -> 0))
  }

  test("resolveTypeParamMapping.Inherited.BinaryOperator.apply") {
    // BinaryOperator<T> extends BiFunction<T, T, T>.
    // apply is declared on BiFunction with params [T, U, R].
    // All three map to index 0 (BinaryOperator.T).
    val method = classOf[java.util.function.BinaryOperator[_]].getMethod("apply", classOf[Object], classOf[Object])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.function.BinaryOperator[_]])
    assert(result == Map("T" -> 0, "U" -> 0, "R" -> 0))
  }

  // --- resolveTypeParamMapping: no type params ---

  test("resolveTypeParamMapping.NoParams.Runnable.run") {
    // Runnable has no type parameters.
    val method = classOf[Runnable].getMethod("run")
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[Runnable])
    assert(result == Map.empty)
  }

  test("resolveTypeParamMapping.NoParams.Object.toString") {
    // toString is declared on Object (no type params).
    val method = classOf[Object].getMethod("toString")
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[Object])
    assert(result == Map.empty)
  }

  // --- resolveTypeParamMapping: static methods ---

  test("resolveTypeParamMapping.Static.Integer.valueOf") {
    // Static methods always return empty map.
    val method = classOf[java.lang.Integer].getMethod("valueOf", classOf[Int])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.lang.Integer])
    assert(result == Map.empty)
  }

  test("resolveTypeParamMapping.Static.Collections.sort") {
    // Static method on a non-generic class.
    val method = classOf[java.util.Collections].getMethod("sort", classOf[java.util.List[_]])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.Collections])
    assert(result == Map.empty)
  }

  // --- resolveTypeParamMapping: inherited from Object ---

  test("resolveTypeParamMapping.InheritedFromObject.Comparator.equals") {
    // Comparator<T> overrides equals(Object) from Object, so it's declared on Comparator.
    // The identity mapping includes Comparator's type param T.
    val method = classOf[java.util.Comparator[_]].getMethod("equals", classOf[Object])
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.Comparator[_]])
    assert(result == Map("T" -> 0))
  }

  test("resolveTypeParamMapping.InheritedFromObject.ArrayList.hashCode") {
    // hashCode is declared on Object (no type params), invoked on ArrayList<E>.
    // Object has no type params so the map should be empty.
    val method = classOf[Object].getMethod("hashCode")
    val result = JvmUtils.resolveTypeParamMapping(method, classOf[java.util.ArrayList[_]])
    assert(result == Map.empty)
  }

}
