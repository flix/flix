# Flix Compiler Tests.

## Unit Test Requirements

* A unit test must be annotated with either `@CompileTest` or `@Test`.
  - Use `@CompileTest` for functions that are only required to compile.
  - Use `@Test` for everything else.

* A unit test function must:

    * Have return type **`Unit`**.
    * Have effect **`Assert`**.
    * Contain one or more **assertions**.

* An **assertion** is a call to a function such as `assertEq`, `assertTrue`, etc., imported from the `Assert` module.

* Unit tests for a function `foo` should be named sequentially: `foo01`, `foo02`, `foo03`, ...

* Unit tests may sometimes include types in their names: `minValue_Bool01`, `maxValueInt8`, ...

* Do not add any copyright headers. If your code is sophisticated enough to require copyright then it is not simple enough.

## Running the Compiler

* Run `java -jar build/libs/flix.jar <filename.flix>` to ensure that the tests compile.

---

# Using the `Assert` Module

* The Flix **`Assert`** module provides a variety of assertion functions, including:

    * `assertEq`
    * `assertNeq`
    * `assertTrue`
    * `assertFalse`
    * You can find the available assertions in `Assert.flix`.

* The form of `assertEq` (and similar functions) is:

    * `assertEq(expected = <expected>, <actual>)`, i.e. the first argument is labelled and the second is not.

* Prefer the most specific assertion available. For example:

    * Use `assertEq(x, y)` instead of `assertTrue(x == y)`.
    * Use `assertSome(o)` instead of `assertTrue(Option.nonEmpty(o))`.
    * Use `assertSome(o)` instead of `assertTrue(o != None)`.
    * Use `fail(<reason>)` instead of `assertTrue(false)`.
    * Avoid over-use of `assertTrue`. Use one of the above assertions instead.
    * Avoid use of `assertTrue(true)`. It does nothing.
