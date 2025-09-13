# Writing Test Suites for the Flix Standard Library

## Test Suite Structure

* Each module `Foo` should have a corresponding test suite in a file named **`TestFoo.flix`**.
* The test suite file must define a module **`TestFoo`**.
* A test suite consists of functions annotated with **`@Test`**. These functions are called **unit tests**.

    * ⚠️ The annotation must be written as **`@Test`** (not `@test`).

## Unit Test Requirements

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

---

# Example Test Suite

```flix
mod TestRef {

    use Assert.{assertEq, assertNeq};

    @Test
    def get01(): Unit \ Assert = region rc {
        assertEq(expected = 1, Ref.get(Ref.fresh(rc, 1)))
    }

    @Test
    def get02(): Unit \ Assert = region rc {
        assertEq(expected = "a", Ref.get(Ref.fresh(rc, "a")))
    }

}
```
