# Follow-up: Fix varargs Java method resolution with vector/array literals

## Context

The PR "Support Java method resolution for polymorphic types" treats rigid type
variables as known (erasing to `Object`) and removes `unchecked_cast` workarounds.
This works correctly for single-argument Java methods but breaks when a
vector/array literal is passed to a Java varargs method like `Stream.of(T...)`.

## Problem

When a `Vector` or `Array` literal containing function applications is passed to
a varargs method, the type checker incorrectly types the literal elements as the
closure type rather than the application result type.

### Minimal failing test case

```flix
import java.lang.Object
import java.util.stream.Stream

def main(): Unit \ IO =
    let mkObject: Int32 -> Object \ IO = i -> {
        new Object {
            def toString(_this: Object): String = "${i}"
        }
    };
    let v: Vector[Object] = Vector#{mkObject(1), mkObject(2), mkObject(3)};
    let stream0 = Stream.of(v);
    println(stream0.findFirst().get().toString())
```

Errors:
```
Unexpected type: expected 'Int32 -> java.lang.Object \ e0',
                   found 'Int32 -> java.lang.Object \ IO'.
```

### What works

- `Stream.of(mkObject(42))` -- single element overload `of(T t)` works fine.
- `let v: Vector[Object] = Vector#{mkObject(1), mkObject(2), mkObject(3)}` alone
  compiles and runs correctly.
- The error only appears when the vector is passed to `Stream.of`.

### Key observations

1. `Stream.of` has two overloads: `of(T t)` and `of(T... values)`.
2. `getJavaType` converts `Vector[Object]` to `Object[].class`, so
   `MethodUtils.getMatchingAccessibleMethod` picks the varargs overload.
3. `mkArgConstraints` only emits constraints for bare `TypeVariable` params;
   varargs params are `GenericArrayType` and are skipped (`case _ => None`).
4. Despite this, the constraint solver back-propagates through the vector
   literal and re-types each element as the closure type (`Int32 -> Object \ IO`)
   instead of the application result (`Object`).
5. The back-propagation introduces a flexible effect variable `e0` that
   conflicts with the concrete `IO` effect.

### Affected tests

- `main/test/flix/Test.Java.Function.flix` -- `testPredicate`, `testIntFunction`,
  `testLongFunction`, `testDoubleFunction` (14 compilation errors total).

### Where to investigate

- `ConstraintGen.scala` `InvokeStaticMethod` case (line ~953): how the
  `UnresolvedJvmType` constraint interacts with vector literal element types.
- `TypeReconstruction.scala` `getArgumentsWithVarArgs` (line ~642): how varargs
  arguments are wrapped and whether constraint generation needs to account for this.
- The interaction between `JvmToType`/`JvmToEff` type functions and collection
  literal element inference.
