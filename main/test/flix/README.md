# Main Repository for Flix Compiler Tests.

We aim to obey four rules:

1. Each test file should declare its own namespace.
2. Each test file should be independent of the standard library.
3. Each test should be given a name of the form "testFeatureX01". 
4. The test cases should cover all of the core Flix types:
    - Unit
    - Bool
    - Char
    - Float32
    - Float64
    - Int8
    - Int16
    - Int32
    - Int64
    - BigInt
    - String
    - Array of a primitive (e.g. Char and/or Int32).
    - Array of a non-primitive (e.g. Option, List, and/or Result).
    - Enums and Tuples.
    - Polymorphic enums.

A good example of this done right is `Test.Exp.Tag.flix`.
