/*
 * Copyright 2021 Magnus Madsen
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fixpoint3.Options {

    ///
    /// Prints the Datalog program before the fixpoint computation.
    ///
    pub def enableDebugging(): Bool = false

    ///
    /// Prints the input and output facts when debugging is enabled.
    ///
    pub def enableDebugPrintFacts(): Bool = false

    ///
    /// Prints the debug output to the file given by `debugFileName`.
    ///
    pub def enableDebugToFile(): Bool = true

    ///
    /// Returns the name of the debug file.
    ///
    pub def debugFileName(): String = "fixpoint_debug.txt"

    ///
    /// The arity of the B+ trees in the interpreter.
    ///
    pub def usedArity(): Int32 = 64

    ///
    /// The maximum depth of recursively spawned threads in the interpreter.
    ///
    /// To be more precise this value is the number of times the interpreter can recursively
    /// split a `RelOp.Search` into smaller workloads for threads running in parallel.
    ///
    /// Consider the program:
    /// ```
    /// search a ∈ A do
    ///     search b ∈ B do
    ///         search c ∈ C do
    ///             ...
    ///         end
    ///     end
    /// end
    /// ```
    /// Spawning `k` threads in every layer with a `RelOp.Search`s undesirable, since this
    /// leads to `k^d` threads for a nesting of depth `d`. Therefore, the interpreter is
    /// limited to only spawn threads for the `MaxParallelNesting` highest `search` `RelOp`s.
    ///
    /// For the example, if we allow a maximum of 2 parallel nestings, we would not spawn
    /// threads when evaluating `search c ∈ C do`.
    ///
    pub def parLevel(): Int32 = 2

}
