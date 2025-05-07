/*
 * Copyright 2021 Magnus Madsen
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod Fixpoint.Options {

    ///
    /// Prints the Datalog program before the fixpoint computation.
    ///
    @Internal
    pub def enableDebugging(): Bool = false

    ///
    /// Prints the input and output facts when debugging is enabled.
    ///
    @Internal
    pub def enableDebugPrintFacts(): Bool = true
    
    ///
    /// Prints the debug output to the file given by `debugFileName`.
    ///
    @Internal
    pub def enableDebugToFile(): Bool = true

    ///
    /// Returns the name of the debug file.
    ///
    @Internal
    pub def debugFileName(): String = "fixpoint_debug.txt" 

    ///
    /// Disables the join optimization phase
    ///
    pub def disableJoinOptimizer(): Bool = false

    ///
    /// Returns the seed used in the random number generator to generate
    /// the representative input used by the profiler
    ///
    @Internal
    pub def profilerFactLowerBound(): Int32 = 10

    ///
    /// Returns the seed used in the random number generator to generate
    /// the representative input used by the profiler
    ///
    @Internal
    pub def profilerSeed(): Int64 = 0i64
    
    ///
    /// Returns the minimum number of facts required for running join optimization
    ///
    @Internal
    pub def profilerMinimumFacts(): Int64 = 100i64

    ///
    /// Returns the fraction of facts used in the join profiling stage
    ///
    @Internal
    pub def profilerDiscrimination(): Float64 = 0.20

}
