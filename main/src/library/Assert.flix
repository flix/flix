/*
 * Copyright 2022 Magnus Madsen
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
mod Assert {

    ///
    /// Asserts that `expected` must equal `actual`.
    ///
    /// Aborts execution if the two values are un-equal.
    ///
    pub def eq(expected: a, actual: a): Bool with Eq[a], ToString[a] = masked_cast({
        if (expected == actual)
            true
        else {
            use Chalk.{green, red};
            println("Assertion Error");
            println("  Expected: ${green(expected)}");
            println("  Actual:   ${red(actual)}");
            println("");
            ?assertEq
        }
    })

}
