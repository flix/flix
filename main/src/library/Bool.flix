/*
 * Copyright 2020 Magnus Madsen
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
namespace Bool {

    ///
    /// Alias for logical conjunction.
    ///
    @Time(1) @Space(1)
    pub def ∧(x: Bool, y: Bool): Bool = x and y

    ///
    /// Alias for logical disjunction.
    ///
    @Time(1) @Space(1)
    pub def ∨(x: Bool, y: Bool): Bool = x or y

    ///
    /// Alias for logical implication.
    ///
    @Time(1) @Space(1)
    pub def →(x: Bool, y: Bool): Bool = not x ∨ y

    ///
    /// Alias for logical bi-implication.
    ///
    @Time(1) @Space(1)
    pub def ↔(x: Bool, y: Bool): Bool = (x → y) ∧ (y → x)

    ///
    /// Alias for exclusive or.
    ///
    @Time(1) @Space(1)
    pub def ⊕(x: Bool, y: Bool): Bool = (x ∨ y) ∧ not (x ∧ y)

}