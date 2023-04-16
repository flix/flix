/*
 * Copyright 2023 Stephen Tetley
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

mod Test.Exp.Regex {

    @test
    def testRegex01(): Regex = regex""

    @test
    def testRegex02(): Regex = regex".*"

    // Double-quote is not a special regex character so it only needs a single slash
    @test
    def testRegex03(): Regex = regex"\"{3}"

    @test
    def testRegex04(): Bool =
        Regex.isMatch(regex"\"{3}", "\"\"\"")

    @test
    def testRegex05(): Bool =
        Regex.isMatch(regex"\"a*b\"", "\"aaaab\"")

    // Open-square-bracket is a special regex character so it need double slash to escape
    @test
    def testRegex06(): Bool =
        Regex.isMatch(regex"\\[{3}", "[[[")

    @test
    def testRegex07(): Bool =
        Regex.isMatch(regex"\\[a*b\\]", "[aaaab]")

    @test
    def testRegex08(): Bool =
        Regex.isMatch(regex"\\[+a*b\\]+", "[[[[aaaab]]")

    @test
    def testRegex09(): Bool =
        Regex.isMatch(regex"\\[+\"a*b\"\\]+", "[[[[\"aaaab\"]]")

    @test
    def testRegex10(): Bool =
        Regex.isMatch(regex"\\[+\\{a*b\\}\\]+", "[[[[{aaaab}]]")

    @test
    def testRegex11(): Bool =
        Regex.isMatch(regex"\\[+\\{+a*b\\}+\\]+", "[[[[{{aaaab}}]]")

    @test
    def testRegex12(): Bool =
        Regex.isMatch(regex"\\[+\\{+\"+a*b\"+\\}+\\]+", "[[[[{{\"\"\"aaaab\"\"\"}}]]")
}
