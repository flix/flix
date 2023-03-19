/*
 *  Copyright 2023 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace TestRegexPattern {

    /////////////////////////////////////////////////////////////////////////////
    // ToString.toString                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        ToString.toString(Regex/Pattern.unmatchable()) != ""

    /////////////////////////////////////////////////////////////////////////////
    // compile                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compile01(): Bool =
        Regex/Pattern.compile("\\p{Alpha}+") |> Result.isOk

    @test
    def compile02(): Bool =
        Regex/Pattern.compile("\\p{Alpha+") |> Result.isErr

    /////////////////////////////////////////////////////////////////////////////
    // compileWithFlags                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compileWithFlags01(): Bool =
        use Regex/Pattern/Flag.{CaseInsenstive};
        Regex/Pattern.compileWithFlags("\\p{Alpha}+", Set#{CaseInsenstive}) |> Result.isOk

    @test
    def compileWithFlags02(): Bool =
        use Regex/Pattern/Flag.{CaseInsenstive};
        Regex/Pattern.compileWithFlags("\\p{Alpha+", Set#{CaseInsenstive}) |> Result.isErr


    @test
    def compileWithFlags03(): Bool =
        use Regex/Pattern/Flag.{CaseInsenstive, Multiline};
        Regex/Pattern.compileWithFlags("\\p{Alpha}+", Set#{CaseInsenstive, Multiline}) |> Result.isOk


    /////////////////////////////////////////////////////////////////////////////
    // unmatchable                                                             //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def unmatchable01(): Bool =
        Regex/Pattern.unmatchable() |> Regex/Pattern.pattern == "^\\b$"

    /////////////////////////////////////////////////////////////////////////////
    // quote                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def quote01(): Bool =
        Regex/Pattern.quote("ab") == "\\Qab\\E"

    @test
    def quote02(): Bool =
        Regex/Pattern.quote("a*b") == "\\Qa*b\\E"

    /////////////////////////////////////////////////////////////////////////////
    // pattern                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def pattern01(): Bool =
        Result.map(Regex/Pattern.pattern, Regex/Pattern.compile("a*b")) == Ok("a*b")

    @test
    def pattern02(): Bool =
        Result.map(Regex/Pattern.pattern, Regex/Pattern.compile("\\p{Blank}+")) == Ok("\\p{Blank}+")

    /////////////////////////////////////////////////////////////////////////////
    // flags                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def flags01(): Bool =
        Result.map(Regex/Pattern.flags, Regex/Pattern.compile("\\p{Blank}+")) == Ok(Set#{})

    @test
    def flags02(): Bool =
        Result.map(Regex/Pattern.flags, Regex/Pattern.compileWithFlags("\\p{Blank}+", Set#{})) == Ok(Set#{})

    @test
    def flags03(): Bool =
        use Regex/Pattern/Flag.{CaseInsenstive};
        Result.map(Regex/Pattern.flags, Regex/Pattern.compileWithFlags("\\p{Blank}", Set#{CaseInsenstive})) == Ok(Set#{CaseInsenstive})

    @test
    def flags04(): Bool =
        use Regex/Pattern/Flag.{CaseInsenstive, Multiline};
        Result.map(Regex/Pattern.flags, Regex/Pattern.compileWithFlags("\\p{Blank}", Set#{CaseInsenstive, Multiline})) == Ok(Set#{CaseInsenstive, Multiline})

}
