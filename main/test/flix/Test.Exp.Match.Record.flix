/*
 *  Copyright 2023 Jakob Schneider Villumsen
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

mod Test.Exp.Match.Record {

    @test
    def testMatchRecord01(): Bool = match { } {
        case { } => true
    }

    @test
    def testMatchRecord02(): Bool = match { } {
        case { } => true
        case _   => false
    }

    @test
    def testMatchRecord03(): Bool = match { x = 1 } {
        case { x } => x == 1
    }

    @test
    def testMatchRecord04(): Bool = match { x = 1 } {
        case { x = y } => y == 1
    }

    @test
    def testMatchRecord05(): Bool = match { x = 1 } {
        case { x = 1 } => true
        case { x = _ } => false
    }

    @test
    def testMatchRecord06(): Bool = match { x = 1 } {
        case { x = 2 } => false
        case { x = 1 } => true
        case _ => false
    }

    @test
    def testMatchRecord07(): Bool = match { x = 3 } {
        case { x = 1 } => false
        case { x = 2 } => false
        case { x = 3 } => true
        case { x = _ } => false
    }

    @test
    def testMatchRecord08(): Bool = match { x = 3 } {
        case { x = 1 } => false
        case { x = 2 } => false
        case { x = 3 } => true
        case { x = 4 } => false
        case { x = _ } => false
    }

    @test
    def testMatchRecord09(): Bool = match { x = 1 } {
        case { x | r } => x == 1
    }

    @test
    def testMatchRecord10(): Bool = match { a = 1, b = "", c = 42 } {
        case { b = "" | r } => true
        case { b = _  | r } => false
        case { b = _  , a = _ , c = _ } => false
    }

    @test
    def testMatchRecord11(): Bool = match { a = { b = 1 } } {
        case { a = { b = 1 } } => true
        case { a = { b = _ } } => false
    }

    @test
    def testMatchRecord12(): Bool = match { a = { b = 1 } } {
        case { a = { b = 1 } | r } => true
        case { a = { b = _ } | r } => false
    }

    @test
    def testMatchRecord13(): Bool = match { a = { b = 1 }, c = false } {
        case { a = { b = 1 } | r } => true
        case { a = { b = _ } | r } => false
    }

    @test
    def testMatchRecord14(): Bool = match { a = { b = A.B(1) } } {
        case { a = { b = A.B(1) } } => true
        case { a = { b = A.B(_) } } => false
        case { a = { b = A.C } }    => false
    }

    enum A {
        case B(Int32),
        case C
    }

    @test
    def testMatchRecord15(): Bool = match { a = 1 } {
        case { a : Int32 = 2 } => false
        case { a : Int32 = _ } => true
    }

    @test
    def testMatchRecord16(): Bool = match { a = 100 } {
        case { a : Int32 } => a == 100
    }

    @test
    def testMatchRecord17(): Bool =
        let rec = { c = "hello" };
        let { c } = rec;
        c == "hello"

    @test
    def testMatchRecord18(): Bool = match { a = { a = { a = true } } } {
        case { a = { a = { a } } } => a
    }

    /*
    @test
    def brokenPatternExhaustiveness01(): Bool = match { a = { b = 1 } } {
        case { a = { b = 1 } | r } => true
        case { a = { b = _ } } => false
    }

    @test
    def brokenPatternExhaustiveness02(): Bool = match { a = { b = 1 } } {
        case { a = { b = 1 | r1 } | r } => true
        case { a = { b = _ } } => false
    }
    */

}
