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
    def testMatchRecord03(): Bool = match { a = 1 } {
        case { a } => a == 1
    }

    @test
    def testMatchRecord04(): Bool = match { a = 1 } {
        case { a = b } => b == 1
    }

    @test
    def testMatchRecord05(): Bool = match { a = 1 } {
        case { a = 1 } => true
        case { a = _ } => false
    }

    @test
    def testMatchRecord06(): Bool = match { a = 1, b = 2 } {
        case { a = _ | r } => true
    }

    @test
    def testMatchRecord07(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ } => true
    }

    @test
    def testMatchRecord08(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ | r } => true
    }

    @test
    def testMatchRecord09(): Bool = match { a = { } } {
        case { a = { } } => true
    }

    @test
    def testMatchRecord10(): Bool = match { a = { a = { } } } {
        case { a = { a = { } } } => true
    }

    @test
    def testMatchRecord11(): Bool = match { a = { a = { } } } {
        case { a = { a = { } } | r } => true
    }

    @test
    def testMatchRecord12(): Bool = match { a = { a = { } } } {
        case { a = { a = { } | r0 } | r1 } => true
    }

    @test
    def testMatchRecord13(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } } => true
    }

    @test
    def testMatchRecord14(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } | r } => true
    }

    @test
    def testMatchRecord15(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } | r0 } | r1 } => true
    }

    @test
    def testMatchRecord16(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } | r0 } | r1 } | r2 } => true
    }

    @test
    def testMatchRecord17(): Bool = match { a = { a = { a = { } } }, b = { } } {
        case { a = { a = { a = { } | r0 } | r1 } | r2 } => true
    }

    @test
    def testMatchRecord18(): Bool = match { a = { a = { a = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | r0 } | r1 } | r2 } => true
    }

    @test
    def testMatchRecord19(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | r0 } | r1 } | r2 } => true
    }

    /*

    @test
    def testMatchRecord07(): Bool = match { a = 3 } {
        case { a = 1 } => false
        case { a = 2 } => false
        case { a = 3 } => true
        case { a = _ } => false
    }

    @test
    def testMatchRecord08(): Bool = match { a = 3 } {
        case { a = 1 } => false
        case { a = 2 } => false
        case { a = 3 } => true
        case { a = 4 } => false
        case { a = _ } => false
    }

    @test
    def testMatchRecord09(): Bool = match { a = 1 } {
        case { a | r } => a == 1
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
