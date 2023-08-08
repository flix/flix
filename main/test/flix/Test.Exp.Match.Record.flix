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

    enum A {
        case A,
        case B
    }

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
        case { a = _ | _ } => true
    }

    @test
    def testMatchRecord07(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ } => true
    }

    @test
    def testMatchRecord08(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ | _ } => true
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
        case { a = { a = { } } | _ } => true
    }

    @test
    def testMatchRecord12(): Bool = match { a = { a = { } } } {
        case { a = { a = { } | _ } | _ } => true
    }

    @test
    def testMatchRecord13(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } } => true
    }

    @test
    def testMatchRecord14(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } | _ } => true
    }

    @test
    def testMatchRecord15(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } | _ } | _ } => true
    }

    @test
    def testMatchRecord16(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testMatchRecord17(): Bool = match { a = { a = { a = { } } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testMatchRecord18(): Bool = match { a = { a = { a = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testMatchRecord19(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testMatchRecord20(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { b = { } | _ } | _ } | _ } => true
    }

    @test
    def testMatchRecord21(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ } | _ } | _ } => true
    }

    @test
    def testMatchRecord22(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ } | _ } => true
    }

    @test
    def testMatchRecord23(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ }, b = { } | _ } => true
    }

    @test
    def testMatchRecord24(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => false
    }

    @test
    def testMatchRecord25(): Bool = match { a = { a = { a = { }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => false
    }

    @test
    def testMatchRecord26(): Bool = match { a = { a = { a = { a = A.B }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.A } } => false
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.B } } => false
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.B } } => false
    }

    @test
    def testMatchRecord27(): Bool = match { a = A.A, b = A.A } {
        case { b = A.A, a = A.A } => true
        case { b = A.A, a = A.B } => false
        case { b = A.B, a = A.A } => false
        case { b = A.B, a = A.B } => false
    }

    @test
    def testMatchRecord28(): Bool = match { a = { a = { }, b = A.B }, b = A.A } {
        case { a = { a = { } | _ } | _ } => true
    }

    @test
    def testMatchRecord29(): Bool = match { a = true, b = true } {
        case { a | r } => a and r.b
    }

    @test
    def testMatchRecord30(): Bool = match { a = false, a = true } {
        case { a, a } => (not a) and a
    }

}
