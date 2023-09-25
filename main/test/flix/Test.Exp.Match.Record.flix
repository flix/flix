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
    def testEmptyRecord01(): Bool = match { } {
        case { } => true
    }

    @test
    def testEmptyRecord02(): Bool = match { } {
        case { } => true
        case _   => false
    }

    @test
    def testRecordLabelValue01(): Bool = match { a = 1 } {
        case { a } => a == 1
    }

    @test
    def testRecordLabelValue02(): Bool = match { a = 1 } {
        case { a = b } => b == 1
    }

    @test
    def testRecordLabelValue03(): Bool = match { a = 1 } {
        case { a = 1 } => true
        case { a = _ } => false
    }

    @test
    def testRecordLabelValue04(): Bool = match { a = 1, b = 2 } {
        case { a = 1, b = 2 } => true
        case { a = _, b = _ } => false
    }

    @test
    def testRecordLabelValue05(): Bool = match { a = 1, b = 2 } {
        case { a = 1, b = 2 | _ } => true
        case { a = _, b = _ | _ } => false
    }

    @test
    def testExtension01(): Bool = match { a = 1, b = 2 } {
        case { a = _ | _ } => true
    }

    @test
    def testExtension03(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ | _ } => true
    }

    @test
    def testNestedRecord01(): Bool = match { a = { } } {
        case { a = { } } => true
    }

    @test
    def testNestedRecord02(): Bool = match { a = { a = { } } } {
        case { a = { a = { } } } => true
    }

    @test
    def testNestedRecord03(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } } => true
    }

    @test
    def testNestedRecordExtension01(): Bool = match { a = { a = { } } } {
        case { a = { a = { } } | _ } => true
    }

    @test
    def testNestedRecordExtension02(): Bool = match { a = { a = { } } } {
        case { a = { a = { } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension03(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } | _ } => true
    }

    @test
    def testNestedRecordExtension04(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension05(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension06(): Bool = match { a = { a = { a = { } } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension07(): Bool = match { a = { a = { a = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension08(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension09(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { b = { } | _ } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension10(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension11(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ } | _ } => true
    }

    @test
    def testNestedRecordExtension12(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ }, b = { } | _ } => true
    }

    @test
    def testExhaustiveness01(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ } => true
    }
    @test
    def testExhaustiveness02(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => false
    }

    @test
    def testExhaustiveness03(): Bool = match { a = { a = { a = { }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => false
    }

    @test
    def testExhaustiveness04(): Bool = match { a = { a = { a = { a = A.B }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.A } } => false
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.B } } => false
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.B } } => false
    }

    @test
    def testExhaustiveness05(): Bool = match { a = A.A, b = A.A } {
        case { b = A.A, a = A.A } => true
        case { b = A.A, a = A.B } => false
        case { b = A.B, a = A.A } => false
        case { b = A.B, a = A.B } => false
    }

    @test
    def testDuplicateLabelsExhaustiveness01(): Bool = match { a = A.A, a = A.B } {
        case { a = A.A, a = A.A } => false
        case { a = A.A, a = A.B } => true
        case { a = A.B, a = A.A } => false
        case { a = A.B, a = A.B } => false
    }

    @test
    def testDuplicateLabelsExhaustiveness02(): Bool = match { a = A.B, a = A.B } {
        case { a = A.A, a = A.A } => false
        case { a = A.A, a = A.B } => false
        case { a = A.B, a = A.A } => false
        case { a = A.B, a = A.B } => true
    }

    @test
    def testDuplicateLabelsExhaustiveness03(): Bool = match { a = A.B, a = A.A } {
        case { a = A.A, a = A.A } => false
        case { a = A.A, a = A.B } => false
        case { a = A.B, a = A.A } => true
        case { a = A.B, a = A.B } => false
    }

    @test
    def testDuplicateLabelsExhaustiveness04(): Bool = match { a = A.B, a = A.A } {
        case { a = A.A | _ } => false
        case { a = A.B | _ } => true
    }

    @test
    def testDuplicateLabelsExhaustiveness05(): Bool = match { a = A.B, a = A.A } {
        case { a = A.A, a = _ } => false
        case { a = A.B, a = _ } => true
    }

    @test
    def testExhaustiveness06(): Bool = match { a = { a = { }, b = A.B }, b = A.A } {
        case { a = { a = { } | _ } | _ } => true
    }
    @test
    def testExtensionLabels01(): Bool = match { a = true, b = true } {
        case { a | r } => a and r.b
    }

    @test
    def testDuplicateLabels01(): Bool = match { a = false, a = true } {
        case { a = x, a = y } => (not x) and y
    }

    @test
    def testDuplicateLabels02(): Bool = match { a = true, a = 42 } {
        case { a = x, a = y } => x and y == 42
    }

    @test
    def testRandomOrder01(): Bool = match { a = true, b = 2, a = 42 } {
        case { b, a = x, a = y } => (x and y == 42) and b == 2
    }

    @test
    def testRandomOrder02(): Bool = match { a = true, b = 2, a = 42 } {
        case { a = x, a = y, b } => (x and y == 42) and b == 2
    }

    @test
    def testRandomOrder03(): Bool = match { q = true, b = 2, a = 42 } {
        case { a, q, b } => (q and a == 42) and b == 2
    }

    @test
    def testRandomOrder04(): Bool = match { q = true, b = 2, a = 42 } {
        case { b, a, q } => (q and a == 42) and b == 2
    }

    @test
    def testDuplicateLabelsWithExtensionLabels01(): Bool = match { a = false, a = true, a = 42 } {
        case { a = x, a = y | r } => ((not x) and y) and r.a == 42
    }

    @test
    def testDuplicateLabelsWithExtensionLabels02(): Bool = match { a = false, a = true, a = 42, a = 3 } {
        case { a = x, a = y | r } => ((not x) and y) and r.a == 42
    }

}
