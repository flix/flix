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

    @Test
    def testEmptyRecord01(): Bool = match { } {
        case { } => true
    }

    @Test
    def testEmptyRecord02(): Bool = match { } {
        case { } => true
        case _   => false
    }

    @Test
    def testRecordFieldValue01(): Bool = match { a = 1 } {
        case { a } => a == 1
    }

    @Test
    def testRecordFieldValue02(): Bool = match { a = 1 } {
        case { a = b } => b == 1
    }

    @Test
    def testRecordFieldValue03(): Bool = match { a = 1 } {
        case { a = 1 } => true
        case { a = _ } => false
    }

    @Test
    def testRecordFieldValue04(): Bool = match { a = 1, b = 2 } {
        case { a = 1, b = 2 } => true
        case { a = _, b = _ } => false
    }

    @Test
    def testRecordFieldValue05(): Bool = match { a = 1, b = 2 } {
        case { a = 1, b = 2 | _ } => true
        case { a = _, b = _ | _ } => false
    }

    @Test
    def testExtension01(): Bool = match { a = 1, b = 2 } {
        case { a = _ | _ } => true
    }

    @Test
    def testExtension03(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ | _ } => true
    }

    @Test
    def testNestedRecord01(): Bool = match { a = { } } {
        case { a = { } } => true
    }

    @Test
    def testNestedRecord02(): Bool = match { a = { a = { } } } {
        case { a = { a = { } } } => true
    }

    @Test
    def testNestedRecord03(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } } => true
    }

    @Test
    def testNestedRecordExtension01(): Bool = match { a = { a = { } } } {
        case { a = { a = { } } | _ } => true
    }

    @Test
    def testNestedRecordExtension02(): Bool = match { a = { a = { } } } {
        case { a = { a = { } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension03(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } | _ } => true
    }

    @Test
    def testNestedRecordExtension04(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension05(): Bool = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension06(): Bool = match { a = { a = { a = { } } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension07(): Bool = match { a = { a = { a = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension08(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension09(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { b = { } | _ } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension10(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension11(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ } | _ } => true
    }

    @Test
    def testNestedRecordExtension12(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ }, b = { } | _ } => true
    }

    @Test
    def testExhaustiveness01(): Bool = match { a = 1, b = 2 } {
        case { a = _, b = _ } => true
    }
    @Test
    def testExhaustiveness02(): Bool = match { a = { a = { a = { }, b = { } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => false
    }

    @Test
    def testExhaustiveness03(): Bool = match { a = { a = { a = { }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => false
    }

    @Test
    def testExhaustiveness04(): Bool = match { a = { a = { a = { a = A.B }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.A } } => false
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.B } } => false
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.A } } => true
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.B } } => false
    }

    @Test
    def testExhaustiveness05(): Bool = match { a = A.A, b = A.A } {
        case { b = A.A, a = A.A } => true
        case { b = A.A, a = A.B } => false
        case { b = A.B, a = A.A } => false
        case { b = A.B, a = A.B } => false
    }

    @Test
    def testDuplicateFieldsExhaustiveness01(): Bool = match { a = A.A, a = A.B } {
        case { a = A.A, a = A.A } => false
        case { a = A.A, a = A.B } => true
        case { a = A.B, a = A.A } => false
        case { a = A.B, a = A.B } => false
    }

    @Test
    def testDuplicateFieldsExhaustiveness02(): Bool = match { a = A.B, a = A.B } {
        case { a = A.A, a = A.A } => false
        case { a = A.A, a = A.B } => false
        case { a = A.B, a = A.A } => false
        case { a = A.B, a = A.B } => true
    }

    @Test
    def testDuplicateFieldsExhaustiveness03(): Bool = match { a = A.B, a = A.A } {
        case { a = A.A, a = A.A } => false
        case { a = A.A, a = A.B } => false
        case { a = A.B, a = A.A } => true
        case { a = A.B, a = A.B } => false
    }

    @Test
    def testDuplicateFieldsExhaustiveness04(): Bool = match { a = A.B, a = A.A } {
        case { a = A.A | _ } => false
        case { a = A.B | _ } => true
    }

    @Test
    def testDuplicateFieldsExhaustiveness05(): Bool = match { a = A.B, a = A.A } {
        case { a = A.A, a = _ } => false
        case { a = A.B, a = _ } => true
    }

    @Test
    def testExhaustiveness06(): Bool = match { a = { a = { }, b = A.B }, b = A.A } {
        case { a = { a = { } | _ } | _ } => true
    }
    @Test
    def testExtensionFields01(): Bool = match { a = true, b = true } {
        case { a | r } => a and r.b
    }

    @Test
    def testDuplicateFields01(): Bool = match { a = false, a = true } {
        case { a = x, a = y } => (not x) and y
    }

    @Test
    def testDuplicateFields02(): Bool = match { a = true, a = 42 } {
        case { a = x, a = y } => x and y == 42
    }

    @Test
    def testRandomOrder01(): Bool = match { a = true, b = 2, a = 42 } {
        case { b, a = x, a = y } => (x and y == 42) and b == 2
    }

    @Test
    def testRandomOrder02(): Bool = match { a = true, b = 2, a = 42 } {
        case { a = x, a = y, b } => (x and y == 42) and b == 2
    }

    @Test
    def testRandomOrder03(): Bool = match { q = true, b = 2, a = 42 } {
        case { a, q, b } => (q and a == 42) and b == 2
    }

    @Test
    def testRandomOrder04(): Bool = match { q = true, b = 2, a = 42 } {
        case { b, a, q } => (q and a == 42) and b == 2
    }

    @Test
    def testDuplicateFieldsWithExtensionFields01(): Bool = match { a = false, a = true, a = 42 } {
        case { a = x, a = y | r } => ((not x) and y) and r.a == 42
    }

    @Test
    def testDuplicateFieldsWithExtensionFields02(): Bool = match { a = false, a = true, a = 42, a = 3 } {
        case { a = x, a = y | r } => ((not x) and y) and r.a == 42
    }

}
