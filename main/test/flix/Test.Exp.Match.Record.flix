mod Test.Exp.Match.Record {

    use Assert.{assertEq, assertTrue, success, fail}

    enum A {
        case A,
        case B
    }

    @Test
    def emptyRecord01(): Unit \ Assert = match { } {
        case { } => success("matched empty record")
    }

    @Test
    def emptyRecord02(): Unit \ Assert = match { } {
        case { } => success("matched empty record")
    }

    @Test
    def recordLabelValue01(): Unit \ Assert = match { a = 1 } {
        case { a } => assertEq(expected = 1, a)
    }

    @Test
    def recordLabelValue02(): Unit \ Assert = match { a = 1 } {
        case { a = b } => assertEq(expected = 1, b)
    }

    @Test
    def recordLabelValue03(): Unit \ Assert = match { a = 1 } {
        case { a = 1 } => success("matched { a = 1 }")
        case { a = _ } => fail("unexpected match on { a = _ }")
    }

    @Test
    def recordLabelValue04(): Unit \ Assert = match { a = 1, b = 2 } {
        case { a = 1, b = 2 } => success("matched { a = 1, b = 2 }")
        case { a = _, b = _ } => fail("unexpected match on { a = _, b = _ }")
    }

    @Test
    def recordLabelValue05(): Unit \ Assert = match { a = 1, b = 2 } {
        case { a = 1, b = 2 | _ } => success("matched { a = 1, b = 2 | _ }")
        case { a = _, b = _ | _ } => fail("unexpected match on { a = _, b = _ | _ }")
    }

    @Test
    def extension01(): Unit \ Assert = match { a = 1, b = 2 } {
        case { a = _ | _ } => success("matched { a = _ | _ }")
    }

    @Test
    def extension03(): Unit \ Assert = match { a = 1, b = 2 } {
        case { a = _, b = _ | _ } => success("matched { a = _, b = _ | _ }")
    }

    @Test
    def nestedRecord01(): Unit \ Assert = match { a = { } } {
        case { a = { } } => success("matched { a = { } }")
    }

    @Test
    def nestedRecord02(): Unit \ Assert = match { a = { a = { } } } {
        case { a = { a = { } } } => success("matched { a = { a = { } } }")
    }

    @Test
    def nestedRecord03(): Unit \ Assert = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } } => success("matched { a = { a = { a = { } } } }")
    }

    @Test
    def nestedRecordExtension01(): Unit \ Assert = match { a = { a = { } } } {
        case { a = { a = { } } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension02(): Unit \ Assert = match { a = { a = { } } } {
        case { a = { a = { } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension03(): Unit \ Assert = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension04(): Unit \ Assert = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension05(): Unit \ Assert = match { a = { a = { a = { } } } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension06(): Unit \ Assert = match { a = { a = { a = { } } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension07(): Unit \ Assert = match { a = { a = { a = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension08(): Unit \ Assert = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { } | _ } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension09(): Unit \ Assert = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { b = { } | _ } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension10(): Unit \ Assert = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension11(): Unit \ Assert = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ } | _ } => success("matched")
    }

    @Test
    def nestedRecordExtension12(): Unit \ Assert = match { a = { a = { a = { }, b = { } }, b = { } }, b = { } } {
        case { a = { a = { a = { }, b = { } | _ }, b = { } | _ }, b = { } | _ } => success("matched")
    }

    @Test
    def exhaustiveness01(): Unit \ Assert = match { a = 1, b = 2 } {
        case { a = _, b = _ } => success("matched")
    }

    @Test
    def exhaustiveness02(): Unit \ Assert = match { a = { a = { a = { }, b = { } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => success("matched A.A")
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => fail("unexpected match on A.B")
    }

    @Test
    def exhaustiveness03(): Unit \ Assert = match { a = { a = { a = { }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.A } } => success("matched A.A")
        case { a = { a = { a = { } | _ } | _ }, b = { x = A.B } } => fail("unexpected match on A.B")
    }

    @Test
    def exhaustiveness04(): Unit \ Assert = match { a = { a = { a = { a = A.B }, b = { b = A.B } }, b = { } }, b = { x = A.A } } {
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.A } } => fail("unexpected match on A.A, A.A")
        case { a = { a = { a = { a = A.A } | _ } | _ }, b = { x = A.B } } => fail("unexpected match on A.A, A.B")
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.A } } => success("matched A.B, A.A")
        case { a = { a = { a = { a = A.B } | _ } | _ }, b = { x = A.B } } => fail("unexpected match on A.B, A.B")
    }

    @Test
    def exhaustiveness05(): Unit \ Assert = match { a = A.A, b = A.A } {
        case { b = A.A, a = A.A } => success("matched A.A, A.A")
        case { b = A.A, a = A.B } => fail("unexpected match on A.A, A.B")
        case { b = A.B, a = A.A } => fail("unexpected match on A.B, A.A")
        case { b = A.B, a = A.B } => fail("unexpected match on A.B, A.B")
    }

    @Test
    def duplicateLabelsExhaustiveness01(): Unit \ Assert = match { a = A.A, a = A.B } {
        case { a = A.A, a = A.A } => fail("unexpected match on A.A, A.A")
        case { a = A.A, a = A.B } => success("matched A.A, A.B")
        case { a = A.B, a = A.A } => fail("unexpected match on A.B, A.A")
        case { a = A.B, a = A.B } => fail("unexpected match on A.B, A.B")
    }

    @Test
    def duplicateLabelsExhaustiveness02(): Unit \ Assert = match { a = A.B, a = A.B } {
        case { a = A.A, a = A.A } => fail("unexpected match on A.A, A.A")
        case { a = A.A, a = A.B } => fail("unexpected match on A.A, A.B")
        case { a = A.B, a = A.A } => fail("unexpected match on A.B, A.A")
        case { a = A.B, a = A.B } => success("matched A.B, A.B")
    }

    @Test
    def duplicateLabelsExhaustiveness03(): Unit \ Assert = match { a = A.B, a = A.A } {
        case { a = A.A, a = A.A } => fail("unexpected match on A.A, A.A")
        case { a = A.A, a = A.B } => fail("unexpected match on A.A, A.B")
        case { a = A.B, a = A.A } => success("matched A.B, A.A")
        case { a = A.B, a = A.B } => fail("unexpected match on A.B, A.B")
    }

    @Test
    def duplicateLabelsExhaustiveness04(): Unit \ Assert = match { a = A.B, a = A.A } {
        case { a = A.A | _ } => fail("unexpected match on A.A")
        case { a = A.B | _ } => success("matched A.B")
    }

    @Test
    def duplicateLabelsExhaustiveness05(): Unit \ Assert = match { a = A.B, a = A.A } {
        case { a = A.A, a = _ } => fail("unexpected match on A.A")
        case { a = A.B, a = _ } => success("matched A.B")
    }

    @Test
    def exhaustiveness06(): Unit \ Assert = match { a = { a = { }, b = A.B }, b = A.A } {
        case { a = { a = { } | _ } | _ } => success("matched")
    }

    @Test
    def exhaustiveness07(): Unit \ Assert = match { x = true } {
        case { x = true } => success("matched true")
        case { x = false } => success("matched false")
    }

    @Test
    def exhaustiveness08(): Unit \ Assert = match { x = true } {
        case { x = _ } => success("matched wildcard")
    }

    @Test
    def exhaustiveness09(): Unit \ Assert =
        let r: {c = A} = { c = A.A };
        match r {
            case { c = A.A } => success("matched A")
            case { c = A.B } => fail("unexpected match on B")
        }

    @Test
    def exhaustiveness10(): Unit \ Assert = match { a = true, b = false } {
        case { a = true, b = _ } => success("matched a=true")
        case { b = _, a = false } => success("matched a=false")
    }

    @Test
    def exhaustiveness11(): Unit \ Assert =
        let r: {a = Bool | _} = { a = true, b = 42 };
        match r {
            case { a = true | _ } => success("matched true")
            case { a = false | _ } => fail("unexpected match on false")
        }

    @Test
    def extensionLabels01(): Unit \ Assert = match { a = true, b = true } {
        case { a | r } => assertTrue(a and r#b)
    }

    @Test
    def duplicateLabels01(): Unit \ Assert = match { a = false, a = true } {
        case { a = x, a = y } => assertTrue((not x) and y)
    }

    @Test
    def duplicateLabels02(): Unit \ Assert = match { a = true, a = 42 } {
        case { a = x, a = y } => assertTrue(x and y == 42)
    }

    @Test
    def randomOrder01(): Unit \ Assert = match { a = true, b = 2, a = 42 } {
        case { b, a = x, a = y } => assertTrue((x and y == 42) and b == 2)
    }

    @Test
    def randomOrder02(): Unit \ Assert = match { a = true, b = 2, a = 42 } {
        case { a = x, a = y, b } => assertTrue((x and y == 42) and b == 2)
    }

    @Test
    def randomOrder03(): Unit \ Assert = match { q = true, b = 2, a = 42 } {
        case { a, q, b } => assertTrue((q and a == 42) and b == 2)
    }

    @Test
    def randomOrder04(): Unit \ Assert = match { q = true, b = 2, a = 42 } {
        case { b, a, q } => assertTrue((q and a == 42) and b == 2)
    }

    @Test
    def duplicateLabelsWithExtensionLabels01(): Unit \ Assert = match { a = false, a = true, a = 42 } {
        case { a = x, a = y | r } => assertTrue(((not x) and y) and r#a == 42)
    }

    @Test
    def duplicateLabelsWithExtensionLabels02(): Unit \ Assert = match { a = false, a = true, a = 42, a = 3 } {
        case { a = x, a = y | r } => assertTrue(((not x) and y) and r#a == 42)
    }

}
