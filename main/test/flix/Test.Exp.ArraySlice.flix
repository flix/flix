mod Test.Exp.ArraySlice {

    use Assert.assertEq

    ///
    /// A polymorphic use of the intrinsic.
    ///
    def slice(rc: Region[r2], b: Int32, e: Int32, a: Array[a, r1]): Array[a, r2] \ { r1, r2 } =
        %%ARRAY_SLICE%%(rc, a, b, e)

    @Test
    def testArraySlice01(): Unit \ Assert = region rc {
        let x: Array[Int32, rc] = Array#{} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 0, 0);
        assertEq(expected = 0, %%ARRAY_LENGTH%%(y))
    }

    @Test
    def testArraySlice02(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 0, 3);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(y))
    }

    @Test
    def testArraySlice03(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 0, 3);
        assertEq(expected = 1, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 2, %%ARRAY_LOAD%%(y, 1));
        assertEq(expected = 3, %%ARRAY_LOAD%%(y, 2))
    }

    @Test
    def testArraySlice04(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 4);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(y))
    }

    @Test
    def testArraySlice05(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 4);
        assertEq(expected = 2, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3, %%ARRAY_LOAD%%(y, 1));
        assertEq(expected = 4, %%ARRAY_LOAD%%(y, 2))
    }

    @Test
    def testArraySlice06(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 2, 2);
        assertEq(expected = 0, %%ARRAY_LENGTH%%(y))
    }

    @Test
    def testArraySlice07(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 5, 5);
        assertEq(expected = 0, %%ARRAY_LENGTH%%(y))
    }

    @Test
    def testArraySlice08(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 4, 5);
        assertEq(expected = 1, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 5, %%ARRAY_LOAD%%(y, 0))
    }

    @Test
    def testArraySlice09(): Unit \ Assert = region rc {
        // The end index may exceed the length: the rest is filled with the default value.
        let x = Array#{1, 2, 3} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 0, 5);
        assertEq(expected = 5, %%ARRAY_LENGTH%%(y))
    }

    @Test
    def testArraySlice10(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 5);
        assertEq(expected = 2, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3, %%ARRAY_LOAD%%(y, 1));
        assertEq(expected = 0, %%ARRAY_LOAD%%(y, 2));
        assertEq(expected = 0, %%ARRAY_LOAD%%(y, 3))
    }

    @Test
    def testArraySlice11(): Unit \ Assert = region rc {
        let x = Array#{"a", "b", "c"} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 2, 6);
        assertEq(expected = 4, %%ARRAY_LENGTH%%(y));
        assertEq(expected = "c", %%ARRAY_LOAD%%(y, 0))
    }

    @Test
    def testArraySlice12(): Unit \ Assert = region rc {
        let x = Array#{true, false, true} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = false, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = true, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice13(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 'b', %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 'c', %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice14(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2i8, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3i8, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice15(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2i16, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3i16, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice16(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2i64, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3i64, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice17(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2.0f32, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3.0f32, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice18(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2.0f64, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3.0f64, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice19(): Unit \ Assert = region rc {
        let x = Array#{1ii, 2ii, 3ii} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2ii, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 3ii, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice20(): Unit \ Assert = region rc {
        let x = Array#{"a", "b", "c"} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = "b", %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = "c", %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice21(): Unit \ Assert = region rc {
        let x = Array#{Some(1), None, Some(3)} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = None, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = Some(3), %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice22(): Unit \ Assert = region rc {
        let x = Array#{(1, "a"), (2, "b"), (3, "c")} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = (2, "b"), %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = (3, "c"), %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice23(): Unit \ Assert = region rc {
        let x = Array#{(), (), ()} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = (), %%ARRAY_LOAD%%(y, 0))
    }

    @Test
    def testArraySlice24(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 3, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(y, 0), 0));
        assertEq(expected = 6, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(y, 1), 1))
    }

    @Test
    def testArraySlice25(): Unit \ Assert = region rc {
        let x = Array#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 3, %%VECTOR_GET%%(%%ARRAY_LOAD%%(y, 0), 0));
        assertEq(expected = 6, %%VECTOR_GET%%(%%ARRAY_LOAD%%(y, 1), 1))
    }

    @Test
    def testArraySlice26(): Unit \ Assert = region rc {
        let x = Array#{x -> x + 1, x -> x + 2, x -> x + 3} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 12, %%ARRAY_LOAD%%(y, 0)(10));
        assertEq(expected = 13, %%ARRAY_LOAD%%(y, 1)(10))
    }

    @Test
    def testArraySlice27(): Unit \ Assert = region rc {
        // The slice is a copy: a store into the slice does not change the array.
        let x = Array#{1, 2, 3} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 0, 3);
        %%ARRAY_STORE%%(y, 0, 42);
        assertEq(expected = 1, %%ARRAY_LOAD%%(x, 0));
        assertEq(expected = 42, %%ARRAY_LOAD%%(y, 0))
    }

    @Test
    def testArraySlice28(): Unit \ Assert = region rc {
        // The slice is a copy: a store into the array does not change the slice.
        let x = Array#{"a", "b", "c"} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 0, 3);
        %%ARRAY_STORE%%(x, 0, "z");
        assertEq(expected = "z", %%ARRAY_LOAD%%(x, 0));
        assertEq(expected = "a", %%ARRAY_LOAD%%(y, 0))
    }

    @Test
    def testArraySlice29(): Unit \ Assert = region rc {
        // A store into a slice of a reference array.
        let x = Array#{"a", "b", "c"} @ rc;
        let y = %%ARRAY_SLICE%%(rc, x, 1, 5);
        %%ARRAY_STORE%%(y, 3, "z");
        assertEq(expected = "b", %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = "z", %%ARRAY_LOAD%%(y, 3))
    }

    @Test
    def testArraySlice30(): Unit \ Assert = region rc1 {
        // The slice is allocated in the given region, not in the region of the array.
        let x = Array#{1, 2, 3} @ rc1;
        let n = region rc2 {
            let y: Array[Int32, rc2] = %%ARRAY_SLICE%%(rc2, x, 1, 3);
            %%ARRAY_LOAD%%(y, 0) + %%ARRAY_LOAD%%(y, 1)
        };
        assertEq(expected = 5, n)
    }

    @Test
    def testArraySlice31(): Unit \ Assert = region rc1 {
        let y: Array[String, rc1] = region rc2 {
            let x = Array#{"a", "b", "c"} @ rc2;
            %%ARRAY_SLICE%%(rc1, x, 1, 3)
        };
        assertEq(expected = "b", %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = "c", %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice32(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = %%ARRAY_SLICE%%(rc, %%ARRAY_SLICE%%(rc, x, 1, 5), 1, 3);
        assertEq(expected = 2, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 3, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 4, %%ARRAY_LOAD%%(y, 1))
    }

    @Test
    def testArraySlice33(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5} @ rc;
        let y = slice(rc, 1, 4, x);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 4, %%ARRAY_LOAD%%(y, 2))
    }

    @Test
    def testArraySlice34(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64} @ rc;
        let y = slice(rc, 1, 4, x);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(y));
        assertEq(expected = 2.0f64, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = 4.0f64, %%ARRAY_LOAD%%(y, 2))
    }

    @Test
    def testArraySlice35(): Unit \ Assert = region rc {
        let x = Array#{"a", "b", "c", "d", "e"} @ rc;
        let y = slice(rc, 1, 4, x);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(y));
        assertEq(expected = "b", %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = "d", %%ARRAY_LOAD%%(y, 2))
    }

    @Test
    def testArraySlice36(): Unit \ Assert = region rc {
        let x = Array#{Some('a'), None, Some('c'), None, Some('e')} @ rc;
        let y = slice(rc, 1, 4, x);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(y));
        assertEq(expected = None, %%ARRAY_LOAD%%(y, 0));
        assertEq(expected = Some('c'), %%ARRAY_LOAD%%(y, 1))
    }

}
