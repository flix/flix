mod Test.Exp.ArrayLoad {

    use Assert.assertEq

    @Test
    def testArrayLoad01(): Unit \ Assert = region rc {
        let x = Array#{1} @ rc;
        assertEq(expected = 1, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad02(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3} @ rc;
        assertEq(expected = 3, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad03(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4} @ rc;
        assertEq(expected = 3, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad04(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        assertEq(expected = 11, %%ARRAY_LOAD%%(x, 10))
    }

    @Test
    def testArrayLoad05(): Unit \ Assert = region rc {
        let x = Array#{1, 2} @ rc;
        assertEq(expected = 2, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad06(): Unit \ Assert = region rc {
        let x = Array#{true} @ rc;
        assertEq(expected = true, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad07(): Unit \ Assert = region rc {
        let x = Array#{false} @ rc;
        assertEq(expected = false, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad08(): Unit \ Assert = region rc {
        let x = Array#{true, false} @ rc;
        assertEq(expected = false, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad9(): Unit \ Assert = region rc {
        let x = Array#{false, true, false} @ rc;
        assertEq(expected = false, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad10(): Unit \ Assert = region rc {
        let x = Array#{false, false, false, false, true} @ rc;
        assertEq(expected = false, %%ARRAY_LOAD%%(x, 3))
    }

    @Test
    def testArrayLoad11(): Unit \ Assert = region rc {
        let x = Array#{'c'} @ rc;
        assertEq(expected = 'c', %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad12(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b'} @ rc;
        assertEq(expected = 'a', %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad13(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        assertEq(expected = 'b', %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad14(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        assertEq(expected = 'j', %%ARRAY_LOAD%%(x, 9))
    }

    @Test
    def testArrayLoad15(): Unit \ Assert = region rc {
        let x = Array#{1i8} @ rc;
        assertEq(expected = 1i8, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad16(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        assertEq(expected = 2i8, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad17(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        assertEq(expected = 3i8, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad18(): Unit \ Assert = region rc {
        let x = Array#{1i16} @ rc;
        assertEq(expected = 1i16, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad19(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        assertEq(expected = 2i16, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad20(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        assertEq(expected = 3i16, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad21(): Unit \ Assert = region rc {
        let x = Array#{1i64} @ rc;
        assertEq(expected = 1i64, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad22(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        assertEq(expected = 2i64, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad23(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        assertEq(expected = 3i64, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad24(): Unit \ Assert = region rc {
        let x = Array#{1.0f32} @ rc;
        assertEq(expected = 1.0f32, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad25(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        assertEq(expected = 2.0f32, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad26(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        assertEq(expected = 3.0f32, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad27(): Unit \ Assert = region rc {
        let x = Array#{1.0f64} @ rc;
        assertEq(expected = 1.0f64, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad28(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        assertEq(expected = 2.0f64, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad29(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        assertEq(expected = 4.0f64, %%ARRAY_LOAD%%(x, 3))
    }

    @Test
    def testArrayLoad30(): Unit \ Assert = region rc {
        let x = Array#{"Hello"} @ rc;
        assertEq(expected = "Hello", %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayLoad31(): Unit \ Assert = region rc {
        let x = Array#{"Hello", "World"} @ rc;
        assertEq(expected = "World", %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad32(): Unit \ Assert = region rc {
        let x = Array#{"World", "Hello", "World"} @ rc;
        assertEq(expected = "World", %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayLoad33(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc} @ rc;
        assertEq(expected = 2, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayLoad34(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        assertEq(expected = 1, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 0))
    }

    @Test
    def testArrayLoad35(): Unit \ Assert = region rc {
        let x = Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        assertEq(expected = 6, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 5))
    }

    @Test
    def testArrayLoad36(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        assertEq(expected = 4, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 3))
    }

    @Test
    def testArrayLoad37(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        assertEq(expected = 3, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 2))
    }

    @Test
    def testArrayLoad38(): Unit \ Assert = region rc {
        let x = Array#{Array#{true, false, true} @ rc} @ rc;
        assertEq(expected = true, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 2))
    }

    @Test
    def testArrayLoad39(): Unit \ Assert = region rc {
        let x = Array#{Array#{true, true} @ rc, Array#{true, false} @ rc} @ rc;
        assertEq(expected = false, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad40(): Unit \ Assert = region rc {
        let x = Array#{Array#{true} @ rc, Array#{true, false, true, false} @ rc} @ rc;
        assertEq(expected = true, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 2))
    }

    @Test
    def testArrayLoad41(): Unit \ Assert = region rc {
        let x = Array#{Array#{true, false, true} @ rc, Array#{true, false, true} @ rc, Array#{true, false, true} @ rc} @ rc;
        assertEq(expected = true, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 2))
    }

    @Test
    def testArrayLoad42(): Unit \ Assert = region rc {
        let x = Array#{Array#{'a', 'b', 'c'} @ rc} @ rc;
        assertEq(expected = 'b', %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayLoad43(): Unit \ Assert = region rc {
        let x = Array#{Array#{'a', 'b'} @ rc, Array#{'a', 'b'} @ rc} @ rc;
        assertEq(expected = 'b', %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad44(): Unit \ Assert = region rc {
        let x = Array#{Array#{'a'} @ rc, Array#{'a', 'b', 'c', 'd', 'e'} @ rc} @ rc;
        assertEq(expected = 'b', %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad45(): Unit \ Assert = region rc {
        let x = Array#{Array#{'a', 'b', 'c', 'd', 'e'} @ rc, Array#{'a'} @ rc} @ rc;
        assertEq(expected = 'd', %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 3))
    }

    @Test
    def testArrayLoad46(): Unit \ Assert = region rc {
        let x = Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc} @ rc;
        assertEq(expected = 'b', %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 1))
    }

    @Test
    def testArrayLoad47(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        assertEq(expected = 2i8, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayLoad48(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i8, 2i8} @ rc, Array#{1i8, 2i8} @ rc} @ rc;
        assertEq(expected = 1i8, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 0))
    }

    @Test
    def testArrayLoad49(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i8, 2i8, 3i8, 4i8} @ rc,Array#{1i8} @ rc} @ rc;
        assertEq(expected = 4i8, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 3))
    }

    @Test
    def testArrayLoad50(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i8} @ rc, Array#{1i8, 2i8, 3i8, 4i8} @ rc} @ rc;
        assertEq(expected = 4i8, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 3))
    }

    @Test
    def testArrayLoad51(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        assertEq(expected = 3i8, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 2))
    }

    @Test
    def testArrayLoad52(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        assertEq(expected = 2i16, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayLoad53(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i16, 2i16} @ rc, Array#{1i16, 2i16} @ rc} @ rc;
        assertEq(expected = 2i16, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad54(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i16, 2i16, 3i16, 4i16} @ rc, Array#{1i16} @ rc} @ rc;
        assertEq(expected = 2i16, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayLoad55(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i16} @ rc, Array#{1i16, 2i16, 3i16, 4i16} @ rc} @ rc;
        assertEq(expected = 2i16, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad56(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        assertEq(expected = 3i16, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 2))
    }

    @Test
    def testArrayLoad57(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        assertEq(expected = 3i64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 2))
    }

    @Test
    def testArrayLoad58(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i64, 2i64} @ rc, Array#{1i64, 2i64} @ rc} @ rc;
        assertEq(expected = 2i64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad59(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i64} @ rc, Array#{1i64, 2i64, 3i64, 4i64} @ rc} @ rc;
        assertEq(expected = 3i64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 2))
    }

    @Test
    def testArrayLoad60(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i64, 2i64, 3i64, 4i64} @ rc, Array#{1i64} @ rc} @ rc;
        assertEq(expected = 1i64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 0))
    }

    @Test
    def testArrayLoad61(): Unit \ Assert = region rc {
        let x = Array#{Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        assertEq(expected = 1i64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 0))
    }

    @Test
    def testArrayLoad62(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ rc} @ rc;
        assertEq(expected = 1.0f32, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 0))
    }

    @Test
    def testArrayLoad63(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32} @ rc, Array#{1.0f32, 2.0f32} @ rc} @ rc;
        assertEq(expected = 2.0f32, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad64(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc} @ rc;
        assertEq(expected = 3.0f32, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 2))
    }

    @Test
    def testArrayLoad65(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc, Array#{1.0f32} @ rc} @ rc;
        assertEq(expected = 3.0f32, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 2))
    }

    @Test
    def testArrayLoad66(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32} @ rc} @ rc;
        assertEq(expected = 3.0f32, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 2))
    }

    @Test
    def testArrayLoad67(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        assertEq(expected = 1.0f64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 0))
    }

    @Test
    def testArrayLoad68(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64} @ rc, Array#{1.0f64, 2.0f64} @ rc} @ rc;
        assertEq(expected = 2.0f64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad69(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc, Array#{1.0f64} @ rc} @ rc;
        assertEq(expected = 2.0f64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayLoad70(): Unit \ Assert = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        assertEq(expected = 3.0f64, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 2))
    }

    @Test
    def testArrayLoad71(): Unit \ Assert = region rc {
        let x = Array#{Array#{"Hello", "Hello", "Hello"} @ rc} @ rc;
        assertEq(expected = "Hello", %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 0))
    }

    @Test
    def testArrayLoad72(): Unit \ Assert = region rc {
        let x = Array#{Array#{"Hello", "Hello"} @ rc, Array#{"World", "World"} @ rc} @ rc;
        assertEq(expected = "World", %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 1))
    }

    @Test
    def testArrayLoad73(): Unit \ Assert = region rc {
        let x = Array#{Array#{"Hello"} @ rc, Array#{"World", "World", "World", "World"} @ rc} @ rc;
        assertEq(expected = "World", %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 3))
    }

    @Test
    def testArrayLoad74(): Unit \ Assert = region rc {
        let x = Array#{Array#{"World", "World", "World", "World"} @ rc, Array#{"Hello"} @ rc} @ rc;
        assertEq(expected = "World", %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 3))
    }

    @Test
    def testArrayLoad75(): Unit \ Assert = region rc {
        let x = Array#{Array#{"World", "World", "World"} @ rc, Array#{"World", "World", "World"} @ rc, Array#{"World", "World", "World"} @ rc} @ rc;
        assertEq(expected = "World", %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 2))
    }

    @Test
    def testArrayLoad77(): Unit \ Assert = region rc {
        let x = Array#{Some(5), Some(6), Some(7)} @ rc;
        assertEq(expected = Some(6), %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayLoad78(): Unit \ Assert = region rc {
        let x = Array#{Array#{None, Some(4), Some(999)} @ rc, Array#{Some(9), Some(5), None, None} @ rc} @ rc;
        assertEq(expected = None, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 2))
    }

    @Test
    def testArrayLoad79(): Unit \ Assert = region rc {
        let x = Array#{Array#{1 :: Nil} @ rc, Array#{3 :: Nil} @ rc} @ rc;
        assertEq(expected = 1 :: Nil, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 0))
    }

}
