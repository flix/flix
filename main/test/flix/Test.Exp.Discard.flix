mod Test.Exp.Discard {

    @CompileTest
    def testBoolDiscard01(): Unit \ IO =
        let arr = Array#{true} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testCharDiscard01(): Unit \ IO =
        let arr = Array#{'q'} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testFloat32Discard01(): Unit \ IO =
        let arr = Array#{32.0f32} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testFloat64Discard01(): Unit \ IO =
        let arr = Array#{64.0f64} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testBigDecimalDiscard01(): Unit \ IO =
        let arr = Array#{9999999.0ff} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testInt8Discard01(): Unit \ IO =
        let arr = Array#{8i8} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testInt16Discard01(): Unit \ IO =
        let arr = Array#{16i16} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testInt32Discard01(): Unit \ IO =
        let arr = Array#{32i32} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testInt64Discard01(): Unit \ IO =
        let arr = Array#{64i64} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testBigIntDiscard01(): Unit \ IO =
        let arr = Array#{9999999ii} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testStringDiscard01(): Unit \ IO =
        let arr = Array#{"cool string"} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testArrayOfIntDiscard01(): Unit \ IO =
        let arr = Array#{Array#{2} @ Static} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testArrayOfStringDiscard01(): Unit \ IO =
        let arr = Array#{Array#{"cool string"} @ Static} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testTupleDiscard01(): Unit \ IO =
        let arr = Array#{("fst", "snd")} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    enum Enum {
        case Data(Int32)
    }

    @CompileTest
    def testEnumDiscard01(): Unit \ IO =
        let arr = Array#{Enum.Data(32)} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    pub enum Poly[a, b] {
            case None,
            case This(a)
            case That(b)
            case Both(a, b)
        }

    @CompileTest
    def testPolyDiscard01(): Unit \ IO =
        let arr = Array#{Poly.That("hello")} @ Static;
        discard %%ARRAY_LOAD%%(arr, 0)

    @CompileTest
    def testBuilderDiscard01(): Bool = region rc {
        let counter = Ref.fresh(rc, 0);
        def increaseBy(i, c) = {
            Ref.put(Ref.get(c) + i, c);
            c
        };
        discard counter |> increaseBy(2) |> increaseBy(5);
        Ref.get(counter) == 7
    }

}
