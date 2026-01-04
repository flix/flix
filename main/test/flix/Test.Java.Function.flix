mod Test.Java.Function {

    use Assert.assertEq

    import java.lang.Object
    import java.util.Optional
    import java.util.function.Function
    import java.util.function.Consumer
    import java.util.function.Predicate
    import java.util.function.IntFunction
    import java.util.function.IntConsumer
    import java.util.function.IntPredicate
    import java.util.function.IntUnaryOperator
    import java.util.stream.IntStream
    import java.util.function.LongFunction
    import java.util.function.LongConsumer
    import java.util.function.LongPredicate
    import java.util.function.LongUnaryOperator
    import java.util.stream.LongStream
    import java.util.function.DoubleFunction
    import java.util.function.DoubleConsumer
    import java.util.function.DoublePredicate
    import java.util.function.DoubleUnaryOperator
    import java.util.stream.DoubleStream
    import java.util.stream.Stream

    @Test
    def testFunction(): Unit \ { Assert, IO } = {
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream = Stream.of(mkObject(42));
        assertEq(expected = "42", stream.findFirst().get().toString())
    }

    @Test
    def testConsumer(): Unit \ { Assert, IO } = {
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let st = Ref.fresh(Static, "<none>");
        let stream = Stream.of(mkObject(8));
        stream.forEach((obj: Object) -> let s1 = obj.toString(); Ref.put(s1, st));
        assertEq(expected = "8", Ref.get(st))
    }

    @Test
    def testPredicate(): Unit \ { Assert, IO } = region rc {
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream0 = Stream.of(Array#{mkObject(1), mkObject(2), mkObject(3), mkObject(4), mkObject(5)} @ rc);
        let stream1 = stream0.filter((obj: Object) -> obj.toString() == "5");
        assertEq(expected = "5", stream1.findFirst().get().toString())
    }

    @Test
    def testIntFunction(): Unit \ { Assert, IO } = {
        let stream0 = IntStream.of(42);
        let f = (i: Int32) -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream1 = stream0.mapToObj(f);
        assertEq(expected = "42", stream1.findFirst().get().toString())
    }

    @Test
    def testIntConsumer(): Unit \ { Assert, IO } = {
        let st = Ref.fresh(Static, 0);
        let stream = IntStream.range(0, 9);
        stream.forEach(i -> Ref.put(i, st));
        assertEq(expected = 8, Ref.get(st))
    }

    @Test
    def testIntPredicate(): Unit \ { Assert, IO } = {
        let stream0 = IntStream.range(0, 9);
        let stream1 = stream0.filter(i -> i `Int32.remainder` 2 == 0);
        assertEq(expected = 20, stream1.sum())
    }

    @Test
    def testIntUnaryOperator(): Unit \ { Assert, IO } = {
        let stream0 = IntStream.of(5);
        let stream1 = stream0.map(i -> checked_ecast(i+7));
        assertEq(expected = 12, stream1.sum())
    }

    @Test
    def testLongFunction(): Unit \ { Assert, IO } = {
        let stream0 = LongStream.of(42i64);
        let f = (i: Int64) -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream1 = stream0.mapToObj(f);
        assertEq(expected = "42", stream1.findFirst().get().toString())
    }

    @Test
    def testLongConsumer(): Unit \ { Assert, IO } = {
        let st = Ref.fresh(Static, 0i64);
        let stream = LongStream.range(0i64, 9i64);
        stream.forEach(i -> Ref.put(i, st));
        assertEq(expected = 8i64, Ref.get(st))
    }

    @Test
    def testLongPredicate(): Unit \ { Assert, IO } = {
        let stream0 = LongStream.range(0i64, 9i64);
        let stream1 = stream0.filter(i -> checked_ecast(i `Int64.remainder` 2i64 == 0i64));
        assertEq(expected = 20i64, stream1.sum())
    }

    @Test
    def testLongUnaryOperator(): Unit \ { Assert, IO } = {
        let stream0 = LongStream.of(5i64);
        let stream1 = stream0.map(i -> checked_ecast(i+7i64));
        assertEq(expected = 12i64, stream1.sum())
    }

    @Test
    def testDoubleFunction(): Unit \ { Assert, IO } = {
        let stream0 = DoubleStream.of(42.0f64);
        let f = d -> new Object {
            def toString(_this: Object): String = match Float64.tryToInt32(d) {
                case Some(i) => "${i}"
                case None    => ""
           }
        };
        let stream1 = stream0.mapToObj(f);
        assertEq(expected = "42", stream1.findFirst().get().toString())
    }

    @Test
    def testDoubleConsumer(): Unit \ { Assert, IO } = {
        let st = Ref.fresh(Static, 0.0f64);
        let stream = DoubleStream.of(Array#{0.0f64, 1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64, 7.0f64, 8.0f64} @ Static);
        stream.forEach(i -> Ref.put(i, st));
        let last = Ref.get(st);
        assertEq(expected = Some(8), Float64.tryToInt32(last))
    }

    @Test
    def testDoublePredicate(): Unit \ { Assert, IO } = region rc {
        let stream0 = DoubleStream.of(Array#{0.0f64, 1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64, 7.0f64, 8.0f64} @ rc);
        let stream1 = stream0.filter(d -> checked_ecast(match Float64.tryToInt32(d) {
            case Some(i) => i `Int32.remainder` 2 == 0
            case None    => false
            }));
        let tot = stream1.sum();
        assertEq(expected = Some(20), Float64.tryToInt32(tot))
    }

    @Test
    def testDoubleUnaryOperator(): Unit \ { Assert, IO } = {
        let stream0 = DoubleStream.of(5.0f64);
        let stream1 = stream0.map(d -> checked_ecast(d + 7.0f64));
        let tot = stream1.sum();
        assertEq(expected = Some(12), Float64.tryToInt32(tot))
    }

}
