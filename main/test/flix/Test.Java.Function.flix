mod Test.Java.Function {
    import java.lang.Object
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
    def testFunction(): Bool \ IO = {
        import static java.util.stream.Stream.of(Object): Stream \ IO;
        import java.util.stream.Stream.findFirst(): ##java.util.Optional \ IO;
        import java.util.Optional.get(): Object;
        import java.lang.Object.toString(): String;
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream = of(mkObject(42));
        toString(get(findFirst(stream))) == "42"
    }

    @Test
    def testConsumer(): Bool \ IO = {
        import static java.util.stream.Stream.of(Object): Stream \ IO;
        import java.util.stream.Stream.forEach(Consumer): Unit \ IO;
        import java.lang.Object.toString(): String;
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let st = ref "<none>" @ Static;
        let stream = of(mkObject(8));
        let _ = forEach(stream, obj -> let s1 = toString(obj); st := s1);
        deref st == "8"
    }

    @Test
    def testPredicate(): Bool \ IO = region r {
        import static java.util.stream.Stream.of(Array[Object, r]): Stream \ IO;
        import java.util.stream.Stream.filter(Predicate): Stream \ IO;
        import java.util.stream.Stream.findFirst(): ##java.util.Optional \ IO;
        import java.util.Optional.get(): Object;
        import java.lang.Object.toString(): String;
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream0 = of(Array#{mkObject(1), mkObject(2), mkObject(3), mkObject(4), mkObject(5)} @ r);
        let stream1 = filter(stream0, obj -> toString(obj) == "5");
        toString(get(findFirst(stream1))) == "5"
    }

    @Test
    def testIntFunction(): Bool \ IO = {
        import static java.util.stream.IntStream.of(Int32): IntStream \ IO;
        import java.util.stream.IntStream.mapToObj(IntFunction): Stream \ IO;
        import java.util.stream.Stream.findFirst(): ##java.util.Optional \ IO;
        import java.util.Optional.get(): Object;
        import java.lang.Object.toString(): String;
        let stream0 = of(42);
        let f = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream1 = mapToObj(stream0, f);
        toString(get(findFirst(stream1))) == "42"
    }

    @Test
    def testIntConsumer(): Bool \ IO = {
        import static java.util.stream.IntStream.range(Int32, Int32): IntStream \ IO;
        import java.util.stream.IntStream.forEach(IntConsumer): Unit \ IO;
        let st = ref 0 @ Static;
        let stream = range(0, 9);
        let _ = forEach(stream, i -> st := i);
        deref st == 8
    }

    @Test
    def testIntPredicate(): Bool \ IO = {
        import static java.util.stream.IntStream.range(Int32, Int32): IntStream \ IO;
        import java.util.stream.IntStream.filter(IntPredicate): IntStream \ IO;
        import java.util.stream.IntStream.sum(): Int32 \ IO;
        let stream0 = range(0, 9);
        let stream1 = filter(stream0, i -> checked_ecast(i `Int32.rem` 2 == 0));
        sum(stream1) == 20
    }

    @Test
    def testIntUnaryOperator(): Bool \ IO = {
        import static java.util.stream.IntStream.of(Int32): IntStream \ IO;
        import java.util.stream.IntStream.map(IntUnaryOperator): IntStream \ IO;
        import java.util.stream.IntStream.sum(): Int32 \ IO;
        let stream0 = of(5);
        let stream1 = map(stream0, i -> checked_ecast(i+7));
        sum(stream1) == 12
    }

    @Test
    def testLongFunction(): Bool \ IO = {
        import static java.util.stream.LongStream.of(Int64): LongStream \ IO;
        import java.util.stream.LongStream.mapToObj(LongFunction): Stream \ IO;
        import java.util.stream.Stream.findFirst(): ##java.util.Optional \ IO;
        import java.util.Optional.get(): Object;
        import java.lang.Object.toString(): String;
        let stream0 = of(42i64);
        let f = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream1 = mapToObj(stream0, f);
        toString(get(findFirst(stream1))) == "42"
    }

    @Test
    def testLongConsumer(): Bool \ IO = {
        import static java.util.stream.LongStream.range(Int64, Int64): LongStream \ IO;
        import java.util.stream.LongStream.forEach(LongConsumer): Unit \ IO;
        let st = ref 0i64 @ Static;
        let stream = range(0i64, 9i64);
        let _ = forEach(stream, i -> st := i);
        deref st == 8i64
    }

    @Test
    def testLongPredicate(): Bool \ IO = {
        import static java.util.stream.LongStream.range(Int64, Int64): LongStream \ IO;
        import java.util.stream.LongStream.filter(LongPredicate): LongStream \ IO;
        import java.util.stream.LongStream.sum(): Int64 \ IO;
        let stream0 = range(0i64, 9i64);
        let stream1 = filter(stream0, i -> checked_ecast(i `Int64.rem` 2i64 == 0i64));
        sum(stream1) == 20i64
    }

    @Test
    def testLongUnaryOperator(): Bool \ IO = {
        import static java.util.stream.LongStream.of(Int64): LongStream \ IO;
        import java.util.stream.LongStream.map(LongUnaryOperator): LongStream \ IO;
        import java.util.stream.LongStream.sum(): Int64 \ IO;
        let stream0 = of(5i64);
        let stream1 = map(stream0, i -> checked_ecast(i+7i64));
        sum(stream1) == 12i64
    }

    @Test
    def testDoubleFunction(): Bool \ IO = {
        import static java.util.stream.DoubleStream.of(Float64): DoubleStream \ IO;
        import java.util.stream.DoubleStream.mapToObj(DoubleFunction): Stream \ IO;
        import java.util.stream.Stream.findFirst(): ##java.util.Optional \ IO;
        import java.util.Optional.get(): Object;
        import java.lang.Object.toString(): String;
        let stream0 = of(42.0f64);
        let f = d -> new Object {
            def toString(_this: Object): String = match Float64.tryToInt32(d) {
                case Some(i) => "${i}"
                case None    => ""
           }
        };
        let stream1 = mapToObj(stream0, f);
        toString(get(findFirst(stream1))) == "42"
    }

    @Test
    def testDoubleConsumer(): Bool \ IO = {
        import static java.util.stream.DoubleStream.of(Array[Float64, Static]): DoubleStream \ IO;
        import java.util.stream.DoubleStream.forEach(DoubleConsumer): Unit \ IO;
        let st = ref 0.0f64 @ Static;
        let stream = of(Array#{0.0f64, 1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64, 7.0f64, 8.0f64} @ Static);
        let _ = forEach(stream, i -> st := i);
        let last = deref st;
        Float64.tryToInt32(last) == Some(8)
    }

    @Test
    def testDoublePredicate(): Bool \ IO = region r {
        import static java.util.stream.DoubleStream.of(Array[Float64, r]): DoubleStream \ IO;
        import java.util.stream.DoubleStream.filter(DoublePredicate): DoubleStream \ IO;
        import java.util.stream.DoubleStream.sum(): Float64 \ IO;
        let stream0 = of(Array#{0.0f64, 1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64, 7.0f64, 8.0f64} @ r);
        let stream1 = filter(stream0, d -> checked_ecast(match Float64.tryToInt32(d) {
            case Some(i) => i `Int32.rem` 2 == 0
            case None    => false
            }));
        let tot = sum(stream1);
        Float64.tryToInt32(tot) == Some(20)
    }

    @Test
    def testDoubleUnaryOperator(): Bool \ IO = {
        import static java.util.stream.DoubleStream.of(Float64): DoubleStream \ IO;
        import java.util.stream.DoubleStream.map(DoubleUnaryOperator): DoubleStream \ IO;
        import java.util.stream.DoubleStream.sum(): Float64 \ IO;
        let stream0 = of(5.0f64);
        let stream1 = map(stream0, d -> checked_ecast(d + 7.0f64));
        let tot = sum(stream1);
        Float64.tryToInt32(tot) == Some(12)
    }

}
