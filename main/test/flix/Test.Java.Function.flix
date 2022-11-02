namespace Test/Java/Function {
    import java.lang.Object
    import java.util.function.IntFunction
    import java.util.function.IntUnaryOperator
    import java.util.function.IntPredicate
    import java.util.function.IntConsumer
    import java.util.stream.IntStream
    import java.util.function.LongFunction
    import java.util.function.LongUnaryOperator
    import java.util.function.LongPredicate
    import java.util.function.LongConsumer
    import java.util.stream.LongStream
    import java.util.stream.Stream

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
  def testIntUnaryOperator(): Bool \ IO = {
       import static java.util.stream.IntStream.of(Int32): IntStream \ IO;
       import java.util.stream.IntStream.map(IntUnaryOperator): IntStream \ IO;
       import java.util.stream.IntStream.sum(): Int32 \ IO;
       let stream0 = of(5);
       let stream1 = map(stream0, upcast(i -> i+7));
       sum(stream1) == 12
   }

  @Test
  def testIntPredicate(): Bool \ IO = {
       import static java.util.stream.IntStream.range(Int32, Int32): IntStream \ IO;
       import java.util.stream.IntStream.filter(IntPredicate): IntStream \ IO;
       import java.util.stream.IntStream.sum(): Int32 \ IO;
       let stream0 = range(0, 9);
       let stream1 = filter(stream0, upcast(i -> i mod 2 == 0));
       sum(stream1) == 20
   }

  @Test
  def testIntConsumer(): Bool \ IO = {
       import static java.util.stream.IntStream.range(Int32, Int32): IntStream \ IO;
       import java.util.stream.IntStream.forEach(IntConsumer): Unit \ IO;
       let st = ref 0;
       let stream = range(0, 9);
       let _ = forEach(stream, i -> st := i);
       deref st == 8
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
  def testLongUnaryOperator(): Bool \ IO = {
       import static java.util.stream.LongStream.of(Int64): LongStream \ IO;
       import java.util.stream.LongStream.map(LongUnaryOperator): LongStream \ IO;
       import java.util.stream.LongStream.sum(): Int64 \ IO;
       let stream0 = of(5i64);
       let stream1 = map(stream0, upcast(i -> i+7i64));
       sum(stream1) == 12i64
   }

  @Test
  def testLongPredicate(): Bool \ IO = {
       import static java.util.stream.LongStream.range(Int64, Int64): LongStream \ IO;
       import java.util.stream.LongStream.filter(LongPredicate): LongStream \ IO;
       import java.util.stream.LongStream.sum(): Int64 \ IO;
       let stream0 = range(0i64, 9i64);
       let stream1 = filter(stream0, upcast(i -> i mod 2i64 == 0i64));
       sum(stream1) == 20i64
   }

  @Test
  def testLongConsumer(): Bool \ IO = {
       import static java.util.stream.LongStream.range(Int64, Int64): LongStream \ IO;
       import java.util.stream.LongStream.forEach(LongConsumer): Unit \ IO;
       let st = ref 0i64;
       let stream = range(0i64, 9i64);
       let _ = forEach(stream, i -> st := i);
       deref st == 8i64
   }


}
