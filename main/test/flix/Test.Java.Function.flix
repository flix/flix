namespace Test/Java/Function {
    import java.lang.Object
    import java.util.function.IntFunction
    import java.util.stream.IntStream
    import java.util.stream.Stream

   @Test
   def testIntFunction(): Bool \ IO = {
        import static java.util.stream.IntStream.of(Int32): IntStream \ IO;
        import java.util.stream.IntStream.mapToObj(IntFunction): Stream \ IO;
        import java.util.stream.Stream.count(): Int64 \ IO;
        import java.util.stream.Stream.distinct(): Stream \ IO;
        import new java.lang.Object(): Object \ IO as newObject;
        let stream0 = of(1);
        let stream1 = mapToObj(stream0, _ -> newObject());
        count(distinct(stream1)) == 1i64
    }
}
