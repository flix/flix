mod Test.Dec.AssocEff {

    eff Time {
        def getTime(): String
    }

    ////////////////
    // Arithmetic //
    ////////////////

    trait Add[a] {
        pub def add(x: a, y: a): a
    }

    trait Sub[a] {
        pub def sub(x: a, y: a): a
    }

    trait Mul[a] {
        pub def mul(x: a, y: a): a
    }

    trait Div[a] {
        type Aef: Eff
        pub def div(x: a, y: a): a \ Div.Aef[a]
    }

    trait FromInt32[a] {
        pub def fromInt32(x: Int32): a
    }

    instance Add[Int32] {
        pub def add(x: Int32, y: Int32): Int32 = $INT32_ADD$(x, y)
    }

    instance Div[Int32] {
        type Aef = {}
        pub def div(x: Int32, y: Int32): Int32 = $INT32_DIV$(x, y)
    }

    instance FromInt32[Int32] {
        pub def fromInt32(x: Int32): Int32 = x
    }

    def avg(l: List[t]): t \ Div.Aef[t] with Add[t], Div[t], FromInt32[t] =
        List.foldLeft(Add.add, FromInt32.fromInt32(0), l) `Div.div` FromInt32.fromInt32(List.length(l))

    @Test
    pub def testAvg(): Bool = avg(List#{1, 2, 3}) == 2

    //////////////
    // Iterator //
    //////////////

    trait Iterable[t: Type] {
        type Elm: Type
        type Aef: Eff
        pub def iterator(rc: Region[r], t: t): Iterator[Iterable.Elm[t], r + Iterable.Aef[t], r] \ r + Iterable.Aef[t]
    }

    instance Iterable[List[a]] {
        type Elm = a
        type Aef = {}
        pub def iterator(rc: Region[r], l: List[a]): Iterator[a, r, r] \ r = {
            List.iterator(rc, l)
        }
    }

    instance Iterable[MutList[a, r1]] {
        type Elm = a
        type Aef = r1
        pub def iterator(rc: Region[r], l: MutList[a, r1]): Iterator[a, r + r1, r] \ r + r1 = {
            MutList.iterator(rc, l)
        }
    }

    instance Iterable[Map[k, v]] {
        type Elm = (k, v)
        type Aef = {}
        pub def iterator(rc: Region[r], l: Map[k, v]): Iterator[(k, v), r, r] \ r = {
            Map.iterator(rc, l)
        }
    }

    instance Iterable[MutMap[k, v, r1]] {
        type Elm = (k, v)
        type Aef = r1
        pub def iterator(rc: Region[r], l: MutMap[k, v, r1]): Iterator[(k, v), r + r1, r] \ r + r1 = {
            MutMap.iterator(rc, l)
        }
    }

    eff Console {
        def println(s: String): Unit
    }

    pub def serveDessert(): Unit \ {Console} = region rc {
        let fruits = List#{"Apple", "Peach", "Mango"};
        let creams = MutList.empty(rc);
        MutList.push("Caramel", creams);
        MutList.push("Vanilla", creams);
        foreach(fruit <- fruits; cream <- creams)
            Console.println("${fruit} topped with ${cream}")
    }

    trait DeepIterable[a] {
        type Elm
        type Aef: Eff
        pub def iterator(rc: Region[r], x: a): Iterator[DeepIterable.Elm[a], r + DeepIterable.Aef[a], r] \ r + DeepIterable.Aef[a]
    }

    instance DeepIterable[Int32] {
        type Elm = Int32
        type Aef = {}
        pub def iterator(rc: Region[r], x: Int32): Iterator[Int32, r, r] \ r = Iterator.singleton(rc, x)
    }

    enum LoudInt32(Int32) with Eq

    instance DeepIterable[LoudInt32] {
        type Elm = Int32
        type Aef = Console
        pub def iterator(rc: Region[r], x: LoudInt32): Iterator[Int32, r + Console, r] \ r + Console = {
            let LoudInt32.LoudInt32(i) = x;
            Console.println("${i}!");
            Iterator.singleton(rc, i) |> Iterator.map(y -> {Console.println("${y}!"); y})
        }
    }

    instance DeepIterable[List[a]] with DeepIterable[a] {
        type Elm = DeepIterable.Elm[a]
        type Aef = DeepIterable.Aef[a]
        pub def iterator(rc: Region[r], x: List[a]): Iterator[DeepIterable.Elm[a], r + DeepIterable.Aef[a], r] \ r + DeepIterable.Aef[a] = {
            // this is needed because the nested Aef isn't actually executed when we flatMap
            let _ = (checked_ecast(123): _ \ DeepIterable.Aef[a]);
            List.iterator(rc, x) |> Iterator.flatMap(DeepIterable.iterator(rc))

        }
    }

    @Test
    pub def testDeepIterableInt32_01(): Bool = region rc {
        let x1 = 123;
        let it = DeepIterable.iterator(rc, x1);
        let l2 = Iterator.toList(it);
        List#{123} == l2
    }

    @Test
    pub def testDeepIterableInt32_02(): Bool = region rc {
        let l1 = List#{1, 2, 3};
        let it = List.iterator(rc, l1);
        let it2 = Iterator.flatMap(DeepIterable.iterator(rc), it);
        let l2 = Iterator.toList(it2);
        l1 == l2
    }

//    @Test
    pub def testDeepIterableLoudInt32_01(): Bool \ Console = region rc {
        let x1 = LoudInt32.LoudInt32(123);
        let it = DeepIterable.iterator(rc, x1);
        let l2 = Iterator.toList(it);
        List#{123} == l2
    }

    @Test
    pub def testDeepIterableInt32(): Bool = region rc {
        let l1 = List#{1, 2, 3};
        let it = DeepIterable.iterator(rc, l1);
        let l2 = Iterator.toList(it);
        l1 == l2
    }


    /////////////
    // Logging //
    /////////////

    trait Logger[a] {
        type Aef: Eff
        pub def log(s: String, x: a): Unit \ Logger.Aef[a]
    }

    enum ConsoleLogger {
        case ConsoleLogger
    }

    instance Logger[ConsoleLogger] {
        type Aef = Console
        pub def log(s: String, _x: ConsoleLogger): Unit \ Console = {
            Console.println(s)
        }
    }

    pub def testConsoleLogger(): Unit \ Console = {
        let logger = ConsoleLogger.ConsoleLogger;
        Logger.log("Hello, world!", logger)
    }

    enum PrintLogger {
        case PrintLogger
    }

    instance Logger[PrintLogger] {
        type Aef = IO
        pub def log(s: String, _x: PrintLogger): Unit \ IO = {
            println(s)
        }
    }

    pub def testPrintLogger(): Unit \ IO = {
        let logger = PrintLogger.PrintLogger;
        Logger.log("Hello, world!", logger)
    }


    enum TimestampedConsoleLogger {
        case TimestampedConsoleLogger
    }

    instance Logger[TimestampedConsoleLogger] {
        type Aef = { Console, Time }
        pub def log(s: String, _x: TimestampedConsoleLogger): Unit \ { Console, Time } = {
            let timestamp = Time.getTime();
            Console.println("${timestamp}: ${s}")
        }
    }

    pub def testTimestampedConsoleLogger(): Unit \ { Console, Time } = {
        let logger = TimestampedConsoleLogger.TimestampedConsoleLogger;
        Logger.log("Hello, world!", logger)
    }
}
