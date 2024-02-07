mod Test.Dec.AssocEff {

    ////////////////
    // Arithmetic //
    ////////////////

    class Add[a] {
        pub def add(x: a, y: a): a
    }

    class Sub[a] {
        pub def sub(x: a, y: a): a
    }

    class Mul[a] {
        pub def mul(x: a, y: a): a
    }

    class Div[a] {
        type Aef: Eff
        pub def div(x: a, y: a): a \ Div.Aef[a]
    }

    class FromInt32[a] {
        pub def fromInt32(x: Int32): a
    }

    instance Add[Int32] {
        pub def add(x: Int32, y: Int32): Int32 = $INT32_ADD$(x, y)
    }

    instance Div[Int32] {
        type Aef = Pure
        pub def div(x: Int32, y: Int32): Int32 = $INT32_DIV$(x, y)
    }

    instance FromInt32[Int32] {
        pub def fromInt32(x: Int32): Int32 = x
    }

    def avg(l: List[t]): t \ Div.Aef[t] with Add[t], Div[t], FromInt32[t] =
        List.foldLeft(Add.add, FromInt32.fromInt32(0), l) `Div.div` FromInt32.fromInt32(List.length(l))

    @Test
    pub def testAvg(): Bool = avg(List#{1, 2, 3}) == 2

    // MATT
    pub def divAll(x: a, y: a, z: a): a with Div[a] = {
        x `Div.div` y `Div.div` z
    }

    //////////////
    // Iterator //
    //////////////

    class Iterable[t: Type] {
        type Elm: Type
        type Aef: Eff
        pub def iterator(rc: Region[r], t: t): Iterator[Iterable.Elm[t], r + Iterable.Aef[t], r] \ r + Iterable.Aef[t]
    }

    instance Iterable[List[a]] {
        type Elm = a
        type Aef = Pure
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
        type Aef = Pure
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
        pub def println(s: String): Unit
    }

    pub def serveDessert(): Unit \ {Console} = region rc {
        let fruits = List#{"Apple", "Peach", "Mango"};
        let creams = MutList.new(rc);
        MutList.push!("Caramel", creams);
        MutList.push!("Vanilla", creams);
        foreach(fruit <- fruits; cream <- creams)
            do Console.println("${fruit} topped with ${cream}")
    }

    class DeepIterable[a] {
        type Elm
        type Aef: Eff
        pub def iterator(rc: Region[r], x: a): Iterator[DeepIterable.Elm[a], r + DeepIterable.Aef[a], r] \ r + DeepIterable.Aef[a]
    }

    instance DeepIterable[Int32] {
        type Elm = Int32
        type Aef = Pure
        pub def iterator(rc: Region[r], x: Int32): Iterator[Int32, r, r] \ r = Iterator.singleton(rc, x)
    }

    enum LoudInt32(Int32) with Eq

    instance DeepIterable[LoudInt32] {
        type Elm = Int32
        type Aef = Console
        pub def iterator(rc: Region[r], x: LoudInt32): Iterator[Int32, r + Console, r] \ r + Console = {
            let LoudInt32.LoudInt32(i) = x;
            do Console.println("${i}!");
            Iterator.singleton(rc, i) |> Iterator.map(y -> {do Console.println("${y}!"); y})
        }
    }

    instance DeepIterable[List[a]] with DeepIterable[a] {
        type Elm = DeepIterable.Elm[a]
        type Aef = DeepIterable.Aef[a]
        pub def iterator(rc: Region[r], x: List[a]): Iterator[DeepIterable.Elm[a], r + DeepIterable.Aef[a], r] \ r + DeepIterable.Aef[a] = {
            // this is needed because the nested Aef isn't actually executed when we flatMap
            let _ = checked_ecast(123): _ \ DeepIterable.Aef[a];
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
}
