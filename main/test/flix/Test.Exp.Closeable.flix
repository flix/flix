mod Test.Exp.Closeable {
    use MyCloseable.MyCloseable
    use MyCloseable2.MyCloseable2

    enum MyCloseable[r: Region](Ref[Bool, r])
    enum MyCloseable2[r: Region](Ref[Bool, r], Ref[Bool, r])

    instance Closeable[MyCloseable[r]] {
        pub def close(x: MyCloseable[r]): Unit \ IO =
            let MyCloseable(closed) = x;
            checked_ecast(closed := true)
    }

    instance Closeable[MyCloseable2[r]] {
        pub def close(x: MyCloseable2[r]): Unit \ IO =
            let MyCloseable2(closed1, closed2) = x;
            checked_ecast(closed2 := deref closed1)
    }

    @test
    def testCloseOnExit01(): Bool \ IO = region rc1 {
        let closed1 = ref false @ rc1;
        let closeable1 = MyCloseable(closed1);
        region rc2 {
            Region.closeOnExit(closeable1, rc2)
        };
        deref closed1
    }

    @test
    def testCloseOnExit02(): Bool \ IO = region rc1 {
        let closed1 = ref false @ rc1;
        let closeable1 = MyCloseable(closed1);
        let closed2 = ref false @ rc1;
        let closeable2 = MyCloseable2(closed1, closed2);
        region rc2 {
            Region.closeOnExit(closeable2, rc2);
            Region.closeOnExit(closeable1, rc2)
        };
        deref closed1 and deref closed2
    }
}
