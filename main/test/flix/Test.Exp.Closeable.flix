namespace Test/Exp/Closable {

    enum MyCloseable[r: Region](Ref[Bool, r])

    instance Closeable[MyCloseable[r]] {
        pub def close(x: MyCloseable[r]): Unit \ IO =
            let MyCloseable(closed) = x;
            unsafe_cast closed := true as _ \ IO
    }

    @Test
    def testCloseOnExit01(): Bool \ IO = region r1 {
        let closed1 = ref false @ r1;
        let closeable1 = MyCloseable(closed1);
        region r2 {
            Region.closeOnExit(closeable1, r2)
        };
        deref closed1
    }

    @Test
    def testCloseOnExit02(): Bool \ IO = region r1 {
        let closed1 = ref false @ r1;
        let closeable1 = MyCloseable(closed1);
        let closed2 = ref false @ r1;
        let closeable2 = MyCloseable(closed2);
        region r2 {
            Region.closeOnExit(closeable1, r2);
            Region.closeOnExit(closeable2, r2)
        };
        deref closed1 and deref closed2
    }
}
