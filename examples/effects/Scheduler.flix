mod SchedulerExample {

    eff Scheduler {
        pub def fork(f: Unit -> Unit \ IO + Scheduler): Unit
        pub def yield(): Unit
    }

    pub def main(): Unit \ IO = region rc {
        let threads = MutDeque.new(rc);
        try {
            def printNumbers(id): Unit \ IO + Scheduler = {
                List.range(0, 10) |> List.forEach(i -> {
                    println("${id}: ${i}");
                    do Scheduler.yield()
                })
            };
            List.range(0, 5) |> List.forEach(i -> do Scheduler.fork(() -> printNumbers(i)))
        } with Scheduler {
            def fork(f, k) = {
                MutDeque.pushBack(f, threads);
                k()
            }
            def yield(k) = {
                MutDeque.pushBack(k, threads)
            }
        };
        def runThreads() = {
            match MutDeque.popFront(threads) {
                case Some(f) => f(); runThreads()
                case None => ()
            }
        };
        runThreads();
        println("done")
    }

}
