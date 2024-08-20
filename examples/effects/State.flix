mod StateExample {

    eff State {
        pub def get(): Int32
        pub def put(value: Int32): Unit
    }

    pub def main(): Unit \ IO = region rc {
        let n = Ref.fresh(rc, 0);
        try {
            let number = 573;
            // Counting how many steps 573 takes to go to 1 in collatz conjecture
            do State.put(0);
            def loop(k: Int32) = {
                if (k == 1)
                    k
                else if (Int32.modulo(k, 2) == 0) {
                    do State.put(do State.get() + 1);
                    loop(k/2)
                } else {
                    do State.put(do State.get() + 1);
                    loop(3*k + 1)
                }
            };
            loop(number);
            let steps = do State.get();
            println("${number} took ${steps} steps to go to 1")
        } with State {
            def get(k) = k(deref n)
            def put(value, k) = {
                Ref.put(value, n);
                k()
            }
        }
    }
}
