eff State {
    def get(): Int32
    def put(v: Int32): Unit
}

pub def main(): Unit \ IO = region rc {
    let n = Ref.fresh(rc, 0);
    run {
        let number = 573;
        // Counting how many steps 573 takes to go to 1 in collatz conjecture
        State.put(0);
        def loop(k: Int32) = {
            if (k == 1)
                k
            else if (Int32.modulo(k, 2) == 0) {
                State.put(State.get() + 1);
                loop(k / 2)
            } else {
                State.put(State.get() + 1);
                loop(3 * k + 1)
            }
        };
        loop(number);
        let steps = State.get();
        println("${number} took ${steps} steps to go to 1")
    } with handler State {
        def get(k) = k(Ref.get(n))
        def put(v, k) = {
            Ref.put(v, n);
            k()
        }
    }
}
