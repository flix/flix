///
/// An effect can be polymorphic in a type. Here `State[s]` describes a
/// mutable cell holding a value of type `s`.
///
eff State[s] {
    def get(): s
    def put(x: s): Unit
}

/// Increments an `Int32` state `n` times.
def tick(n: Int32): Unit \ State[Int32] =
    if (n <= 0) () else {
        State.put(State.get() + 1);
        tick(n - 1)
    }

def main(): Unit \ IO = region rc {
    let cell = Ref.fresh(rc, 0);
    run tick(3) with handler State {
        def get(k) = k(Ref.get(cell))
        def put(x, k) = {
            Ref.put(x, cell);
            k(())
        }
    };
    println("count = ${Ref.get(cell)}")
}
