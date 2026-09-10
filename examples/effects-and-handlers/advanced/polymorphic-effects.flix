///
/// An effect can be polymorphic in a type. Here `State[s]` describes a
/// mutable cell holding a value of type `s`.
///
eff State[s] {
    def get(): s
    def put(x: s): Unit
}

///
/// Runs `f` with the initial state `init` and returns its result together
/// with the final state. The handler works for every state type `s`.
///
def runState(init: s, f: Unit -> a \ State[s] + ef): (a, s) \ ef = region rc {
    let cell = Ref.fresh(rc, init);
    let result = run f() with handler State {
        def get(k) = k(Ref.get(cell))
        def put(x, k) = {
            Ref.put(x, cell);
            k(())
        }
    };
    (result, Ref.get(cell))
}

/// Increments an `Int32` state `n` times.
def tick(n: Int32): Unit \ State[Int32] =
    if (n <= 0) () else {
        State.put(State.get() + 1);
        tick(n - 1)
    }

/// Appends `word` to a `String` state.
def say(word: String): Unit \ State[String] =
    State.put(State.get() + word)

def countTicks(): Int32 =
    let (_, count) = runState(0, () -> tick(3));
    count

def greeting(): String =
    let (_, text) = runState("", () -> {
        say("Hello");
        say(", ");
        say("World!")
    });
    text

def main(): Unit \ IO = {
    println("count = ${countTicks()}");
    println("text = ${greeting()}")
}
