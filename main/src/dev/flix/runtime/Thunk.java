package dev.flix.runtime;

/**
 * A {@link Thunk} represents a function `Unit -> Result`.
 * <p>
 * A thunk is typically used to be represent a tail call that has to be trampolined.
 * <p>
 * Importantly, we can repeatedly evaluate a thunk to obtain either a {@link Value} or a {@link Suspension}.
 */
public abstract class Thunk implements Result, Runnable {
    // cannot be called apply because of java functional interfaces collision
    public abstract Result invoke();

    @Override
    public void run() {
        // this method is used for spawn, so assumed pure, i.e. no effect
        Result result = this.invoke();
        while (result instanceof Thunk) {
            result = ((Thunk) result).invoke();
        }

        if (result instanceof Suspension) {
            throw new RuntimeException("function was assumed control-pure for java interop but suspension was returned");
        }
    }
}

// TODO: Calling conventions how to pass arguments
// TODO: Marker interface: EffectSym/Class (no strngs)
// TODO: Recursion to while. where
// TODO: Closures
