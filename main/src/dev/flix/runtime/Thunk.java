package dev.flix.runtime;

/**
 * A {@link Thunk} represents a function `Unit -> Result`.
 * <p>
 * A thunk is typically used to be represent a tail call that has to be trampolined.
 * <p>
 * Importantly, we can repeatedly evaluate a thunk to obtain either a {@link Value} or a {@link Suspension}.
 */
public interface Thunk extends Result {
    Result apply();
}

// TODO: Calling conventions how to pass arguments
// TODO: Marker interface: EffectSym/Class (no strngs)
// TODO: Recursion to while. where
// TODO: Closures
