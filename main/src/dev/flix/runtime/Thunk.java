package dev.flix.runtime;

// Representations a continuation, a function from Unit to another Result.
public interface Thunk extends Result {
    Result apply();
}

// TODO:
// Calling conventions how to pass arguments
// Marker interface: EffectSym/Class (no strngs)
// Recursion to while.
// Closures
