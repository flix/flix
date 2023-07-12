package dev.flix.runtime;

// Representations a continuation, a function from Unit to another Result.
public interface Thunk extends Result {
    Result apply();
}
