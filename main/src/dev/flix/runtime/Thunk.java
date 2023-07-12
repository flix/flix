package dev.flix.runtime;

public interface Thunk extends Result {
    Result apply();
}
