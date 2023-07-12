package dev.flix.runtime;

public interface Thunk extends Action {
    Action apply();
}
