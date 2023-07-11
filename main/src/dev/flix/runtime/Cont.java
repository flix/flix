package dev.flix.runtime;

public interface Cont extends Action {
    Action apply();
}
