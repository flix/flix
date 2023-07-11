package dev.flix.runtime;

public interface TailCall extends Cont {
    Cont apply();
}
