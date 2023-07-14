package dev.flix.runtime;

public interface Frame {
    Result apply(Value arg);
}
