package dev.flix.runtime;

/**
 * A {@link Frame} is represents a function `Value -> Result`.
 * <p>
 * Thus a {@link Frame} is like a {@link Thunk}, but it takes an argument.
 * <p>
 * We use a {@link Frame} to model the resumption of a function with a resume argument.
 * That is, to implement `resume(v)` for some value `v` where the resumption happens inside some function `f`.
 * Then the frame represents the ability to call into `f` at the right place with the value `resumeArg`.
 */
public abstract class Frame {
    public abstract Result apply(Value resumeArg);

    public static Result staticApply(Frame f, Value resumeArg) {
        return f.apply(resumeArg);
    }
}
