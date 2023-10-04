package dev.flix.runtime;

public class ResumptionNil implements Resumption {

    @Override
    public Result rewind(Value v) { // TODO: instance method.
        return v;
    }
}
