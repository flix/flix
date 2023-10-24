package dev.flix.runtime;

public class ResumptionNil implements Resumption {

    @Override
    public Result rewind(Value v) {
        return v;
    }
}
