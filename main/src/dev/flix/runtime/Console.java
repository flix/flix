package dev.flix.runtime;

public interface Console {
    Result read(Resumption k);

    Result print(String s, Resumption k);

}

class ConsoleHandler17 implements Console {
    public Result read(Resumption k) {
        return Done.mkInt32(21);
    }

    public Result print(String s, Resumption k) {
        return Done.mkInt32(42);
    }

}

// and then we can create objects/instances for each execution of the try-with.
// Each ResumptionCons contains a handler instance (typed as Object).
