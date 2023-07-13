package dev.flix.runtime;

import dev.flix.runtime.example.EffectCall;

public interface Console {
    Result read(Resumption k);

    Result print(String s, Resumption k);

}

class ConsoleHandler17 implements Console, Handler {

    public Result read(Resumption k) {
        return Resumption.rewind(k, 42);
    }

    public Result print(String s, Resumption k) {
        System.out.println("STDOUT: " + s);

        return Resumption.rewind(k, 1337);
    }

}

// and then we can create objects/instances for each execution of the try-with.
// Each ResumptionCons contains a handler instance (typed as Object).
