package dev.flix.runtime.example;

import dev.flix.runtime.Handler;
import dev.flix.runtime.Result;
import dev.flix.runtime.Resumption;
import dev.flix.runtime.Unit;

public class ConsoleHandler17 implements Console, Handler {

    public Result read(Unit opArg, Resumption k) {
        return Resumption.rewind(k, 42);    // resume(42)
    }

    public Result print(String opArg, Resumption k) {
        System.out.println("STDOUT: " + opArg);
        return Resumption.rewind(k, 1337);  // resume(1337)
    }

}
