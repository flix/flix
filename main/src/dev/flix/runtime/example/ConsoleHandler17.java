package dev.flix.runtime.example;

import dev.flix.runtime.Handler;
import dev.flix.runtime.Result;
import dev.flix.runtime.Resumption;
import dev.flix.runtime.Unit;
import dev.flix.runtime.Value;

public class ConsoleHandler17 implements Console, Handler {

    public Result read(Unit opArg, Resumption k) {
        return Resumption.rewind(k, new Value("John"));    // resume("John")
    }

    public Result print(String opArg, Resumption k) {
        System.out.println("STDOUT: " + opArg);
        return Resumption.rewind(k, new Value(Unit.instance));  // resume()
    }

}
