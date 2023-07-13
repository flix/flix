package dev.flix.runtime.example;

import dev.flix.runtime.Console;
import dev.flix.runtime.Result;
import dev.flix.runtime.Resumption;

public interface UseOfConsole {
   Result apply(Console c, Resumption k);
}
