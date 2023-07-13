package dev.flix.runtime.example;

import dev.flix.runtime.Handler;
import dev.flix.runtime.Result;
import dev.flix.runtime.Resumption;

public interface EffectCall {
   Result apply(Handler h, Resumption k);
}
