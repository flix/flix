package dev.flix.runtime;

public interface EffectCall {
   Result apply(Handler h, Resumption k);
}
