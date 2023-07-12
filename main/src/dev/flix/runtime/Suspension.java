package dev.flix.runtime;


record Suspension(String effSym, String effOp, Object effArg, Frames prefix, Resumption resumption) implements Result {}
