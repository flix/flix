package dev.flix.runtime;

interface Resumption {}
class ResumptionNil implements Resumption {}

record ResumptionCons(String effSym, Frames frames, Resumption tail) implements Resumption {} // TOOD: Expand

