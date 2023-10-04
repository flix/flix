package dev.flix.runtime;

public class Unit {

    public static final Unit instance = new Unit();

    @Override
    public String toString() {
        return "()";
    }
}
