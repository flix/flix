package dev.flix.runtime;

/**
 * A {@link Value} holds a single primitive or non-primitive value.
 * <p>
 * For example, if a `Value` holds an `Int32` then the `int32` field is set
 * and all other fields are meaningless and hold a default JVM specific value.
 */
public class Value implements Result {
    // Note: Only one of these fields will hold a meaningful value.
    //       Based on the conceptual type T of Value[T].
    public boolean b = false;
    public char c = 0;
    public byte i8 = 0;
    public short i16 = 0;
    public int i32 = 0;
    public long i64 = 0;
    public float f32 = 0;
    public double f64 = 0;
    public Object o = null;

    public Value(boolean b) {
        this.b = b;
    }

    public Value(char c) {
        this.c = c;
    }

    public Value(byte i8) {
        this.i8 = i8;
    }

    public Value(short i16) {
        this.i16 = i16;
    }

    public Value(int i32) {
        this.i32 = i32;
    }

    public Value(long i64) {
        this.i64 = i64;
    }

    public Value(float f32) {
        this.f32 = f32;
    }

    public Value(double f64) {
        this.f64 = f64;
    }

    public Value(Object o) {
        this.o = o;
    }
}
