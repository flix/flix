package dev.flix.runtime;

public class Value implements Result {
    public final byte int8;
    public final short int16;
    public final int int32;
    // TODO: Add other primitives.
    public final Object obj;

    private Value(byte int8, short int16, int int32, Object obj) {
        this.int8 = int8;
        this.int16 = int16;
        this.int32 = int32;
        this.obj = obj;
    }

    public static Value mkInt32(int i) {
        return new Value((byte) 0, (short) 0, i, null);
    }

    public static Value mkObj(Object o) {
        return new Value((byte) 0, (short) 0, 0, o);
    }

    @Override
    public String toString() {
        return "Value{" + "int8=" + int8 + ", int16=" + int16 + ", int32=" + int32 + ", obj=" + obj + '}';
    }
}
