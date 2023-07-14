package dev.flix.runtime;

/**
 * A {@link Value} holds a single primitive or non-primitive value.
 * <p>
 * For example, if a `Value` holds an `Int32` then the `int32` field is set
 * and all other fields are meaningless and hold a default JVM specific value.
 */
public class Value implements Result {
    // Note: Only one of these fields will hold a meaningful value.
    public final byte int8;
    public final short int16;
    public final int int32;
    // TODO: Add other primitives.
    public final Object obj;

    // Private constructor. Use of the smart constructors to create a value.
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
