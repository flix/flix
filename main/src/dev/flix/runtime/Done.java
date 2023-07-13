package dev.flix.runtime;

public class Done implements Result {
    public final byte int8;
    public final short int16;
    public final int int32;
    public final Object result;
    // TODO: Add rest

    private Done(byte int8, short int16, int int32, Object result) {
        this.int8 = int8;
        this.int16 = int16;
        this.int32 = int32;
        this.result = result;
    }
    public static Done mkInt32(int i) {
        return new Done((byte) 0, (short) 0, i, null);
    }
    public static Done mkObj(Object o) {
        return new Done((byte) 0, (short) 0, 0, o);
    }

    @Override
    public String toString() {
        return "Done{" +
                "int8=" + int8 +
                ", int16=" + int16 +
                ", int32=" + int32 +
                ", result=" + result +
                '}';
    }
}
