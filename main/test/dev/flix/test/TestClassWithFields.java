package dev.flix.test;

public class TestClassWithFields {

    public interface ParentInterface {
        int interfaceField = 1;
    }

    public static class Parent {
        public int inheritedField = 2;
        public static int hiddenField = 3;
    }

    public static class Child extends Parent implements ParentInterface {
        public int declaredField = 4;
        public int hiddenField = 5;
        private int privateField = 6;
    }

}
