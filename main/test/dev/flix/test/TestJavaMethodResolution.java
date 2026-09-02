package dev.flix.test;

import java.util.function.Function;
import java.util.function.UnaryOperator;

public final class TestJavaMethodResolution {

  public static class StaticBase {
    public static Object inherited(Number value) {
      return value;
    }

    public static Object hidden(Object value) {
      return value;
    }

    public static Object overloaded(Object value) {
      return value;
    }
  }

  public static final class StaticChild extends StaticBase {
    public static Object hidden(Object value) {
      return value;
    }

    public static String overloaded(String value) {
      return value;
    }

    public static Object exactStatic(StringBuilder value) {
      return value;
    }

    public Object exactInstance(StringBuffer value) {
      return value;
    }

    public Object exactStatic(Object value) {
      return value;
    }

    public static Object exactInstance(Object value) {
      return value;
    }
  }

  public interface StaticParentInterface {
    static Object notInherited(Object value) {
      return value;
    }
  }

  public interface StaticChildInterface extends StaticParentInterface {
  }

  public static final class Bridge implements UnaryOperator<String> {
    @Override
    public String apply(String value) {
      return value;
    }
  }

  public static final class Accessible extends PackagePrivateFunction {
  }
}

class PackagePrivateFunction implements Function<Object, Object> {
  @Override
  public Object apply(Object value) {
    return value;
  }
}
