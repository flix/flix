@test
def trycatch01(): Bool = unsafe
    try true catch {
        case ex: ##java.lang.Throwable => false
    }

@test
def trycatch02(): Bool = unsafe
    try true catch {
        case ex: ##java.lang.Throwable => false
        case ex: ##java.lang.Exception => false
    }


@test
def trycatch03(): Bool = unsafe
    try true catch {
        case ex: ##java.lang.Throwable        => false
        case ex: ##java.lang.Exception        => false
        case ex: ##java.lang.RuntimeException => false
    }

@test
def trycatch04(): Bool = unsafe
    try true catch {
        case ex: ##java.lang.Throwable           => false
        case ex: ##java.lang.Exception           => false
        case ex: ##java.lang.RuntimeException    => false
        case ex: ##java.lang.ArithmeticException => false
    }

@test
def trycatch05(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException => true
    }

@test
def trycatch06(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.RuntimeException => true
    }

@test
def trycatch07(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.Exception => true
    }

@test
def trycatch08(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.Throwable => true
    }

@test
def trycatch09(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.io.IOException           => false
        case ex: ##java.lang.ArithmeticException => true
    }

@test
def trycatch10(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.io.IOException            => false
        case ex: ##java.lang.NullPointerException => false
        case ex: ##java.lang.ArithmeticException  => true
    }

@test
def trycatch11(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.io.IOException            => false
        case ex: ##java.lang.NullPointerException => false
        case ex: ##java.lang.InterruptedException => false
        case ex: ##java.lang.ArithmeticException  => true
    }

@test
def trycatch12(): Bool = unsafe
    try {
        native method java.lang.String.charAt("", 0)
    } catch {
        case ex: ##java.lang.StringIndexOutOfBoundsException => true
    }

// TODO: Add tests where
// (1) we use the exception value returned.
// (2) We have nested lets inside the case.
// (3) We have nested try/catch.