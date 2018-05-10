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

@test
def trycatch13(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            let x = true;
                x
    }

@test
def trycatch14(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            let x = true;
            let y = false;
            let z = true;
                x && z
    }

@test
def trycatch15(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            let x = 0.0;
            let y = 123;
            let z = 0.5;
                true
    }

//@test // TODO: Currently broken.
//def trycatch16(): Bool = unsafe
//    try (0 / 0) == 0 catch {
//        case ex: ##java.lang.ArithmeticException =>
//            let msg = native method java.lang.Throwable.getMessage(ex);
//                true
//    }


//@test // TODO: Currently broken.
def trytrycatchcatch01(): Bool = unsafe
    try {
        try (0 / 0) == 0 catch {
            case ex: ##java.lang.ArithmeticException => true
        }
    } catch {
         case ex: ##java.lang.ArithmeticException => false
     }

@test
def trycatchtrycatch01(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            try (0 / 0) == 0 catch {
                case ex: ##java.lang.ArithmeticException => true
            }
    }

@test
def trycatchtrycatch02(): Bool = unsafe
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            try (0 / 0) == 0 catch {
                case ex: ##java.lang.ArithmeticException =>
                    try (0 / 0) == 0 catch {
                        case ex: ##java.lang.ArithmeticException => true
                    }
            }
    }
